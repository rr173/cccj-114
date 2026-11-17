# =============================================================================
# test-coverage.R —— 覆盖核验：约定解析与拒绝、逐地区逐月判定（缺失/撤回/
#                    登记缺失/可入面板/豁免/豁免期来件/约定缺失）、未约定地区
#                    现存月报、跨月裁切、无副作用、并发一致性、发布原子性、
#                    CLI 退出码、人工约定对实际账本逐项核对
# =============================================================================

# 账本全部用户表的内容快照（含 sqlite_sequence 的自增计数），用于断言
# “核验前后账本各表与回执/导出发布数量不变”。
ledger_table_dump <- function(con) {
  tabs <- sort(setdiff(DBI::dbListTables(con), "sqlite_sequence"))
  res <- lapply(tabs, function(t)
    DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", t)))
  names(res) <- tabs
  res$sqlite_sequence <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM sqlite_sequence"),
    error = function(e) NULL)
  res
}

coverage_file <- function(dir, name, rows) {
  p <- file.path(dir, paste0(name, "_coverage.csv"))
  fwrite(as.data.table(rows), p)
  p
}

crows <- function(region_id, start_month, end_month, status = "expected",
                  reason = "") {
  data.table(region_id = region_id, start_month = start_month,
             end_month = end_month, status = status, reason = reason)
}

item_at <- function(report, rid, mo) {
  it <- as.data.table(report$items)
  it[region_id == rid & month == mo]
}

test_that("空账本（未看到任何输入行）也能按约定发现缺失月报", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  cov <- coverage_file(d, "c", rbind(
    crows("R1", "2024-01", "2024-03"),
    crows("R2", "2024-01", "2024-01")))
  # 账本里只有 R2 其他月份的记录：对 R2/2024-01 的判定不依赖“见过该地区”
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R2", "2024-09", 1L)),
         registration = NULL), list(batch_id = "B1"))
  rep <- verify_coverage(L$con, cov, "2024-01", "2024-01")
  expect_equal(rep$verdict, "findings")
  expect_equal(nrow(rep$items), 2L)   # 范围只有 2024-01：R1 裁切到 1 个月 + R2
  r1 <- item_at(rep, "R1", "2024-01")
  expect_equal(r1$status, "missing_report")
  expect_equal(r1$report_state, "never_received")
  expect_true(is.na(r1$monthly_revision))
  expect_true(is.na(r1$monthly_batch_id))
  expect_equal(r1$registration_state, "none")
  expect_equal(r1$source_row, 2L)     # 约定来源行（表头为第 1 行）
  expect_true(r1$actionable)
  r2 <- item_at(rep, "R2", "2024-01")
  expect_equal(r2$status, "missing_report")
  expect_equal(r2$source_row, 3L)
  # R2 的 2024-09 月报在范围外：不进入 unscheduled
  expect_equal(nrow(rep$unscheduled_reports), 0L)
  expect_equal(rep$summary$n_missing_report, 2L)
  expect_equal(rep$summary$n_actionable, 2L)
})

test_that("豁免保留原因；豁免期来件单列，不静默计作按期交付", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  cov <- coverage_file(d, "c", rbind(
    crows("R1", "2024-01", "2024-02", "exempt", "机房迁移"),
    crows("R2", "2024-01", "2024-02", "exempt", "尚未开播")))
  # 全部豁免且无来件：clean
  rep0 <- verify_coverage(L$con, cov, "2024-01", "2024-02")
  expect_equal(rep0$verdict, "clean")
  expect_equal(rep0$summary$n_actionable, 0L)
  expect_equal(rep0$summary$n_exempt, 4L)
  r1 <- item_at(rep0, "R1", "2024-01")
  expect_equal(r1$status, "exempt")
  expect_equal(r1$exemption_reason, "机房迁移")
  expect_equal(r1$report_state, "never_received")
  expect_false(r1$actionable)

  # R1/2024-02 收到月报（还带登记）：豁免期有来件，不能计作按期交付
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R1", "2024-02", 1L)),
         registration = registration_file(d, "r", rrows("R1"))),
    list(batch_id = "B1"))
  # R2/2024-01 的月报曾到达但已撤回：不算来件，但仍可见撤回状态
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m2", mrows("R2", "2024-01", 1L)),
         registration = NULL), list(batch_id = "B2"))
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m3",
           mrows("R2", "2024-01", 2L, op = "delete",
                 y = NA, x1 = NA, x2 = NA)),
         registration = NULL), list(batch_id = "B3"))
  rep <- verify_coverage(L$con, cov, "2024-01", "2024-02")
  expect_equal(rep$verdict, "findings")
  expect_equal(rep$summary$n_exempt_with_delivery, 1L)
  expect_equal(rep$summary$n_ready_for_panel, 0L)   # 来件没有变成“按期交付”
  expect_equal(rep$summary$n_actionable, 1L)
  d1 <- item_at(rep, "R1", "2024-02")
  expect_equal(d1$status, "exempt_with_delivery")
  expect_equal(d1$exemption_reason, "机房迁移")
  expect_equal(d1$monthly_revision, 1L)
  expect_equal(d1$monthly_batch_id, "B1")
  expect_true(d1$actionable)
  d2 <- item_at(rep, "R2", "2024-01")
  expect_equal(d2$status, "exempt")                 # 已撤回不算现存来件
  expect_equal(d2$report_state, "withdrawn")
  expect_equal(d2$monthly_revision, 2L)             # 撤回版本仍可追溯
  expect_equal(d2$monthly_batch_id, "B3")
  expect_false(d2$actionable)
  # 豁免月的现存来件同时出现在 unscheduled_reports（covered_by = exempt）
  un <- as.data.table(rep$unscheduled_reports)
  expect_equal(nrow(un), 1L)
  expect_equal(un$region_id, "R1")
  expect_equal(un$month, "2024-02")
  expect_equal(un$covered_by, "exempt")
})

test_that("月报撤回与登记缺失的不同归因（含登记撤回）", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", rbind(
           mrows("R1", "2024-01", 1L),
           mrows("R2", "2024-01", 1L),
           mrows("R3", "2024-01", 1L),
           mrows("R4", "2024-01", 1L))),
         registration = registration_file(d, "r", rbind(
           rrows("R3", 1L, "upsert", "2024-01"),
           rrows("R4", 1L, "upsert", "never")))),
    list(batch_id = "B1"))
  # R1 撤回月报（r2 delete）；R3 撤回登记（r2 delete）
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m2",
           mrows("R1", "2024-01", 2L, op = "delete",
                 y = NA, x1 = NA, x2 = NA)),
         registration = registration_file(d, "r2",
           rrows("R3", 2L, op = "delete", first_treat_month = NA))),
    list(batch_id = "B2"))
  cov <- coverage_file(d, "c",
    crows(c("R1", "R2", "R3", "R4"), "2024-01", "2024-01"))
  rep <- verify_coverage(L$con, cov, "2024-01", "2024-01")

  w <- item_at(rep, "R1", "2024-01")
  expect_equal(w$status, "report_withdrawn")        # 撤回 ≠ 登记缺失
  expect_equal(w$report_state, "withdrawn")
  expect_equal(w$monthly_revision, 2L)
  expect_equal(w$monthly_batch_id, "B2")
  expect_true(w$actionable)

  n <- item_at(rep, "R2", "2024-01")
  expect_equal(n$status, "missing_registration")
  expect_equal(n$report_state, "valid")             # 已有有效月报
  expect_equal(n$registration_state, "none")        # 从未登记
  expect_true(is.na(n$registration_revision))
  expect_equal(n$monthly_revision, 1L)
  expect_equal(n$monthly_batch_id, "B1")

  rw <- item_at(rep, "R3", "2024-01")
  expect_equal(rw$status, "missing_registration")
  expect_equal(rw$registration_state, "withdrawn")  # 登记已撤回
  expect_equal(rw$registration_revision, 2L)
  expect_equal(rw$registration_batch_id, "B2")

  ok <- item_at(rep, "R4", "2024-01")
  expect_equal(ok$status, "ready_for_panel")        # 已具备进入面板的条件
  expect_equal(ok$registration_state, "effective")
  expect_equal(ok$first_treat_month, "never")
  expect_equal(ok$registration_revision, 1L)
  expect_equal(ok$registration_batch_id, "B1")
  expect_false(ok$actionable)

  s <- rep$summary
  expect_equal(s$n_report_withdrawn, 1L)
  expect_equal(s$n_missing_registration, 2L)
  expect_equal(s$n_ready_for_panel, 1L)
  expect_equal(s$n_actionable, 3L)
})

test_that("未约定地区/未约定月份的现存月报逐条列出", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", rbind(
           mrows("R1", c("2024-01", "2024-03"), 1L),   # R1 只有 01 约定
           mrows("R2", "2024-01", 1L),                # R2 豁免月来件
           mrows("R9", c("2024-02", "2024-05"), 1L),  # R9 完全不在约定文件
           mrows("R3", "2024-02", 1L))),
         registration = NULL), list(batch_id = "B1"))
  # R3 的 2024-02 随后撤回：不再是“现存月报”，不进 unscheduled
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m2",
           mrows("R3", "2024-02", 2L, op = "delete",
                 y = NA, x1 = NA, x2 = NA)),
         registration = NULL), list(batch_id = "B2"))
  cov <- coverage_file(d, "c", rbind(
    crows("R1", "2024-01", "2024-01"),
    crows("R2", "2024-01", "2024-01", "exempt", "试点期")))
  rep <- verify_coverage(L$con, cov, "2024-01", "2024-03")
  un <- as.data.table(rep$unscheduled_reports)
  expect_equal(un$region_id, c("R1", "R2", "R9"))
  expect_equal(un$month, c("2024-03", "2024-01", "2024-02"))
  expect_equal(un$covered_by, c("none", "exempt", "none"))
  expect_equal(un$monthly_revision, c(1L, 1L, 1L))
  expect_true(all(un$monthly_batch_id == "B1"))
  # R9/2024-05 在范围外、R3/2024-02 已撤回：都不出现
  expect_equal(rep$summary$n_unscheduled_reports, 3L)
  expect_equal(rep$summary$n_unscheduled_no_convention, 2L)
  # R1/2024-03 同时是“约定缺失”明细行
  u <- item_at(rep, "R1", "2024-03")
  expect_equal(u$status, "uncovered")
  expect_equal(u$coverage, "none")
  expect_true(is.na(u$source_row))
  expect_equal(u$report_state, "valid")   # 该月有现存月报，但无约定
})

test_that("约定缺失单独报告，不默认为豁免", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  cov <- coverage_file(d, "c", crows("R1", "2024-01", "2024-02"))
  rep <- verify_coverage(L$con, cov, "2024-01", "2024-04")
  it <- as.data.table(rep$items)
  expect_equal(nrow(it), 4L)
  expect_equal(it$status, c("missing_report", "missing_report",
                            "uncovered", "uncovered"))
  expect_equal(it$coverage, c("expected", "expected", "none", "none"))
  expect_true(all(it$actionable))
  expect_equal(rep$summary$n_uncovered, 2L)
  expect_equal(rep$summary$n_exempt, 0L)   # 未被覆盖的月份没有变成豁免
  expect_equal(rep$verdict, "findings")
})

test_that("跨月边界裁切：只处理与范围相交的约定", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  cov <- coverage_file(d, "c", rbind(
    crows("R1", "2023-11", "2024-03"),   # 裁切到 2024-01..2024-02
    crows("R2", "2025-01", "2025-12"),   # 完全不相交：整区不处理
    crows("R3", "2023-12", "2024-01")))  # 裁切到 2024-01
  rep <- verify_coverage(L$con, cov, "2024-01", "2024-02")
  it <- as.data.table(rep$items)
  # R1 裁切为 2024-01..02；R3 裁切为 2024-01，且 R3/2024-02 成为约定缺失；
  # R2 的约定完全不相交，整区不出现
  expect_equal(nrow(it), 4L)
  expect_equal(it$region_id, c("R1", "R1", "R3", "R3"))
  expect_equal(it$month, c("2024-01", "2024-02", "2024-01", "2024-02"))
  expect_equal(it$status, c("missing_report", "missing_report",
                            "missing_report", "uncovered"))
  expect_equal(it$source_row, c(2L, 2L, 4L, NA_integer_))
  expect_false("R2" %in% it$region_id)
  expect_equal(rep$summary$n_items, 4L)
})

test_that("非法约定整体拒绝并指出问题行，且不覆盖已有报告", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  rp <- file.path(d, "reports", "coverage.json")
  good <- verify_coverage(L$con, coverage_file(d, "good",
    crows("R1", "2024-01", "2024-01")), "2024-01", "2024-01")
  write_coverage_report(good, rp)
  before <- readLines(rp, warn = FALSE)

  expect_reject <- function(rows_or_path, code, pattern = NULL) {
    p <- if (is.data.frame(rows_or_path))
      coverage_file(d, paste0("bad", sample.int(1e6, 1)), rows_or_path)
    else rows_or_path
    err <- tryCatch({
      verify_coverage(L$con, p, "2024-01", "2024-06")
      write_coverage_report(list(should = "never_happen"), rp)
      NULL
    }, ledger_error = function(e) e)
    expect_true(is_ledger_error(err), info = paste("期望拒绝:", code))
    expect_equal(err$code, code)
    if (!is.null(pattern))
      expect_true(grepl(pattern, conditionMessage(err)),
                  info = conditionMessage(err))
    expect_equal(readLines(rp, warn = FALSE), before)   # 旧报告不动
    err
  }

  # 区间重叠（同状态也算重叠）
  e <- expect_reject(rbind(
    crows("R1", "2024-01", "2024-03"),
    crows("R1", "2024-02", "2024-04")), "coverage_validation_error", "重叠")
  rows <- vapply(e$details, `[[`, 0L, "row")
  expect_true(all(c(2L, 3L) %in% rows))
  # 相互矛盾（应报 vs 豁免 重叠）
  expect_reject(rbind(
    crows("R1", "2024-01", "2024-03"),
    crows("R1", "2024-03", "2024-05", "exempt", "原因X")),
    "coverage_validation_error", "重叠")
  # 嵌套重叠
  expect_reject(rbind(
    crows("R1", "2024-01", "2024-12"),
    crows("R1", "2024-05", "2024-06")), "coverage_validation_error", "重叠")
  # 区间倒置
  expect_reject(crows("R1", "2024-05", "2024-01"),
                "coverage_validation_error", "倒置")
  # 月份无效
  expect_reject(crows("R1", "2024-13", "2024-14"),
                "coverage_validation_error", "YYYY-MM")
  expect_reject(crows("R1", "2024-1", "2024-01"),
                "coverage_validation_error", "YYYY-MM")
  # 豁免缺原因
  expect_reject(crows("R1", "2024-01", "2024-02", "exempt", ""),
                "coverage_validation_error", "原因")
  # 应报带原因
  expect_reject(crows("R1", "2024-01", "2024-02", "expected", "不该填"),
                "coverage_validation_error", "原因")
  # 非法 status
  expect_reject(crows("R1", "2024-01", "2024-02", "maybe"),
                "coverage_validation_error", "status")
  # 表头缺列 / 多列
  p1 <- write_csv_lines(file.path(d, "h1.csv"),
    c("region_id,start_month,end_month,status", "R1,2024-01,2024-02,expected"))
  expect_reject(p1, "coverage_format_error", "表头")
  p2 <- write_csv_lines(file.path(d, "h2.csv"),
    c("region_id,start_month,end_month,status,reason,extra",
      "R1,2024-01,2024-02,expected,,x"))
  expect_reject(p2, "coverage_format_error", "表头")
  # 空文件（只有表头）
  p3 <- write_csv_lines(file.path(d, "h3.csv"),
    "region_id,start_month,end_month,status,reason")
  expect_reject(p3, "coverage_format_error", "数据行")
  # 文件不存在
  expect_reject(file.path(d, "no_such.csv"), "coverage_file_not_found")

  # 相邻（不重叠）区间是合法的：2024-01..03 与 2024-04..06
  adj <- verify_coverage(L$con, coverage_file(d, "adj", rbind(
    crows("R1", "2024-01", "2024-03"),
    crows("R1", "2024-04", "2024-06", "exempt", "换季"))),
    "2024-01", "2024-06")
  expect_equal(nrow(adj$items), 6L)
  expect_equal(adj$summary$n_expected, 3L)
  expect_equal(adj$summary$n_exempt, 3L)
})

test_that("核验范围参数校验", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  cov <- coverage_file(d, "c", crows("R1", "2024-01", "2024-02"))
  e1 <- tryCatch(verify_coverage(L$con, cov, "2024-06", "2024-01"),
                 ledger_error = function(e) e)
  expect_equal(e1$code, "invalid_range")
  e2 <- tryCatch(verify_coverage(L$con, cov, "2024-1", "2024-06"),
                 ledger_error = function(e) e)
  expect_equal(e2$code, "invalid_range")
  e3 <- tryCatch(verify_coverage(L$con, cov, NULL, "2024-06"),
                 ledger_error = function(e) e)
  expect_equal(e3$code, "missing_range")
})

test_that("核验前后账本各表、回执与导出发布数量不变", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", rbind(
           mrows("R1", c("2024-01", "2024-02"), 1L),
           mrows("R2", "2024-01", 1L))),
         registration = registration_file(d, "r", rrows("R1"))),
    list(batch_id = "B1"))
  # 先做一次正式导出，让 export_release/export_file 也有数据
  export_panel(L$con, list(out_dir = file.path(d, "exports")))
  before <- ledger_table_dump(L$con)
  snap_before <- ledger_snapshot(L$con)

  cov <- coverage_file(d, "c", rbind(
    crows("R1", "2024-01", "2024-03"),       # 含缺失月报
    crows("R2", "2024-01", "2024-01"),       # 缺登记
    crows("R3", "2024-01", "2024-02", "exempt", "演练")))
  rep <- verify_coverage(L$con, cov, "2024-01", "2024-03")
  expect_equal(rep$verdict, "findings")
  write_coverage_report(rep, file.path(d, "reports", "cov.json"))

  after <- ledger_table_dump(L$con)
  expect_equal(after, before)                 # 全部用户表 + sqlite_sequence
  snap_after <- ledger_snapshot(L$con)
  expect_equal(snap_after$n_receipts, snap_before$n_receipts)
  expect_equal(snap_after$n_releases, snap_before$n_releases)
  expect_equal(snap_after$n_monthly_versions,
               snap_before$n_monthly_versions)
  expect_equal(snap_after$n_registration_versions,
               snap_before$n_registration_versions)
  expect_true(check_ledger_invariants(L$con)$consistent)
})

test_that("并发导入期间核验：报告只对应一个可核对的账本状态", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "base",
           mrows(paste0("B", 1:3), rep("2024-01", 3), 1L)),
         registration = registration_file(d, "baser",
           rrows(paste0("B", 1:3), 1L, "upsert", "never"))),
    list(batch_id = "BASE"))
  b0 <- capture_coverage_boundary(L$con)

  # 约定同时覆盖 B*（已报送）与 Y*（将由并发批次报送）
  cov <- coverage_file(d, "c", rbind(
    crows(paste0("B", 1:3), "2024-01", "2024-01"),
    crows(paste0("Y", 1:4), "2024-01", "2024-01")))
  fy <- monthly_file(d, "iy", mrows(paste0("Y", 1:4), rep("2024-01", 4), 1L))
  fry <- registration_file(d, "iyr", rrows(paste0("Y", 1:4), 1L, "upsert",
                                           "never"))
  marker <- file.path(d, "verify.marker")
  report_path <- file.path(d, "coverage.json")
  done_v <- file.path(d, "done_v"); done_i <- file.path(d, "done_i")
  cmd_v <- cli_cmd("verify_coverage.R",
    c("--db", L$db, "--coverage", cov, "--from", "2024-01", "--to", "2024-01",
      "--report", report_path,
      "--pause-inside-snapshot", "4", "--pause-marker", marker),
    done_v, file.path(d, "v.log"))
  system2("bash", c("-c", shQuote(cmd_v)), wait = FALSE)
  wait_file(marker, timeout = 60)   # 核验已固定只读快照
  cmd_i <- cli_cmd("import_batch.R",
    c("--db", L$db, "--batch-id", "IY",
      "--monthly", fy, "--registration", fry),
    done_i, file.path(d, "i.log"))
  system2("bash", c("-c", shQuote(cmd_i)), wait = FALSE)

  cv <- wait_done(done_v, timeout = 90)
  ci <- wait_done(done_i, timeout = 90)
  expect_equal(ci, 0L)
  expect_equal(cv, 3L)   # Y* 在快照时点尚未报送 -> findings

  rep <- jsonlite::fromJSON(report_path, simplifyVector = FALSE)
  expect_equal(rep$verdict, "findings")
  # 边界 = 核验开始前的状态（IY 未混入）
  expect_equal(rep$state_boundary$max_monthly_record_id,
               b0$max_monthly_record_id)
  expect_equal(rep$state_boundary$max_registration_record_id,
               b0$max_registration_record_id)
  expect_equal(rep$state_boundary$n_monthly_versions, b0$n_monthly_versions)
  expect_true(grepl("只读事务", rep$state_boundary$note))
  # Y* 全部按导入前状态判定为 missing_report，而不是 ready_for_panel
  its <- rep$items
  y_status <- vapply(Filter(function(x) startsWith(x$region_id, "Y"), its),
                     `[[`, "", "status")
  expect_equal(length(y_status), 4L)
  expect_true(all(y_status == "missing_report"))
  b_status <- vapply(Filter(function(x) startsWith(x$region_id, "B"), its),
                     `[[`, "", "status")
  expect_true(all(b_status == "ready_for_panel"))
  # unscheduled 中没有 Y* 的月报（它们在快照时点还不存在）
  expect_equal(length(rep$unscheduled_reports), 0L)

  # IY 已正式提交：账本前进，但报告仍描述核验时的状态
  now <- capture_coverage_boundary(L$con)
  expect_gt(now$max_monthly_record_id, b0$max_monthly_record_id)
  expect_equal(ledger_snapshot(L$con)$n_receipts, 2L)   # BASE + IY
  rep2 <- verify_coverage(L$con, cov, "2024-01", "2024-01")
  expect_equal(rep2$verdict, "clean")                   # 现在全部到齐
  expect_true(check_ledger_invariants(L$con)$consistent)
})

test_that("人工约定对实际账本逐地区逐月核对（含缺口与豁免）", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  root <- LEDGER_TEST_ROOT
  demo <- file.path(root, "data", "ledger_demo")
  if (!file.exists(file.path(demo, "monthly", "d01_monthly.csv")))
    system(paste(shQuote(LEDGER_TEST_R),
                 shQuote(file.path(root, "R", "ledger",
                                   "make_demo_batches.R")), "2>/dev/null"))
  # 实际账本：D01（R001/R002/R003 各 2022-01..2023-12 月报；R001/R002 登记）
  #           D04（R002 2022-03 撤回）
  ingest_batch(L$con,
    list(monthly = file.path(demo, "monthly", "d01_monthly.csv"),
         registration = file.path(demo, "registration",
                                  "d01_registration.csv")),
    list(batch_id = "D01"))
  ingest_batch(L$con,
    list(monthly = file.path(demo, "monthly", "d04_monthly.csv"),
         registration = NULL), list(batch_id = "D04"))

  # 人工约定（已知应报缺口与豁免月份）
  cov <- coverage_file(d, "manual", rbind(
    crows("R001", "2022-01", "2022-03"),
    crows("R001", "2022-05", "2022-06"),
    crows("R001", "2022-04", "2022-04", "exempt", "线路检修"),
    crows("R002", "2022-01", "2022-06"),
    crows("R003", "2022-01", "2022-03"),
    crows("R004", "2022-01", "2022-06"),
    crows("R005", "2022-01", "2022-06", "exempt", "尚未开播")))
  rep <- verify_coverage(L$con, cov, "2022-01", "2022-06")
  it <- as.data.table(rep$items)
  expect_equal(nrow(it), 30L)

  # ---- 逐地区逐月核对 -------------------------------------------------------
  st <- function(rid, mo) item_at(rep, rid, mo)$status
  # R001：应报月全部 ready（登记 never），豁免月有来件
  expect_equal(st("R001", "2022-01"), "ready_for_panel")
  expect_equal(st("R001", "2022-02"), "ready_for_panel")
  expect_equal(st("R001", "2022-03"), "ready_for_panel")
  expect_equal(st("R001", "2022-04"), "exempt_with_delivery")
  expect_equal(st("R001", "2022-05"), "ready_for_panel")
  expect_equal(st("R001", "2022-06"), "ready_for_panel")
  expect_equal(item_at(rep, "R001", "2022-04")$exemption_reason, "线路检修")
  expect_equal(item_at(rep, "R001", "2022-04")$source_row, 4L)
  # R002：2022-03 已撤回，其余 ready
  expect_equal(st("R002", "2022-01"), "ready_for_panel")
  expect_equal(st("R002", "2022-02"), "ready_for_panel")
  expect_equal(st("R002", "2022-03"), "report_withdrawn")
  expect_equal(item_at(rep, "R002", "2022-03")$monthly_revision, 2L)
  expect_equal(item_at(rep, "R002", "2022-03")$monthly_batch_id, "D04")
  expect_equal(st("R002", "2022-04"), "ready_for_panel")
  expect_equal(st("R002", "2022-06"), "ready_for_panel")
  # R003：有月报无登记；2022-04..06 无约定 -> 约定缺失
  expect_equal(st("R003", "2022-01"), "missing_registration")
  expect_equal(st("R003", "2022-03"), "missing_registration")
  expect_equal(item_at(rep, "R003", "2022-01")$registration_state, "none")
  expect_equal(st("R003", "2022-04"), "uncovered")
  expect_equal(st("R003", "2022-06"), "uncovered")
  # R004：从未收到月报（整个范围都是缺口）
  expect_equal(st("R004", "2022-01"), "missing_report")
  expect_equal(st("R004", "2022-02"), "missing_report")
  expect_equal(st("R004", "2022-06"), "missing_report")
  expect_equal(item_at(rep, "R004", "2022-02")$source_row, 7L)
  # R005：豁免且无来件
  expect_equal(st("R005", "2022-01"), "exempt")
  expect_equal(st("R005", "2022-06"), "exempt")
  expect_equal(item_at(rep, "R005", "2022-01")$exemption_reason, "尚未开播")

  # ---- 与账本正式导出逐行交叉核对 -------------------------------------------
  ex <- compute_export_tables(L$con)
  ready <- it[status == "ready_for_panel"]
  for (i in seq_len(nrow(ready))) {
    p <- ex$provenance[region_id == ready$region_id[i] &
                       month == ready$month[i]]
    expect_equal(nrow(p), 1L)
    expect_equal(p$monthly_revision, ready$monthly_revision[i])
    expect_equal(p$monthly_batch_id, ready$monthly_batch_id[i])
    expect_equal(p$registration_revision, ready$registration_revision[i])
    expect_equal(p$registration_batch_id, ready$registration_batch_id[i])
  }
  miss <- it[status == "missing_registration"]
  for (i in seq_len(nrow(miss))) {
    m <- ex$missing[region_id == miss$region_id[i] & month == miss$month[i]]
    expect_equal(nrow(m), 1L)
    expect_equal(m$reason, "no_registration")
    expect_equal(m$monthly_revision, miss$monthly_revision[i])
  }

  # ---- 未约定现存月报 --------------------------------------------------------
  un <- as.data.table(rep$unscheduled_reports)
  expect_equal(nrow(un), 4L)
  expect_equal(un[region_id == "R001" & month == "2022-04", covered_by],
               "exempt")
  expect_equal(nrow(un[region_id == "R003" & covered_by == "none"]), 3L)

  # ---- 汇总与边界 ------------------------------------------------------------
  s <- rep$summary
  expect_equal(s$n_items, 30L)
  expect_equal(s$n_expected, 20L)
  expect_equal(s$n_exempt, 7L)
  expect_equal(s$n_uncovered, 3L)
  expect_equal(s$n_ready_for_panel, 10L)
  expect_equal(s$n_report_withdrawn, 1L)
  expect_equal(s$n_missing_registration, 3L)
  expect_equal(s$n_missing_report, 6L)
  expect_equal(s$n_exempt_with_delivery, 1L)
  expect_equal(s$n_unscheduled_reports, 4L)
  expect_equal(s$n_unscheduled_no_convention, 3L)
  expect_equal(s$n_actionable, 17L)
  expect_equal(rep$verdict, "findings")
  b <- capture_coverage_boundary(L$con)
  expect_equal(rep$state_boundary$max_monthly_record_id,
               b$max_monthly_record_id)
  expect_equal(rep$state_boundary$n_monthly_versions, 73L)  # 72 + 1 撤回行
  expect_equal(rep$state_boundary$n_registration_versions, 2L)

  # ---- JSON 往返：明细逐行保留，NA -> null -----------------------------------
  rp <- file.path(d, "reports", "manual.json")
  write_coverage_report(rep, rp)
  rj <- jsonlite::fromJSON(rp, simplifyVector = FALSE)
  expect_equal(length(rj$items), 30L)
  r004 <- Filter(function(x) x$region_id == "R004" && x$month == "2022-02",
                 rj$items)
  expect_equal(length(r004), 1L)
  # 缺失月报的月报字段以显式 null 保留（键不消失），便于机器核对
  expect_true("monthly_revision" %in% names(r004[[1]]))
  expect_null(r004[[1]]$monthly_revision)
  expect_true("monthly_batch_id" %in% names(r004[[1]]))
  expect_null(r004[[1]]$monthly_batch_id)
  expect_equal(r004[[1]]$source_row, 7L)
  expect_equal(r004[[1]]$status, "missing_report")
  expect_equal(rj$summary$n_actionable, 17L)
})

test_that("报告全部生成后才发布；失败不覆盖已有报告", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  rp <- file.path(d, "reports", "coverage.json")
  rep <- verify_coverage(L$con, coverage_file(d, "ok",
    crows("R1", "2024-01", "2024-01")), "2024-01", "2024-01")
  write_coverage_report(rep, rp)
  expect_true(file.exists(rp))
  first <- readLines(rp, warn = FALSE)
  expect_true(any(grepl('"findings"', first)))

  # 非法约定 -> 报错且已有报告不变
  err <- tryCatch({
    verify_coverage(L$con, coverage_file(d, "bad",
      crows("R1", "2024-02", "2024-01")), "2024-01", "2024-06")
    write_coverage_report(list(should = "never_happen"), rp)
  }, ledger_error = function(e) e)
  expect_equal(err$code, "coverage_validation_error")
  expect_equal(readLines(rp, warn = FALSE), first)

  # 写盘失败（目标在非法路径下）也不留临时文件、不破坏已有报告
  err2 <- tryCatch(
    write_coverage_report(rep, file.path(rp, "nested", "x.json")),
    ledger_error = function(e) e)
  expect_true(is_ledger_error(err2))
  expect_equal(readLines(rp, warn = FALSE), first)
  expect_equal(length(list.files(dirname(rp), pattern = "^\\.")), 0L)

  # 新的成功核验可以正常覆盖发布（原子替换）
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R1", "2024-01", 1L)),
         registration = registration_file(d, "r", rrows("R1"))),
    list(batch_id = "B1"))
  rep2 <- verify_coverage(L$con, coverage_file(d, "ok2",
    crows("R1", "2024-01", "2024-01")), "2024-01", "2024-01")
  expect_equal(rep2$verdict, "clean")
  write_coverage_report(rep2, rp)
  now <- readLines(rp, warn = FALSE)
  expect_true(any(grepl('"clean"', now)))
})

test_that("覆盖核验 CLI：退出码 0/3/2 与 stdout JSON 契约", {
  d <- tempfile("coverage_cli_"); dir.create(d)
  db <- file.path(d, "ledger.sqlite")
  root <- LEDGER_TEST_ROOT
  run <- function(script, args) {
    cmd <- paste(c(shQuote(LEDGER_TEST_R), shQuote(file.path(root, "R",
      "ledger", script)), vapply(args, shQuote, "")), collapse = " ")
    out <- suppressWarnings(system(paste(cmd, "2>/dev/null"), intern = TRUE))
    status <- attr(out, "status")
    list(status = if (is.null(status)) 0L else status,
         json = if (length(out) > 0L) jsonlite::fromJSON(
           out[length(out)]) else NULL)
  }

  r <- run("init_ledger.R", c("--db", db, "--source-id", "SRC-COV"))
  expect_equal(r$status, 0L)

  cov_clean <- file.path(d, "clean.csv")
  writeLines(c("region_id,start_month,end_month,status,reason",
               "R1,2024-01,2024-01,expected,"), cov_clean)
  cov_gap <- file.path(d, "gap.csv")
  writeLines(c("region_id,start_month,end_month,status,reason",
               "R1,2024-01,2024-02,expected,",
               "R2,2024-01,2024-01,exempt,演练"), cov_gap)
  cov_bad <- file.path(d, "bad.csv")
  writeLines(c("region_id,start_month,end_month,status,reason",
               "R1,2024-01,2024-03,expected,",
               "R1,2024-02,2024-04,expected,"), cov_bad)
  rp <- file.path(d, "out", "cov.json")

  # 参数缺失 -> 2
  r <- run("verify_coverage.R", c("--db", db, "--from", "2024-01",
                                  "--to", "2024-02", "--report", rp))
  expect_equal(r$status, 2L)
  expect_equal(r$json$error_code, "missing_coverage_path")
  r <- run("verify_coverage.R", c("--db", db, "--coverage", cov_clean,
                                  "--from", "2024-01", "--to", "2024-02"))
  expect_equal(r$status, 2L)
  expect_equal(r$json$error_code, "missing_report_path")
  r <- run("verify_coverage.R", c("--db", db, "--coverage", cov_clean,
                                  "--to", "2024-02", "--report", rp))
  expect_equal(r$status, 2L)
  expect_equal(r$json$error_code, "missing_range")

  # 有缺口 -> 3，报告完整发布
  r <- run("verify_coverage.R", c("--db", db, "--coverage", cov_gap,
    "--from", "2024-01", "--to", "2024-02", "--report", rp))
  expect_equal(r$status, 3L)
  expect_true(r$json$ok)
  expect_equal(r$json$verdict, "findings")
  expect_equal(r$json$summary$n_missing_report, 2L)
  expect_true(file.exists(rp))
  saved <- readLines(rp, warn = FALSE)

  # 非法约定 -> 2，且不写报告（已有报告不变）
  r <- run("verify_coverage.R", c("--db", db, "--coverage", cov_bad,
    "--from", "2024-01", "--to", "2024-02", "--report", rp))
  expect_equal(r$status, 2L)
  expect_equal(r$json$error_code, "coverage_validation_error")
  expect_equal(readLines(rp, warn = FALSE), saved)

  # 补齐数据后 -> 0
  mf <- file.path(d, "m.csv")
  fwrite(mrows("R1", c("2024-01", "2024-02"), 1L), mf)
  rf <- file.path(d, "r.csv")
  fwrite(rrows("R1"), rf)
  r <- run("import_batch.R", c("--db", db, "--batch-id", "B1",
                               "--monthly", mf, "--registration", rf))
  expect_equal(r$status, 0L)
  r <- run("verify_coverage.R", c("--db", db, "--coverage", cov_clean,
    "--from", "2024-01", "--to", "2024-01", "--report", rp))
  expect_equal(r$status, 0L)
  expect_true(r$json$ok)
  expect_equal(r$json$verdict, "clean")
  expect_equal(r$json$summary$n_ready_for_panel, 1L)
})
