# =============================================================================
# test-rebuild.R —— 按已提交导入批次重建分析面板（schema v2）
#
# 覆盖需求中的每一条：
#  1. 老账本（v1）升级后的基线与升级时当前导出完全相同；
#  2. 升级后一次混合批次（月报+登记）可按同一边界重建两类数据；
#  3. 较高修订后再重建旧边界读不到新值；较低修订晚到、delete 墓碑屏蔽、
#     更高版本恢复都在时间旅行视图里遵循账本修订规则；边界之后的更正不泄漏；
#  4. 并发导入的批次边界有唯一、无空洞、可核对的提交顺序；
#  5. 失败批次与相同内容重放不制造新边界；
#  6. 重建不改变当前导出与账本业务状态；同一边界重复重建面板与来源逐格一致，
#     且结果能被现有分析入口（validate_data.R）读取；
#  7. 未知 / 失败 / 升级基线以前的 batch_id 被明确拒绝，旧结果目录保持不变。
# =============================================================================

# ---- 工具：构造一个已带数据的 v1 老账本（模拟现场升级）----------------------
make_v1_ledger <- function(d, source_id = "SRC-UPG") {
  db <- file.path(d, "ledger.sqlite")
  init_ledger(db, source_id)
  con <- open_ledger(db, quiet = TRUE)
  ingest_batch(con,
    list(monthly = monthly_file(d, "old_m", rbind(
           mrows("R1", c("2024-01", "2024-02"), 1L, y = 10),
           mrows("R2", c("2024-01", "2024-02"), 1L, y = 5))),
         registration = registration_file(d, "old_r",
           rrows("R1", 1L, "upsert", "2024-02"))),
    list(batch_id = "OLD1"))
  ingest_batch(con,
    list(monthly = monthly_file(d, "old_m2",
           mrows("R1", "2024-02", 2L, y = 77)),
         registration = NULL),
    list(batch_id = "OLD2"))
  before <- compute_export_tables(con)
  close_ledger(con)
  # 去掉 v2 对象并把 schema 降回 v1，模拟升级前的现场账本
  raw <- DBI::dbConnect(RSQLite::SQLite(), db)
  for (t in c("commit_boundary", "baseline_meta",
              "baseline_monthly", "baseline_registration"))
    DBI::dbExecute(raw, paste("DROP TABLE IF EXISTS", t))
  DBI::dbExecute(raw,
    "UPDATE meta SET value='1' WHERE key='schema_version'")
  DBI::dbDisconnect(raw)
  list(db = db, before = before)
}

rebuild_read <- function(res, file = "panel.csv")
  data.table::fread(file.path(res$rebuild_dir, file))

# 逐字节比较两个重建目录里的同名 CSV（fwrite 输出确定，最严格的一致性）
csv_text <- function(dir, file)
  paste(readLines(file.path(dir, file), warn = FALSE), collapse = "\n")

# 内存表的规范化 CSV 文本（忽略 data.table 内部索引/属性）
canon_csv <- function(dt)
  paste(capture.output(data.table::fwrite(dt)), collapse = "\n")

md5_of_dir <- function(dir) {
  fs <- sort(list.files(dir, pattern = "\\.csv$", full.names = TRUE))
  setNames(unname(tools::md5sum(fs)), basename(fs))
}

# ---- 1. 升级基线 == 升级时的当前导出 -----------------------------------------
test_that("v1 老账本升级：基线冻结点与升级瞬间当前导出完全相同", {
  d <- tempfile("rb_upg_"); dir.create(d)
  v1 <- make_v1_ledger(d)
  # 打开旧库必须先被门禁拦下
  err <- tryCatch(open_ledger(v1$db), ledger_error = function(e) e)
  expect_equal(err$code, "schema_upgrade_required")
  init_ledger(v1$db, "SRC-UPG")
  con <- open_ledger(v1$db, quiet = TRUE)
  on.exit(close_ledger(con), add = TRUE)

  expect_equal(ledger_meta(con)$schema_version, "2")
  bm <- DBI::dbGetQuery(con, "SELECT * FROM baseline_meta")
  expect_equal(bm$origin, "upgrade_v1")
  expect_equal(bm$n_monthly_current, 4L)
  expect_equal(bm$n_registration_current, 1L)

  rb <- rebuild_panel(con, list(out_dir = file.path(d, "rb"),
                                batch_id = "__baseline__"))
  expect_equal(rb$boundary_kind, "baseline")
  expect_equal(rb$commit_seq, 0L)
  # 基线重建的四个 CSV 与“升级瞬间当前导出”重算结果逐格一致：
  # 内存表按相同列序、排序后 fwrite 成规范化文本比对
  expect_equal(csv_text(rb$rebuild_dir, "panel.csv"),
               canon_csv(v1$before$panel))
  expect_equal(csv_text(rb$rebuild_dir, "missing_registration.csv"),
               canon_csv(v1$before$missing))
  expect_equal(csv_text(rb$rebuild_dir,
                        "missing_registration_summary.csv"),
               canon_csv(v1$before$missing_summary))
  expect_equal(csv_text(rb$rebuild_dir, "provenance.csv"),
               canon_csv(v1$before$provenance))

  # 升级后立即做普通导出，结果也必须等于基线（升级本身不改业务状态）
  cur <- export_panel(con, list(out_dir = file.path(d, "ex")))
  expect_equal(data.table::fread(file.path(cur$release_dir, "panel.csv")),
               v1$before$panel)
})

test_that("升级中断后重试：不重复基线、不丢失既有数据", {
  d <- tempfile("rb_upgint_"); dir.create(d)
  v1 <- make_v1_ledger(d)
  # 模拟“建了 v2 表但进程在提交前被杀”：全部回滚，库保持 v1
  raw <- DBI::dbConnect(RSQLite::SQLite(), v1$db)
  DBI::dbExecute(raw, "PRAGMA busy_timeout=30000")
  DBI::dbExecute(raw, "BEGIN IMMEDIATE")
  ddl <- schema_ddl()
  for (nm in c("commit_boundary", "baseline_meta",
               "baseline_monthly", "baseline_registration"))
    DBI::dbExecute(raw, ddl[[nm]])
  DBI::dbExecute(raw, "ROLLBACK")
  DBI::dbDisconnect(raw)
  expect_error(open_ledger(v1$db), class = "ledger_error")

  init_ledger(v1$db, "SRC-UPG")
  con <- open_ledger(v1$db, quiet = TRUE)
  on.exit(close_ledger(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM baseline_meta")$n, 1L)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM baseline_monthly")$n, 4L)
  # 再跑一次 init：幂等，不重复冻结
  init_ledger(v1$db, "SRC-UPG")
  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM baseline_meta")$n, 1L)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM baseline_monthly")$n, 4L)
  expect_equal(nrow(query_current_monthly(con)), 4L)
  expect_true(check_ledger_invariants(con)$consistent)
})

# ---- 2. 升级后混合批次按同一边界重建月报与登记 -------------------------------
test_that("升级后混合批次：同一边界同时重建月报与登记，顺序严格按提交", {
  d <- tempfile("rb_mix_"); dir.create(d)
  v1 <- make_v1_ledger(d)
  init_ledger(v1$db, "SRC-UPG")
  con <- open_ledger(v1$db, quiet = TRUE)
  on.exit(close_ledger(con), add = TRUE)

  r_mix <- ingest_batch(con,
    list(monthly = monthly_file(d, "mix_m",
           mrows("R3", c("2024-01", "2024-02"), 1L, y = 30)),
         registration = registration_file(d, "mix_r", rbind(
           rrows("R2", 1L, "upsert", "never"),
           rrows("R3", 1L, "upsert", "2024-01")))),
    list(batch_id = "MIX1"))
  expect_equal(r_mix$status, "committed")
  expect_equal(r_mix$commit_seq, 1L)

  b <- query_boundaries(con)
  expect_equal(b$commit_seq, 0:1)
  expect_equal(b$batch_id, c("__baseline__", "MIX1"))
  expect_equal(b$kind, c("baseline", "batch"))

  rb <- rebuild_panel(con, list(out_dir = file.path(d, "rb"),
                                batch_id = "MIX1"))
  panel <- rebuild_read(rb)
  # R2 在 MIX1 才补登记 -> 进入面板；R3 同批登记 2024-01 起处理
  expect_equal(sort(unique(panel$region_id)), c("R1", "R2", "R3"))
  r3 <- panel[region_id == "R3"]
  expect_equal(r3$treat, c(1L, 1L))           # 2024-01、02 都 >= first_treat
  r2 <- panel[region_id == "R2"]
  expect_true(all(r2$treat == 0L))            # never
  prov <- rebuild_read(rb, "provenance.csv")
  expect_true(all(prov[region_id == "R3"]$monthly_batch_id == "MIX1"))
  expect_true(all(prov[region_id %in% c("R2", "R3")]$registration_batch_id
                  == "MIX1"))
  # 基线重建里 R2/R3 尚不可见（R2 无登记在待补清单、R3 完全不存在）
  rb0 <- rebuild_panel(con, list(out_dir = file.path(d, "rb0"),
                                 batch_id = "__baseline__"))
  expect_false("R3" %in% rebuild_read(rb0)$region_id)
  miss0 <- rebuild_read(rb0, "missing_registration.csv")
  expect_true(all(miss0$region_id == "R2"))
})

# ---- 3. 修订规则在时间旅行视图上逐条成立 --------------------------------------
test_that("旧边界读不到后来的高修订；低修订晚到/delete/恢复均按账本规则", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "b1", rbind(
           mrows("R1", "2024-01", 1L, y = 10),
           mrows("R1", "2024-02", 1L, y = 11))),
         registration = registration_file(d, "r1",
           rrows("R1", 1L, "upsert", "never"))),
    list(batch_id = "B1"))
  # B2：高修订改 2024-02
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "b2",
           mrows("R1", "2024-02", 2L, y = 22)), registration = NULL),
    list(batch_id = "B2"))
  # B3：低修订晚到（stale，不改变任何状态）
  r3 <- ingest_batch(L$con,
    list(monthly = monthly_file(d, "b3",
           mrows("R1", "2024-02", 1L, y = 999)), registration = NULL),
    list(batch_id = "B3"))
  expect_equal(r3$monthly$n_stale, 1L)
  # B4：delete 墓碑（更高修订 r3）
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "b4",
           mrows("R1", "2024-02", 3L, op = "delete",
                 y = NA, x1 = NA, x2 = NA)), registration = NULL),
    list(batch_id = "B4"))
  # B5：墓碑之后再来低修订 upsert，仍不能复活
  r5 <- ingest_batch(L$con,
    list(monthly = monthly_file(d, "b5",
           mrows("R1", "2024-02", 2L, y = 22)), registration = NULL),
    list(batch_id = "B5"))
  expect_equal(r5$monthly$n_stale, 1L)
  # B6：更高修订 upsert 恢复
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "b6",
           mrows("R1", "2024-02", 4L, y = 44)), registration = NULL),
    list(batch_id = "B6"))

  at <- function(seq) {
    rb <- rebuild_panel(L$con,
      list(out_dir = file.path(d, paste0("at", seq)),
           batch_id = paste0("B", seq)))
    rebuild_read(rb)
  }
  p1 <- at(1)
  expect_equal(p1[month == "2024-02"]$y, 11)           # B1 时的旧值
  p2 <- at(2)
  expect_equal(p2[month == "2024-02"]$y, 22)           # B2 高修订
  # B3 不改变：B3 重建仍等于 B2（用规范化 CSV 文本比较，规避内部索引属性）
  p3 <- at(3)
  expect_equal(canon_csv(p3), canon_csv(p2))
  # B4：2024-02 被墓碑屏蔽，只剩 2024-01
  p4 <- at(4)
  expect_equal(p4$month, "2024-01")
  # B5：低修订不能复活，仍只剩 2024-01
  p5 <- at(5)
  expect_equal(canon_csv(p5), canon_csv(p4))
  # B6：高修订恢复
  p6 <- at(6)
  expect_equal(p6[month == "2024-02"]$y, 44)

  # 边界之后的更正不得泄漏：B1 重建里永远看不到 B2+ 的任何值
  expect_false(any(p1$y %in% c(22, 44)))
  # 当前指针始终是最高修订 r4
  cur <- query_current_monthly(L$con)
  expect_equal(cur[month == "2024-02"]$revision, 4L)
  expect_equal(cur[month == "2024-02"]$op, "upsert")
})

test_that("登记的 delete 墓碑与高修订恢复在旧边界上同样成立", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m",
           mrows("R1", c("2024-01", "2024-02"), 1L)),
         registration = registration_file(d, "r1",
           rrows("R1", 1L, "upsert", "2024-02"))),
    list(batch_id = "B1"))
  ingest_batch(L$con,
    list(monthly = NULL,
         registration = registration_file(d, "r2",
           rrows("R1", 2L, "delete", NA))),
    list(batch_id = "B2"))
  ingest_batch(L$con,
    list(monthly = NULL,
         registration = registration_file(d, "r3",
           rrows("R1", 1L, "upsert", "never"))),   # 低修订晚到
    list(batch_id = "B3"))
  ingest_batch(L$con,
    list(monthly = NULL,
         registration = registration_file(d, "r4",
           rrows("R1", 3L, "upsert", "never"))),
    list(batch_id = "B4"))

  rb2 <- rebuild_panel(L$con,
    list(out_dir = file.path(d, "rb2"), batch_id = "B2"))
  expect_equal(nrow(rebuild_read(rb2)), 0L)
  expect_equal(nrow(rebuild_read(rb2, "missing_registration.csv")), 2L)
  rb3 <- rebuild_panel(L$con,
    list(out_dir = file.path(d, "rb3"), batch_id = "B3"))
  expect_equal(nrow(rebuild_read(rb3)), 0L)          # 低修订不复活登记
  rb4 <- rebuild_panel(L$con,
    list(out_dir = file.path(d, "rb4"), batch_id = "B4"))
  p4 <- rebuild_read(rb4)
  expect_equal(nrow(p4), 2L)
  expect_true(all(p4$treat == 0L))
})

# ---- 4. 并发批次边界唯一且可核对 ----------------------------------------------
test_that("并发成功批次得到唯一、无空洞、与提交顺序一致的 commit_seq", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  rows_a <- mrows(paste0("R", 1:15), rep("2024-01", 15), 1L, y = 1)
  rows_b <- mrows(paste0("R", 1:15), rep("2024-01", 15), 2L, y = 2)
  fa <- monthly_file(d, "a", rows_a)
  fb <- monthly_file(d, "b", rows_b)
  sha <- cli_cmd("import_batch.R",
    c("--db", L$db, "--batch-id", "PA", "--monthly", fa),
    file.path(d, "done_a"), file.path(d, "a.log"))
  shb <- cli_cmd("import_batch.R",
    c("--db", L$db, "--batch-id", "PB", "--monthly", fb),
    file.path(d, "done_b"), file.path(d, "b.log"))
  system2("bash", c("-c", shQuote(sha)), wait = FALSE)
  system2("bash", c("-c", shQuote(shb)), wait = FALSE)
  expect_equal(wait_done(file.path(d, "done_a"), timeout = 60), 0L)
  expect_equal(wait_done(file.path(d, "done_b"), timeout = 60), 0L)

  cb <- DBI::dbGetQuery(L$con,
    "SELECT commit_seq, batch_id FROM commit_boundary ORDER BY commit_seq")
  expect_equal(cb$commit_seq, 1:2)               # 无空洞
  expect_equal(sort(cb$batch_id), c("PA", "PB"))
  expect_equal(length(unique(cb$commit_seq)), 2L)

  # 先提交者的重建必须只看到 revision 1；后提交者看到 revision 2
  first_bd <- cb$batch_id[cb$commit_seq == 1L]
  second_bd <- cb$batch_id[cb$commit_seq == 2L]
  rb1 <- rebuild_panel(L$con,
    list(out_dir = file.path(d, "rb_first"), batch_id = first_bd))
  rb2 <- rebuild_panel(L$con,
    list(out_dir = file.path(d, "rb_second"), batch_id = second_bd))
  expect_true(all(rebuild_read(rb1)$y == 1))
  expect_true(all(rebuild_read(rb2)$y == 2))
  # 顺序以账本提交为准，而不是 batch_id 字典序或文件名
  expect_equal(get_boundary_row(L$con, first_bd)$commit_seq, 1L)
  expect_equal(get_boundary_row(L$con, second_bd)$commit_seq, 2L)
})

# ---- 5. 失败事务与重放不制造边界 ----------------------------------------------
test_that("失败批次不占边界；相同 batch_id 相同内容重放仍是原回执", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  f <- monthly_file(d, "ok", mrows("R1", "2024-01", 1L, y = 1))
  r1 <- ingest_batch(L$con, list(monthly = f, registration = NULL),
                     list(batch_id = "B1"))
  # 失败批次（坏月份）
  bad <- write_csv_lines(file.path(d, "bad.csv"),
    c("region_id,month,revision,op,y,x1,x2", "R9,2024-13,1,upsert,1,0,0"))
  err <- tryCatch(
    ingest_batch(L$con, list(monthly = bad, registration = NULL),
                 list(batch_id = "BAD")),
    ledger_error = function(e) e)
  expect_equal(err$code, "file_validation_error")
  # 失败 batch_id 可被新内容复用，且成功后只占一个边界
  good <- monthly_file(d, "good", mrows("R9", "2024-01", 1L, y = 9))
  ingest_batch(L$con, list(monthly = good, registration = NULL),
               list(batch_id = "BAD"))
  # 同内容重放
  replay <- ingest_batch(L$con, list(monthly = f, registration = NULL),
                         list(batch_id = "B1"))
  expect_true(replay$replay)

  cb <- DBI::dbGetQuery(L$con,
    "SELECT commit_seq, batch_id FROM commit_boundary ORDER BY commit_seq")
  expect_equal(cb$commit_seq, 1:2)
  expect_equal(cb$batch_id, c("B1", "BAD"))
  expect_equal(nrow(query_receipts(L$con)), 2L)
  # 失败的“BAD”从未留下被拒回执；重建目标校验只认成功边界
  expect_equal(resolve_rebuild_target(L$con, "B1")$commit_seq, 1L)
  expect_equal(resolve_rebuild_target(L$con, "BAD")$commit_seq, 2L)
})

# ---- 6. 重建是纯只读、可重复、可被分析入口读取 -------------------------------
test_that("重建不改当前导出/账本状态；重复重建逐格一致且能被分析入口读取", {
  root <- LEDGER_TEST_ROOT
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", rbind(
           mrows("R1", c("2024-01", "2024-02"), 1L, y = 10),
           mrows("R2", c("2024-01", "2024-02"), 1L, y = 5))),
         registration = registration_file(d, "r", rbind(
           rrows("R1", 1L, "upsert", "2024-02"),
           rrows("R2", 1L, "upsert", "never")))),
    list(batch_id = "B1"))
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m2",
           mrows("R1", "2024-02", 2L, y = 77)), registration = NULL),
    list(batch_id = "B2"))

  state_before <- ledger_snapshot(L$con)
  cur_before <- query_current_monthly(L$con)
  exp_before <- export_panel(L$con, list(out_dir = file.path(d, "ex")))
  rel_before <- query_releases(L$con)

  out <- file.path(d, "rb")
  rb1 <- rebuild_panel(L$con, list(out_dir = out, batch_id = "B1"))
  rb2 <- rebuild_panel(L$con,
    list(out_dir = file.path(d, "rb_other"), batch_id = "B1"))
  # 两个目录内容（四个 CSV）逐字节一致
  expect_equal(md5_of_dir(rb1$rebuild_dir), md5_of_dir(rb2$rebuild_dir))
  m1 <- jsonlite::fromJSON(file.path(rb1$rebuild_dir, "manifest.json"),
                           simplifyVector = FALSE)
  m2 <- jsonlite::fromJSON(file.path(rb2$rebuild_dir, "manifest.json"),
                           simplifyVector = FALSE)
  expect_equal(m1$target, m2$target)
  expect_equal(m1$baseline$baseline_id, m2$baseline$baseline_id)
  # 身份字段齐全：batch_id、提交顺序、账本/来源身份、文件校验值
  expect_equal(m1$target$batch_id, "B1")
  expect_equal(m1$target$commit_seq, 1L)
  expect_equal(m1$source_id, "SRC-TEST")
  expect_true(nzchar(m1$db_path))
  expect_true(all(vapply(m1$files, function(f) nzchar(f$md5), logical(1))))
  expect_equal(m1$side_effects, "read_only")
  # 边界顺序清单包含基线与两个批次
  expect_equal(vapply(m1$boundary_order, function(x) x$commit_seq, integer(1)),
               0:2)
  expect_equal(vapply(m1$boundary_order, function(x) x$kind, character(1)),
               c("baseline", "batch", "batch"))

  # 账本业务状态与普通导出完全未变
  expect_equal(ledger_snapshot(L$con)[c("n_receipts", "n_monthly_versions",
    "n_monthly_current", "n_registration_versions",
    "n_registration_current")],
    state_before[c("n_receipts", "n_monthly_versions",
      "n_monthly_current", "n_registration_versions",
      "n_registration_current")])
  expect_equal(query_current_monthly(L$con), cur_before)
  expect_equal(query_releases(L$con), rel_before)   # 不登记 export_release

  # 重复重建到同一目录被拒绝，旧目录保持不变
  old_md5 <- md5_of_dir(rb1$rebuild_dir)
  err <- tryCatch(rebuild_panel(L$con, list(out_dir = out, batch_id = "B1")),
                  ledger_error = function(e) e)
  expect_equal(err$code, "rebuild_output_exists")
  expect_equal(md5_of_dir(rb1$rebuild_dir), old_md5)

  # 重建面板能被现有分析入口的校验层直接消费
  source(file.path(root, "R", "utils.R"))
  source(file.path(root, "R", "validate_data.R"))
  cfg <- load_config(file.path(root, "config", "ledger_export.json"))
  cfg$data_path <- file.path(rb1$rebuild_dir, "panel.csv")
  cfg$window_start <- NULL
  cfg$window_end <- NULL
  valid <- load_or_validate(cfg)
  expect_equal(sort(unique(valid$analytic$region_id)), c("R1", "R2"))
  expect_true(all(valid$analytic[region_id == "R1" &
    month >= as.Date("2024-02-01"), treat] == 1))
})

test_that("写出失败不覆盖已存在的结果目录", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R1", "2024-01", 1L)),
         registration = registration_file(d, "r", rrows("R1"))),
    list(batch_id = "B1"))
  out <- file.path(d, "rb")
  ok <- rebuild_panel(L$con, list(out_dir = out, batch_id = "B1"))
  before <- md5_of_dir(ok$rebuild_dir)
  # 只读根目录：staging 无法创建，触发 rebuild_write_failed / invalid_output_dir；
  # 此前已发布的结果目录必须原样保留。结束前恢复权限以便临时目录清理。
  readonly <- file.path(d, "readonly")
  dir.create(readonly)
  on.exit(Sys.chmod(readonly, "755"), add = TRUE)
  Sys.chmod(readonly, "555")
  err <- tryCatch(rebuild_panel(L$con, list(
    out_dir = file.path(readonly, "nested"),
    batch_id = "B1")), ledger_error = function(e) e)
  expect_true(inherits(err, "ledger_error"))
  expect_true(err$code %in% c("invalid_output_dir", "rebuild_write_failed"))
  expect_equal(md5_of_dir(ok$rebuild_dir), before)
  expect_false(any(grepl("^\\.staging", list.files(out))))
})

# ---- 7. 拒绝路径：未知 / 失败 / 基线以前的 batch_id ---------------------------
test_that("未知、失败或升级基线以前的 batch_id 被明确拒绝", {
  d <- tempfile("rb_rej_"); dir.create(d)
  v1 <- make_v1_ledger(d)
  init_ledger(v1$db, "SRC-UPG")
  con <- open_ledger(v1$db, quiet = TRUE)
  on.exit(close_ledger(con), add = TRUE)

  e_unknown <- tryCatch(
    rebuild_panel(con, list(out_dir = file.path(d, "x"), batch_id = "GHOST")),
    ledger_error = function(e) e)
  expect_equal(e_unknown$code, "unknown_batch")

  e_old <- tryCatch(
    rebuild_panel(con, list(out_dir = file.path(d, "y"), batch_id = "OLD1")),
    ledger_error = function(e) e)
  expect_equal(e_old$code, "batch_before_baseline")
  e_old2 <- tryCatch(
    rebuild_panel(con, list(out_dir = file.path(d, "z"), batch_id = "OLD2")),
    ledger_error = function(e) e)
  expect_equal(e_old2$code, "batch_before_baseline")

  # 被拒绝时不得产生任何结果目录
  expect_false(dir.exists(file.path(d, "x")))
  expect_false(dir.exists(file.path(d, "y")))
  expect_false(dir.exists(file.path(d, "z")))

  # 失败 batch_id（账本中根本无回执）走 unknown 分支
  bad <- write_csv_lines(file.path(d, "bad.csv"),
    c("region_id,month,revision,op,y,x1,x2", "R9,2024-13,1,upsert,1,0,0"))
  e_fail <- tryCatch(
    ingest_batch(con, list(monthly = bad, registration = NULL),
                 list(batch_id = "BAD")),
    ledger_error = function(e) e)
  expect_equal(e_fail$code, "file_validation_error")
  e_rebuild_fail <- tryCatch(
    rebuild_panel(con, list(out_dir = file.path(d, "w"), batch_id = "BAD")),
    ledger_error = function(e) e)
  expect_equal(e_rebuild_fail$code, "unknown_batch")

  # 先放一个合法结果目录，再触发拒绝，旧目录原样保留
  good <- rebuild_panel(con,
    list(out_dir = file.path(d, "keep"), batch_id = "__baseline__"))
  keep_md5 <- md5_of_dir(good$rebuild_dir)
  e_again <- tryCatch(
    rebuild_panel(con, list(out_dir = file.path(d, "keep"),
                            batch_id = "GHOST")),
    ledger_error = function(e) e)
  expect_equal(e_again$code, "unknown_batch")
  expect_equal(md5_of_dir(good$rebuild_dir), keep_md5)
})

test_that("新建账本具有等价且可复核的空基线起点", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  bm <- DBI::dbGetQuery(L$con, "SELECT * FROM baseline_meta")
  expect_equal(bm$origin, "fresh_init")
  expect_equal(bm$n_monthly_current, 0L)
  expect_equal(bm$n_registration_current, 0L)
  rb <- rebuild_panel(L$con,
    list(out_dir = file.path(L$dir, "rb"), batch_id = "__baseline__"))
  expect_equal(rb$n_panel, 0L)
  expect_equal(nrow(rebuild_read(rb, "provenance.csv")), 0L)
  # 空基线 manifest 仍带完整身份字段
  m <- jsonlite::fromJSON(file.path(rb$rebuild_dir, "manifest.json"),
                          simplifyVector = FALSE)
  expect_equal(m$target$kind, "baseline")
  expect_equal(m$target$commit_seq, 0L)
  expect_equal(length(m$boundary_order), 1L)
  expect_equal(m$boundary_order[[1]]$batch_id, "__baseline__")
})
