# =============================================================================
# test-preview.R —— 批次预演：逐行结果、面板/待补差异、重放与冲突、无副作用、
#                    并发一致性、预测面板与正式导出对照、报告发布原子性
# =============================================================================

# 账本全部用户表的内容快照（含 sqlite_sequence 的自增计数），用于断言
# “预演前后账本各表与回执数量不变”。
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

# 在 panel_diff$rows（list）里按键查找
find_panel_row <- function(report, region_id, month) {
  for (row in report$panel_diff$rows)
    if (identical(row$region_id, region_id) && identical(row$month, month))
      return(row)
  NULL
}

find_missing_row <- function(report, region_id, month) {
  for (row in report$missing_diff$rows)
    if (identical(row$region_id, region_id) && identical(row$month, month))
      return(row)
  NULL
}

test_that("混合批次：逐行结果与面板/待补差异（含修订来源）", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  # BASE：R1 三个月（2024-03 已是 r2）+ 登记 2024-03；R2 一个月、无登记
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "base", rbind(
           mrows("R1", "2024-01", 1L, y = 1),
           mrows("R1", "2024-02", 1L, y = 2),
           mrows("R1", "2024-03", 2L, y = 3),
           mrows("R2", "2024-01", 1L, y = 10))),
         registration = registration_file(d, "baser",
           rrows("R1", 1L, "upsert", "2024-03"))),
    list(batch_id = "BASE"))

  # 预演批：replace / insert / duplicate / stale / 新地区 + R2 补登记
  m <- monthly_file(d, "px", rbind(
    mrows("R1", "2024-02", 2L, y = 22),          # replace -> 面板值变化
    mrows("R1", "2024-04", 1L, y = 4),           # insert  -> 面板新增
    mrows("R1", "2024-01", 1L, y = 1),           # duplicate（与库内一致）
    mrows("R1", "2024-03", 1L, y = 3),           # stale（库内已是 r2）
    mrows("R3", "2024-01", 1L, y = 30)))         # insert，但无登记 -> 待补
  r <- registration_file(d, "pxr", rrows("R2", 1L, "upsert", "never"))
  rep <- preview_batch(L$con, list(monthly = m, registration = r),
                       list(batch_id = "PX"))

  expect_equal(rep$verdict, "would_commit")
  expect_equal(rep$batch_id, "PX")
  expect_equal(rep$report_type, "batch_preview")

  # ---- 逐行结果：每种判定都按行给出 -------------------------------------
  oc <- as.data.table(rep$row_outcomes)
  expect_equal(nrow(oc), 6L)
  by_key <- oc[, .(outcome), by = .(kind, region_id, month, revision)]
  get_oc <- function(k, rid, mo)
    by_key[kind == k & region_id == rid & month == mo, outcome]
  expect_equal(get_oc("monthly", "R1", "2024-02"), "replace")
  expect_equal(get_oc("monthly", "R1", "2024-04"), "insert")
  expect_equal(get_oc("monthly", "R1", "2024-01"), "duplicate")
  expect_equal(get_oc("monthly", "R1", "2024-03"), "stale")
  expect_equal(get_oc("monthly", "R3", "2024-01"), "insert")
  expect_equal(by_key[kind == "registration" & region_id == "R2", outcome],
               "insert")
  expect_equal(rep$outcome_counts$insert, 3L)
  expect_equal(rep$outcome_counts$replace, 1L)
  expect_equal(rep$outcome_counts$stale, 1L)
  expect_equal(rep$outcome_counts$duplicate, 1L)

  # ---- 面板差异：新增 / 值变化 / 不受影响，且保留月报与登记修订来源 ------
  expect_equal(rep$panel_diff$counts$added, 2L)     # R1/2024-04, R2/2024-01
  expect_equal(rep$panel_diff$counts$changed, 1L)   # R1/2024-02
  expect_equal(rep$panel_diff$counts$removed, 0L)
  expect_equal(rep$panel_diff$counts$unchanged, 2L) # R1/2024-01, R1/2024-03

  ch <- find_panel_row(rep, "R1", "2024-02")
  expect_equal(ch$status, "changed")
  expect_equal(ch$changed_fields, "y")
  expect_equal(ch$before$y, 2)
  expect_equal(ch$after$y, 22)
  expect_equal(ch$before$monthly_revision, 1L)
  expect_equal(ch$after$monthly_revision, 2L)
  expect_equal(ch$after$monthly_batch_id, "PX")
  expect_equal(ch$after$registration_revision, 1L)   # 登记未变
  expect_equal(ch$after$registration_batch_id, "BASE")

  ad <- find_panel_row(rep, "R1", "2024-04")
  expect_equal(ad$status, "added")
  expect_null(ad$before)
  expect_equal(ad$after$y, 4)
  expect_equal(ad$after$treat, 1L)                   # 2024-04 >= 2024-03

  un <- find_panel_row(rep, "R1", "2024-01")
  expect_equal(un$status, "unchanged")
  expect_equal(un$before$y, 1)

  r2 <- find_panel_row(rep, "R2", "2024-01")
  expect_equal(r2$status, "added")                   # 补登记后进入面板
  expect_equal(r2$after$first_treat_month, "never")
  expect_equal(r2$after$treat, 0L)
  expect_equal(r2$after$registration_revision, 1L)
  expect_equal(r2$after$registration_batch_id, "PX")

  # ---- 待补清单差异：R3 进入（从未登记），R2 离开 -------------------------
  expect_equal(rep$missing_diff$counts$entered, 1L)
  expect_equal(rep$missing_diff$counts$left, 1L)
  en <- find_missing_row(rep, "R3", "2024-01")
  expect_equal(en$status, "entered")
  expect_equal(en$after_reason, "no_registration")
  expect_equal(en$monthly_batch_id, "PX")
  lf <- find_missing_row(rep, "R2", "2024-01")
  expect_equal(lf$status, "left")
  expect_equal(lf$before_reason, "no_registration")

  # ---- 预测面板/待补清单完整给出 ------------------------------------------
  expect_equal(nrow(rep$predicted_panel), 5L)
  expect_equal(names(rep$predicted_panel), PANEL_COLS)
  expect_equal(nrow(rep$predicted_missing_registration), 1L)
  expect_equal(names(rep$predicted_missing_registration), MISSING_COLS)

  # ---- 状态边界：记录的是预演时的账本状态 ---------------------------------
  b0 <- capture_state_boundary(L$con)
  expect_equal(rep$state_boundary$max_monthly_record_id,
               b0$max_monthly_record_id)
  expect_equal(rep$state_boundary$max_registration_record_id,
               b0$max_registration_record_id)
  expect_true(grepl("本次预演所依据", rep$state_boundary$note))
})

test_that("登记更正跨月重算 treat；撤回登记显示移出面板的月报与待补变化", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  months <- c("2024-01", "2024-02", "2024-03", "2024-04")
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R1", months, 1L)),
         registration = registration_file(d, "r",
           rrows("R1", 1L, "upsert", "2024-04"))),
    list(batch_id = "BASE"))

  # ---- 更正 first_treat_month 2024-04 -> 2024-02：所有现存月份重算 --------
  rep <- preview_batch(L$con,
    list(monthly = NULL,
         registration = registration_file(d, "r2",
           rrows("R1", 2L, "upsert", "2024-02"))),
    list(batch_id = "FIX"))
  expect_equal(rep$verdict, "would_commit")
  expect_equal(rep$panel_diff$counts$changed, 4L)   # 全部月份都受影响
  expect_equal(rep$panel_diff$counts$unchanged, 0L)
  for (mo in months) {
    row <- find_panel_row(rep, "R1", mo)
    expect_equal(row$status, "changed")
    expect_equal(row$after$first_treat_month, "2024-02")
    expect_equal(row$after$registration_revision, 2L)
    expect_equal(row$after$registration_batch_id, "FIX")
    expect_equal(row$before$registration_revision, 1L)
  }
  # treat 变化只发生在跨阈值的月份；first_treat_month 所有月份都变
  expect_equal(find_panel_row(rep, "R1", "2024-01")$changed_fields,
               "first_treat_month")
  expect_equal(find_panel_row(rep, "R1", "2024-02")$changed_fields,
               c("first_treat_month", "treat"))
  expect_equal(find_panel_row(rep, "R1", "2024-03")$changed_fields,
               c("first_treat_month", "treat"))
  expect_equal(find_panel_row(rep, "R1", "2024-04")$changed_fields,
               "first_treat_month")
  expect_equal(find_panel_row(rep, "R1", "2024-02")$after$treat, 1L)
  expect_equal(find_panel_row(rep, "R1", "2024-01")$after$treat, 0L)

  # ---- 撤回登记：月报从面板移出，全部月份进入待补 -------------------------
  rep2 <- preview_batch(L$con,
    list(monthly = NULL,
         registration = registration_file(d, "r3",
           rrows("R1", 3L, op = "delete", first_treat_month = NA))),
    list(batch_id = "WD"))
  expect_equal(rep2$verdict, "would_commit")
  expect_equal(rep2$panel_diff$counts$removed, 4L)
  expect_equal(rep2$missing_diff$counts$entered, 4L)
  for (mo in months) {
    prow <- find_panel_row(rep2, "R1", mo)
    expect_equal(prow$status, "removed")
    expect_null(prow$after)
    expect_equal(prow$before$registration_revision, 1L)
    mrow <- find_missing_row(rep2, "R1", mo)
    expect_equal(mrow$status, "entered")
    expect_equal(mrow$after_reason, "registration_withdrawn")
  }
  expect_equal(nrow(rep2$predicted_panel), 0L)
  expect_equal(nrow(rep2$predicted_missing_registration), 4L)
  # 撤回是 delete 判定
  oc <- as.data.table(rep2$row_outcomes)
  expect_equal(oc$outcome, "delete")
})

test_that("同版本冲突（对库内/批内）与文件校验错误：整批拒绝报告", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R1", "2024-01", 1L, y = 1)),
         registration = NULL), list(batch_id = "BASE"))
  before <- ledger_table_dump(L$con)

  # 与库内同版本不同内容 -> would_reject / revision_content_conflict
  rep <- preview_batch(L$con,
    list(monthly = monthly_file(d, "c1", mrows("R1", "2024-01", 1L, y = 2)),
         registration = NULL), list(batch_id = "C1"))
  expect_equal(rep$verdict, "would_reject")
  expect_equal(rep$rejection$error_code, "revision_content_conflict")
  expect_true(grepl("整批回滚", rep$rejection$message))
  expect_equal(length(rep$rejection$offending_rows), 1L)
  off <- rep$rejection$offending_rows[[1]]
  expect_equal(off$region_id, "R1")
  expect_equal(off$month, "2024-01")
  expect_equal(off$revision, 1L)
  expect_null(rep$predicted_panel)
  expect_null(rep$panel_diff)

  # 批内同键同版本不同内容 -> would_reject / same_revision_conflict
  rep2 <- preview_batch(L$con,
    list(monthly = monthly_file(d, "c2", rbind(
           mrows("R1", "2024-02", 1L, y = 1),
           mrows("R1", "2024-02", 1L, y = 2))),
         registration = NULL), list(batch_id = "C2"))
  expect_equal(rep2$verdict, "would_reject")
  expect_equal(rep2$rejection$error_code, "same_revision_conflict")
  expect_true(length(rep2$rejection$offending_rows) >= 1L)

  # 单元格级校验错误 -> would_reject / file_validation_error，逐格列出
  bad <- write_csv_lines(file.path(d, "bad.csv"),
    c("region_id,month,revision,op,y,x1,x2",
      "R1,2024-13,1,upsert,1,0,0",
      "R2,2024-01,1,upsert,abc,0,0"))
  rep3 <- preview_batch(L$con, list(monthly = bad, registration = NULL),
                        list(batch_id = "C3"))
  expect_equal(rep3$verdict, "would_reject")
  expect_equal(rep3$rejection$error_code, "file_validation_error")
  expect_equal(length(rep3$rejection$offending_rows), 2L)
  fields <- vapply(rep3$rejection$offending_rows, `[[`, "", "field")
  expect_true(all(c("month", "y") %in% fields))

  # 拒绝报告不留下任何痕迹
  expect_equal(ledger_table_dump(L$con), before)
})

test_that("已有 batch_id：相同文件标记重放不模拟，不同文件照正式规则拒绝", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  f1 <- monthly_file(d, "a", mrows("R1", "2024-01", 1L, y = 1))
  r1 <- ingest_batch(L$con, list(monthly = f1, registration = NULL),
                     list(batch_id = "B1"))
  before <- ledger_table_dump(L$con)

  # 相同文件 -> replay 报告，明确标记，不模拟再次生效
  rep <- preview_batch(L$con, list(monthly = f1, registration = NULL),
                       list(batch_id = "B1"))
  expect_equal(rep$verdict, "replay")
  expect_equal(rep$replay$received_at, r1$received_at)
  expect_equal(rep$replay$original_summary$monthly$n_applied, 1L)
  expect_true(grepl("重放", rep$replay$note))
  expect_null(rep$predicted_panel)
  expect_null(rep$row_outcomes)
  expect_null(rep$panel_diff)
  expect_equal(ledger_table_dump(L$con), before)

  # 不同内容 -> 与正式导入相同的拒绝（batch_replay_conflict），不写报告
  f2 <- monthly_file(d, "b", mrows("R1", "2024-02", 1L, y = 2))
  err <- tryCatch(
    preview_batch(L$con, list(monthly = f2, registration = NULL),
                  list(batch_id = "B1")),
    ledger_error = function(e) e)
  expect_equal(err$code, "batch_replay_conflict")
  expect_equal(ledger_table_dump(L$con), before)
})

test_that("预演不留下任何痕迹：各表内容与回执数量不变，同批可再正式导入", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R1", c("2024-01", "2024-02"),
                                              1L)),
         registration = registration_file(d, "r", rrows("R1"))),
    list(batch_id = "BASE"))
  # 再导出一次，让 export_release/export_file 也有数据，验证预演不动它们
  export_panel(L$con, list(out_dir = file.path(d, "exports")))
  before <- ledger_table_dump(L$con)
  expect_true(length(before) >= 9L)

  fx <- monthly_file(d, "x", mrows("R1", "2024-03", 1L, y = 9))
  frx <- registration_file(d, "x", rrows("R2", 1L, "upsert", "never"))

  # would_commit 预演
  rep1 <- preview_batch(L$con, list(monthly = fx, registration = frx),
                        list(batch_id = "PX1"))
  expect_equal(rep1$verdict, "would_commit")
  expect_equal(ledger_table_dump(L$con), before)

  # would_reject 预演
  rep2 <- preview_batch(L$con,
    list(monthly = monthly_file(d, "c", mrows("R1", "2024-01", 1L, y = 99)),
         registration = NULL), list(batch_id = "PX2"))
  expect_equal(rep2$verdict, "would_reject")
  expect_equal(ledger_table_dump(L$con), before)

  # replay 预演
  rep3 <- preview_batch(L$con,
    list(monthly = monthly_file(d, "m2", mrows("R1", c("2024-01", "2024-02"),
                                               1L)),
         registration = registration_file(d, "r2", rrows("R1"))),
    list(batch_id = "BASE"))
  expect_equal(rep3$verdict, "replay")
  expect_equal(ledger_table_dump(L$con), before)

  # 同一 batch_id 原文件不变仍可正式导入（预演没有占用编号）
  ok <- ingest_batch(L$con, list(monthly = fx, registration = frx),
                     list(batch_id = "PX1"))
  expect_equal(ok$status, "committed")
  expect_false(isTRUE(ok$replay))
  expect_equal(query_current_monthly(L$con)[month == "2024-03", y], 9)
})

test_that("成功预演的预测面板与紧接着正式导入后的导出逐行一致", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "base", rbind(
           mrows("R1", c("2024-01", "2024-02", "2024-03"), 1L, y = 1),
           mrows("R2", "2024-01", 1L, y = 5))),
         registration = registration_file(d, "baser",
           rrows("R1", 1L, "upsert", "2024-03"))),
    list(batch_id = "BASE"))
  fx <- monthly_file(d, "x", rbind(
    mrows("R1", "2024-02", 2L, y = 2.5),
    mrows("R1", "2024-04", 1L, y = 4),
    mrows("R3", c("2024-01", "2024-02"), 1L, y = 7)))
  frx <- registration_file(d, "x", rbind(
    rrows("R2", 1L, "upsert", "2024-01"),
    rrows("R3", 1L, "upsert", "never")))

  rep <- preview_batch(L$con, list(monthly = fx, registration = frx),
                       list(batch_id = "PX"))
  expect_equal(rep$verdict, "would_commit")
  # 报告落盘（走 JSON 序列化），随后正式导入同一文件并导出
  rp <- write_preview_report(rep, file.path(d, "reports", "px.json"))
  ingest_batch(L$con, list(monthly = fx, registration = frx),
               list(batch_id = "PX"))
  ex <- export_panel(L$con, list(out_dir = file.path(d, "exports")))

  exported <- fread(file.path(ex$release_dir, "panel.csv"))
  predicted <- as.data.table(rep$predicted_panel)
  expect_equal(nrow(predicted), nrow(exported))
  expect_equal(predicted$region_id, exported$region_id)
  expect_equal(predicted$month, exported$month)
  expect_equal(predicted$first_treat_month, exported$first_treat_month)
  expect_equal(predicted$treat, exported$treat)
  expect_equal(predicted$y, exported$y)
  expect_equal(predicted$x1, exported$x1)
  expect_equal(predicted$x2, exported$x2)

  # 待补清单（含模拟的 monthly_record_id）也逐行一致
  exp_m <- fread(file.path(ex$release_dir, "missing_registration.csv"))
  pre_m <- as.data.table(rep$predicted_missing_registration)
  expect_equal(nrow(pre_m), nrow(exp_m))
  if (nrow(exp_m) > 0L) {
    expect_equal(pre_m$region_id, exp_m$region_id)
    expect_equal(pre_m$month, exp_m$month)
    expect_equal(pre_m$monthly_record_id, exp_m$monthly_record_id)
    expect_equal(pre_m$monthly_revision, exp_m$monthly_revision)
    expect_equal(pre_m$monthly_batch_id, exp_m$monthly_batch_id)
    expect_equal(pre_m$reason, exp_m$reason)
  }

  # 报告文件里的预测面板与内存对象一致（JSON 往返不丢行）
  rj <- jsonlite::fromJSON(rp, simplifyVector = FALSE)
  expect_equal(rj$verdict, "would_commit")
  expect_equal(length(rj$predicted_panel), nrow(exported))
  json_regions <- vapply(rj$predicted_panel, `[[`, "", "region_id")
  expect_equal(json_regions, exported$region_id)
})

test_that("报告全部生成后才发布；失败的预演不覆盖已有报告", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  f1 <- monthly_file(d, "a", mrows("R1", "2024-01", 1L, y = 1))
  ingest_batch(L$con, list(monthly = f1, registration = NULL),
               list(batch_id = "B1"))
  rp <- file.path(d, "reports", "preview.json")

  # 成功预演 -> 发布
  rep <- preview_batch(L$con,
    list(monthly = monthly_file(d, "p", mrows("R2", "2024-01", 1L)),
         registration = NULL), list(batch_id = "OK1"))
  write_preview_report(rep, rp)
  expect_true(file.exists(rp))
  first <- readLines(rp, warn = FALSE)
  expect_true(any(grepl('"would_commit"', first)))

  # 失败的预演（batch_id 对应不同内容）-> 报错且已有报告不变
  err <- tryCatch({
    preview_batch(L$con,
      list(monthly = monthly_file(d, "q", mrows("R9", "2024-09", 1L)),
           registration = NULL), list(batch_id = "B1"))
    # 若 preview_batch 抛错，下面的写盘根本不会执行；这里模拟 CLI 顺序：
    write_preview_report(list(should = "never_happen"), rp)
  }, ledger_error = function(e) e)
  expect_equal(err$code, "batch_replay_conflict")
  expect_equal(readLines(rp, warn = FALSE), first)

  # 写盘失败（目标在非法路径下）也不留临时文件、不破坏已有报告
  err2 <- tryCatch(
    write_preview_report(rep, file.path(rp, "nested", "x.json")),
    ledger_error = function(e) e)
  expect_true(is_ledger_error(err2))
  expect_equal(readLines(rp, warn = FALSE), first)
  expect_equal(length(list.files(dirname(rp), pattern = "^\\.")), 0L)

  # 新的成功预演可以正常覆盖发布（原子替换）
  rep2 <- preview_batch(L$con, list(monthly = f1, registration = NULL),
                        list(batch_id = "B1"))
  expect_equal(rep2$verdict, "replay")
  write_preview_report(rep2, rp)
  now <- readLines(rp, warn = FALSE)
  expect_true(any(grepl('"replay"', now)))
})

test_that("并发导入期间预演：边界与报告一致，不混入对方批次", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "base",
           mrows(paste0("B", 1:5), rep("2024-01", 5), 1L)),
         registration = registration_file(d, "baser",
           rrows(paste0("B", 1:5), 1L, "upsert", "never"))),
    list(batch_id = "BASE"))
  m0 <- capture_state_boundary(L$con)

  # 预演批 PX（地区 P*）与并发导入批 IY（地区 Y*）互不相交
  fx <- monthly_file(d, "px", mrows(paste0("P", 1:5), rep("2024-01", 5), 1L))
  frx <- registration_file(d, "pxr", rrows(paste0("P", 1:5), 1L, "upsert",
                                           "2024-01"))
  fy <- monthly_file(d, "iy", mrows(paste0("Y", 1:8), rep("2024-01", 8), 1L))
  fry <- registration_file(d, "iyr", rrows(paste0("Y", 1:8), 1L, "upsert",
                                           "2024-01"))

  marker <- file.path(d, "preview.marker")
  report_path <- file.path(d, "preview.json")
  done_p <- file.path(d, "done_p"); done_i <- file.path(d, "done_i")
  cmd_p <- cli_cmd("preview_batch.R",
    c("--db", L$db, "--batch-id", "PX",
      "--monthly", fx, "--registration", frx,
      "--report", report_path,
      "--pause-inside-tx", "4", "--pause-marker", marker),
    done_p, file.path(d, "p.log"))
  system2("bash", c("-c", shQuote(cmd_p)), wait = FALSE)
  wait_file(marker, timeout = 60)   # 预演已持有写锁
  cmd_i <- cli_cmd("import_batch.R",
    c("--db", L$db, "--batch-id", "IY",
      "--monthly", fy, "--registration", fry),
    done_i, file.path(d, "i.log"))
  system2("bash", c("-c", shQuote(cmd_i)), wait = FALSE)

  cp <- wait_done(done_p, timeout = 90)
  ci <- wait_done(done_i, timeout = 90)
  expect_equal(cp, 0L)
  expect_equal(ci, 0L)

  # 报告边界 = 预演开始前的状态（IY 未混入）
  rep <- jsonlite::fromJSON(report_path, simplifyVector = FALSE)
  expect_equal(rep$verdict, "would_commit")
  expect_equal(rep$state_boundary$max_monthly_record_id,
               m0$max_monthly_record_id)
  expect_equal(rep$state_boundary$max_registration_record_id,
               m0$max_registration_record_id)
  expect_equal(rep$state_boundary$n_monthly_versions,
               m0$n_monthly_versions)

  # 预测面板含 BASE+PX 地区，绝不含 IY 地区
  regions <- unique(vapply(rep$predicted_panel, `[[`, "", "region_id"))
  expect_true(all(paste0("P", 1:5) %in% regions))
  expect_true(all(paste0("B", 1:5) %in% regions))
  expect_false(any(paste0("Y", 1:8) %in% regions))
  # 面板差异行里也没有 IY 的痕迹
  diff_regions <- unique(vapply(rep$panel_diff$rows, `[[`, "", "region_id"))
  expect_false(any(paste0("Y", 1:8) %in% diff_regions))

  # IY 已正式提交：账本前进，但报告仍描述预演时的状态
  now <- capture_state_boundary(L$con)
  expect_gt(now$max_monthly_record_id, m0$max_monthly_record_id)
  expect_gt(now$max_registration_record_id, m0$max_registration_record_id)
  # 预演本身没有留下任何痕迹
  expect_null(get_receipt(L$con, "PX"))
  expect_equal(DBI::dbGetQuery(L$con,
    "SELECT COUNT(*) AS n FROM monthly_version WHERE batch_id='PX'")$n, 0L)
  expect_equal(DBI::dbGetQuery(L$con,
    "SELECT COUNT(*) AS n FROM receipt")$n, 2L)   # BASE + IY
  expect_true(check_ledger_invariants(L$con)$consistent)
})

test_that("月报删除与恢复：预演显示退出/重回面板", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R1", c("2024-01", "2024-02"),
                                              1L, y = 1)),
         registration = registration_file(d, "r", rrows("R1"))),
    list(batch_id = "BASE"))

  # 删除 2024-02：该月退出面板（不进入待补，因为月报本身没了）
  rep <- preview_batch(L$con,
    list(monthly = monthly_file(d, "del",
           mrows("R1", "2024-02", 2L, op = "delete", y = NA, x1 = NA, x2 = NA)),
         registration = NULL), list(batch_id = "DEL"))
  expect_equal(rep$verdict, "would_commit")
  oc <- as.data.table(rep$row_outcomes)
  expect_equal(oc$outcome, "delete")
  expect_equal(rep$panel_diff$counts$removed, 1L)
  expect_equal(rep$missing_diff$counts$entered, 0L)
  expect_equal(rep$missing_diff$counts$left, 0L)
  expect_equal(nrow(rep$predicted_panel), 1L)
  expect_equal(rep$predicted_panel$month, "2024-01")

  # 正式删除后，再预演更高版本 upsert：恢复，该月重回面板
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "del2",
           mrows("R1", "2024-02", 2L, op = "delete", y = NA, x1 = NA, x2 = NA)),
         registration = NULL), list(batch_id = "DEL"))
  rep2 <- preview_batch(L$con,
    list(monthly = monthly_file(d, "rst", mrows("R1", "2024-02", 3L, y = 5)),
         registration = NULL), list(batch_id = "RST"))
  expect_equal(rep2$verdict, "would_commit")
  expect_equal(as.data.table(rep2$row_outcomes)$outcome, "restore")
  expect_equal(rep2$panel_diff$counts$added, 1L)
  row <- find_panel_row(rep2, "R1", "2024-02")
  expect_equal(row$status, "added")
  expect_equal(row$after$y, 5)
  expect_equal(row$after$monthly_revision, 3L)
})

test_that("预演 CLI：命令可运行、退出码与 stdout JSON 符合契约", {
  d <- tempfile("preview_cli_"); dir.create(d)
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
  run("init_ledger.R", c("--db", db, "--source-id", "SRC-CLI"))

  m1 <- monthly_file(d, "m1", mrows("R1", "2024-01", 1L, y = 1))
  r1 <- registration_file(d, "r1", rrows("R1", 1L, "upsert", "2024-01"))
  rp <- file.path(d, "out", "preview.json")

  # 缺少 --report -> 退出码 2
  r <- run("preview_batch.R", c("--db", db, "--batch-id", "P1",
                                "--monthly", m1))
  expect_equal(r$status, 2L)
  expect_equal(r$json$error_code, "missing_report_path")

  # 正常预演 -> 0，报告发布，stdout 摘要一致
  r <- run("preview_batch.R", c("--db", db, "--batch-id", "P1",
    "--monthly", m1, "--registration", r1, "--report", rp))
  expect_equal(r$status, 0L)
  expect_true(r$json$ok)
  expect_equal(r$json$verdict, "would_commit")
  expect_true(file.exists(rp))
  expect_equal(r$json$outcome_counts$insert, 2L)
  expect_equal(r$json$panel_diff_counts$added, 1L)
  expect_equal(r$json$n_predicted_panel, 1L)
  expect_equal(r$json$state_boundary$max_monthly_record_id, 0L)
  rep <- jsonlite::fromJSON(rp)
  expect_equal(rep$verdict, "would_commit")
  expect_equal(rep$batch_id, "P1")

  # 账本没有任何变化
  r <- run("query_ledger.R", c("--db", db, "--what", "snapshot"))
  expect_equal(r$json$n_receipts, 0L)
  expect_equal(r$json$n_monthly_versions, 0L)

  # 正式导入后重放预演 -> verdict=replay
  r <- run("import_batch.R", c("--db", db, "--batch-id", "P1",
    "--monthly", m1, "--registration", r1))
  expect_equal(r$status, 0L)
  r <- run("preview_batch.R", c("--db", db, "--batch-id", "P1",
    "--monthly", m1, "--registration", r1, "--report", rp))
  expect_equal(r$status, 0L)
  expect_equal(r$json$verdict, "replay")
  expect_true(r$json$replay)

  # 同编号不同内容 -> 退出码 2（batch_replay_conflict），报告不被覆盖
  before <- readLines(rp, warn = FALSE)
  m2 <- monthly_file(d, "m2", mrows("R2", "2024-01", 1L))
  r <- run("preview_batch.R", c("--db", db, "--batch-id", "P1",
                                "--monthly", m2, "--report", rp))
  expect_equal(r$status, 2L)
  expect_equal(r$json$error_code, "batch_replay_conflict")
  expect_equal(readLines(rp, warn = FALSE), before)

  # 冲突批次 -> 退出码 0，报告 verdict=would_reject
  m3 <- monthly_file(d, "m3", mrows("R1", "2024-01", 1L, y = 99))
  r <- run("preview_batch.R", c("--db", db, "--batch-id", "P9",
                                "--monthly", m3, "--report", rp))
  expect_equal(r$status, 0L)
  expect_equal(r$json$verdict, "would_reject")
  expect_equal(r$json$error_code, "revision_content_conflict")
  rep <- jsonlite::fromJSON(rp)
  expect_equal(rep$verdict, "would_reject")
})
