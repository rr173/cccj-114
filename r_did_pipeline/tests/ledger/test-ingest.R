# =============================================================================
# test-ingest.R —— 修订号语义、回执幂等、事务原子性
# =============================================================================

test_that("初始化绑定 source_id，不允许改绑", {
  L <- new_temp_ledger("SRC-A")
  on.exit(L$close(), add = TRUE)
  expect_equal(ledger_source_id(L$con), "SRC-A")
  expect_equal(init_ledger(L$db, "SRC-A")$status, "exists")
  err <- tryCatch(init_ledger(L$db, "SRC-B"), ledger_error = function(e) e)
  expect_equal(err$code, "source_id_conflict")
  d2 <- tempfile(); dir.create(d2)
  con2 <- open_ledger(L$db, quiet = TRUE)
  on.exit(close_ledger(con2), add = TRUE)
  err2 <- tryCatch(
    ingest_batch(con2, list(monthly = NULL, registration = NULL),
                 list(batch_id = "X")), ledger_error = function(e) e)
  expect_equal(err2$code, "empty_batch")
})

test_that("更高 revision 替换，低 revision 记为过时且不覆盖", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  f1 <- monthly_file(d, "a", mrows("R1", "2024-01", 1L, y = 10))
  r1 <- ingest_batch(L$con, list(monthly = f1, registration = NULL),
                     list(batch_id = "B1"))
  expect_equal(r1$status, "committed")
  expect_equal(r1$monthly$n_applied, 1L)
  expect_equal(query_current_monthly(L$con)$y, 10)

  f2 <- monthly_file(d, "b", mrows("R1", "2024-01", 2L, y = 20))
  r2 <- ingest_batch(L$con, list(monthly = f2, registration = NULL),
                     list(batch_id = "B2"))
  expect_equal(r2$outcomes[[1]]$outcome, "replace")
  expect_equal(query_current_monthly(L$con)$y, 20)

  # 低版本后到
  f3 <- monthly_file(d, "c", mrows("R1", "2024-01", 1L, y = 99))
  r3 <- ingest_batch(L$con, list(monthly = f3, registration = NULL),
                     list(batch_id = "B3"))
  expect_equal(r3$outcomes[[1]]$outcome, "stale")
  expect_equal(r3$monthly$n_stale, 1L)
  expect_equal(query_current_monthly(L$con)$y, 20)
  h <- query_monthly_history(L$con, "R1", "2024-01")
  expect_equal(nrow(h), 2L)           # 只保留有效版本，过时来件在 arrival_log
  al <- DBI::dbGetQuery(L$con,
    "SELECT * FROM arrival_log WHERE batch_id='B3' AND outcome='stale'")
  expect_equal(nrow(al), 1L)
  expect_equal(al$y, 99)
})

test_that("同版本内容一致是重复，内容冲突整批失败且不占批次号", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  f1 <- monthly_file(d, "a", mrows("R1", "2024-01", 1L, y = 10))
  ingest_batch(L$con, list(monthly = f1, registration = NULL),
               list(batch_id = "B1"))
  f2 <- monthly_file(d, "b", mrows("R1", "2024-01", 1L, y = 10))
  r2 <- ingest_batch(L$con, list(monthly = f2, registration = NULL),
                     list(batch_id = "B2"))
  expect_equal(r2$outcomes[[1]]$outcome, "duplicate")
  expect_equal(r2$monthly$n_duplicate, 1L)
  expect_equal(ledger_snapshot(L$con)$n_monthly_versions, 1L)

  f3 <- monthly_file(d, "c", mrows("R1", "2024-01", 1L, y = 11))
  err <- tryCatch(
    ingest_batch(L$con, list(monthly = f3, registration = NULL),
                 list(batch_id = "B3")), ledger_error = function(e) e)
  expect_equal(err$code, "revision_content_conflict")
  expect_null(get_receipt(L$con, "B3"))
  expect_equal(query_current_monthly(L$con)$y, 10)
  # 修正后同 batch_id 可以重新导入
  f4 <- monthly_file(d, "d", mrows("R1", "2024-01", 2L, y = 12))
  r4 <- ingest_batch(L$con, list(monthly = f4, registration = NULL),
                     list(batch_id = "B3"))
  expect_equal(r4$status, "committed")
  expect_equal(query_current_monthly(L$con)$y, 12)
})

test_that("delete 占据最高修订号，旧数据不能复活；仅更高 upsert 恢复", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "a", mrows("R1", "2024-01", 2L, y = 5)),
         registration = NULL), list(batch_id = "B1"))
  r_del <- ingest_batch(L$con,
    list(monthly = monthly_file(d, "b", mrows("R1", "2024-01", 3L,
                                             op = "delete",
                                             y = NA, x1 = NA, x2 = NA)),
         registration = NULL), list(batch_id = "B2"))
  expect_equal(r_del$outcomes[[1]]$outcome, "delete")
  expect_equal(query_current_monthly(L$con)$op, "delete")

  # 旧版本 upsert 后到：stale
  r_old <- ingest_batch(L$con,
    list(monthly = monthly_file(d, "c", mrows("R1", "2024-01", 2L, y = 5)),
         registration = NULL), list(batch_id = "B3"))
  expect_equal(r_old$outcomes[[1]]$outcome, "stale")
  expect_equal(query_current_monthly(L$con)$op, "delete")

  # 同版本 upsert 也不能复活 delete（内容不同 -> 冲突）
  err <- tryCatch(
    ingest_batch(L$con,
      list(monthly = monthly_file(d, "d", mrows("R1", "2024-01", 3L, y = 5)),
           registration = NULL), list(batch_id = "B4")),
    ledger_error = function(e) e)
  expect_equal(err$code, "revision_content_conflict")
  expect_equal(query_current_monthly(L$con)$op, "delete")

  r_restore <- ingest_batch(L$con,
    list(monthly = monthly_file(d, "e", mrows("R1", "2024-01", 4L, y = 6)),
         registration = NULL), list(batch_id = "B5"))
  expect_equal(r_restore$outcomes[[1]]$outcome, "restore")
  expect_equal(query_current_monthly(L$con)$op, "upsert")
  expect_equal(query_current_monthly(L$con)$y, 6)
})

test_that("批内判定与行序无关：等价于按 revision 升序逐条到达", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  rows <- rbind(
    mrows("R1", "2024-01", 3L, y = 30),
    mrows("R1", "2024-01", 1L, y = 10),
    mrows("R1", "2024-01", 2L, y = 20))
  f <- monthly_file(d, "x", rows)
  r <- ingest_batch(L$con, list(monthly = f, registration = NULL),
                    list(batch_id = "B1"))
  # 三条都是该键的新版本，全部进入历史；当前版本是最高 revision
  expect_equal(r$monthly$n_applied, 3L)
  expect_equal(query_current_monthly(L$con)$revision, 3L)
  expect_equal(query_current_monthly(L$con)$y, 30)
  expect_equal(sort(query_monthly_history(L$con)$revision), c(1L, 2L, 3L))

  # 再看一个“库内已有更高版本”的批次：批内低版本按到达顺序分别记为
  # stale / 生效，结果与行序无关。
  L2 <- new_temp_ledger(); on.exit(L2$close(), add = TRUE)
  d2 <- L2$dir
  ingest_batch(L2$con,
    list(monthly = monthly_file(d2, "base", mrows("R1", "2024-01", 5L, y = 50)),
         registration = NULL), list(batch_id = "BASE"))
  rev_batch <- rbind(
    mrows("R1", "2024-01", 7L, y = 70),
    mrows("R1", "2024-01", 6L, y = 60),
    mrows("R1", "2024-01", 4L, y = 40))
  r2 <- ingest_batch(L2$con,
    list(monthly = monthly_file(d2, "rev", rev_batch), registration = NULL),
    list(batch_id = "REV"))
  expect_equal(r2$monthly$n_applied, 2L)   # r6、r7 生效，r4 过时
  expect_equal(r2$monthly$n_stale, 1L)
  expect_equal(query_current_monthly(L2$con)$revision, 7L)
})

test_that("原文件重传返回原回执；换内容必须拒绝", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  f1 <- monthly_file(d, "a", mrows("R1", "2024-01", 1L, y = 1))
  r1 <- ingest_batch(L$con, list(monthly = f1, registration = NULL),
                     list(batch_id = "B1"))
  r1b <- ingest_batch(L$con, list(monthly = f1, registration = NULL),
                      list(batch_id = "B1"))
  expect_true(r1b$replay)
  expect_equal(r1b$received_at, r1$received_at)
  expect_equal(r1b$monthly$n_applied, r1$monthly$n_applied)
  expect_equal(ledger_snapshot(L$con)$n_receipts, 1L)

  f2 <- monthly_file(d, "b", mrows("R1", "2024-02", 1L, y = 2))
  err <- tryCatch(
    ingest_batch(L$con, list(monthly = f2, registration = NULL),
                 list(batch_id = "B1")), ledger_error = function(e) e)
  expect_equal(err$code, "batch_replay_conflict")
  expect_equal(ledger_snapshot(L$con)$n_monthly_versions, 1L)
})

test_that("失败批次不占编号，修正后可导入；成功回执与数据同事务", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  bad <- write_csv_lines(file.path(d, "bad.csv"),
    c("region_id,month,revision,op,y,x1,x2", "R1,2024-13,1,upsert,1,0,0"))
  expect_error(ingest_batch(L$con, list(monthly = bad, registration = NULL),
                            list(batch_id = "B1")), "格式错误")
  expect_null(get_receipt(L$con, "B1"))
  good <- monthly_file(d, "good", mrows("R1", "2024-01", 1L))
  expect_equal(ingest_batch(L$con,
    list(monthly = good, registration = NULL),
    list(batch_id = "B1"))$status, "committed")
})

test_that("事务内中断点：整批不可见（版本行、结果、回执都不留）", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  f <- monthly_file(d, "a", mrows(c("R1", "R2"), c("2024-01", "2024-02")))
  before <- ledger_snapshot(L$con)
  err <- tryCatch(
    ingest_batch(L$con, list(monthly = f, registration = NULL),
                 list(batch_id = "B1", interrupt_at = "before_receipt")),
    ledger_error = function(e) e)
  expect_equal(err$code, "interrupted_at")
  after <- ledger_snapshot(L$con)
  expect_equal(after$n_monthly_versions, before$n_monthly_versions)
  expect_equal(after$n_receipts, before$n_receipts)
  expect_null(get_receipt(L$con, "B1"))
  expect_equal(nrow(DBI::dbGetQuery(L$con,
    "SELECT * FROM ingest_outcome WHERE batch_id='B1'")), 0L)
  # 重传（无中断）成功
  expect_equal(ingest_batch(L$con, list(monthly = f, registration = NULL),
                            list(batch_id = "B1"))$status, "committed")
  expect_equal(ledger_snapshot(L$con)$n_monthly_versions,
               before$n_monthly_versions + 2L)
})

test_that("两类记录可同批提交，互不干扰", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  r <- ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R1", "2024-01", 1L)),
         registration = registration_file(d, "r",
                                           rrows("R1", 1L, "upsert", "2024-03"))),
    list(batch_id = "B1"))
  expect_equal(r$monthly$n_applied, 1L)
  expect_equal(r$registration$n_applied, 1L)
  expect_equal(r$n_inserted, 2L)
  expect_equal(query_current_registration(L$con)$first_treat_month, "2024-03")
})

test_that("缺少登记的月报可以入库", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  r <- ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows(c("R1", "R2"),
                                              c("2024-01", "2024-01"))),
         registration = registration_file(d, "r", rrows("R1"))),
    list(batch_id = "B1"))
  expect_equal(r$monthly$n_applied, 2L)
  expect_equal(nrow(query_current_monthly(L$con)), 2L)
  expect_equal(nrow(query_current_registration(L$con)), 1L)
})

test_that("相同 source_id/batch_id 原文件重传跨连接也返回原回执", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  f <- monthly_file(d, "m", mrows("R1", "2024-01"))
  r1 <- ingest_batch(L$con, list(monthly = f, registration = NULL),
                     list(batch_id = "B1"))
  con2 <- open_ledger(L$db, quiet = TRUE)
  on.exit(close_ledger(con2), add = TRUE)
  r2 <- ingest_batch(con2, list(monthly = f, registration = NULL),
                     list(batch_id = "B1"))
  expect_true(r2$replay)
  expect_equal(r2$received_at, r1$received_at)
})

test_that("schema 版本门禁与升级策略", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  expect_equal(ledger_meta(L$con)$schema_version, "1")
  close_ledger(L$con)

  # 库版本高于代码：拒绝打开
  con <- DBI::dbConnect(RSQLite::SQLite(), L$db)
  DBI::dbExecute(con, "UPDATE meta SET value='99' WHERE key='schema_version'")
  DBI::dbDisconnect(con)
  err <- tryCatch(open_ledger(L$db, quiet = TRUE), ledger_error = function(e) e)
  expect_equal(err$code, "schema_newer_than_code")

  # 低于代码但缺少迁移：拒绝并提示先运行 init_ledger.R
  con <- DBI::dbConnect(RSQLite::SQLite(), L$db)
  DBI::dbExecute(con, "UPDATE meta SET value='0' WHERE key='schema_version'")
  DBI::dbDisconnect(con)
  err <- tryCatch(open_ledger(L$db, quiet = TRUE), ledger_error = function(e) e)
  expect_equal(err$code, "schema_upgrade_required")
  err <- tryCatch(init_ledger(L$db, "SRC-TEST"), ledger_error = function(e) e)
  expect_equal(err$code, "schema_no_migration")

  # 登记迁移后可经 init_ledger.R 升级，失败会回滚
  old <- SCHEMA_MIGRATIONS
  SCHEMA_MIGRATIONS[["0"]] <<- function(c) invisible(TRUE)
  on.exit({ SCHEMA_MIGRATIONS <<- old }, add = TRUE)
  expect_equal(init_ledger(L$db, "SRC-TEST")$status, "exists")
  con2 <- open_ledger(L$db, quiet = TRUE)
  expect_equal(ledger_meta(con2)$schema_version, "1")
  close_ledger(con2)
})
