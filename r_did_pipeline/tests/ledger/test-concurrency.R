# =============================================================================
# test-concurrency.R —— 实际启动多个进程：
#   A. 两个进程同时导入不同批次：修订比较与写入之间无竞态，两个都成功，
#      最终状态等于先后串行执行的结果，且账本不变量成立。
#   B. 一个进程停在事务内、提交前的暂停点，SIGKILL 后本批完全不可见；
#      重启重传后只生效一次，不重复。
# =============================================================================

test_that("两个进程并发导入不同批次：都成功且最终状态一致", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  # 两个批次写入同一批业务键的不同 revision，检验“检查-写入”是否原子
  rows_a <- mrows(paste0("R", 1:20), rep("2024-01", 20), 1L, y = 1)
  rows_b <- mrows(paste0("R", 1:20), rep("2024-01", 20), 2L, y = 2)
  fa <- monthly_file(d, "a", rows_a)
  fb <- monthly_file(d, "b", rows_b)

  done_a <- file.path(d, "done_a"); done_b <- file.path(d, "done_b")
  sha <- cli_cmd("import_batch.R",
    c("--db", L$db, "--batch-id", "PA", "--monthly", fa), done_a,
    file.path(d, "a.log"))
  shb <- cli_cmd("import_batch.R",
    c("--db", L$db, "--batch-id", "PB", "--monthly", fb), done_b,
    file.path(d, "b.log"))
  # 同时启动
  pa <- system2("bash", c("-c", shQuote(sha)), wait = FALSE)
  pb <- system2("bash", c("-c", shQuote(shb)), wait = FALSE)
  ca <- wait_done(done_a, timeout = 60)
  cb <- wait_done(done_b, timeout = 60)
  expect_equal(ca, 0L)
  expect_equal(cb, 0L)

  cur <- query_current_monthly(L$con)
  expect_equal(nrow(cur), 20L)
  # 两个批次对应同一批键、revision 1 与 2：无论谁先提交，最终都必须是 r2
  expect_true(all(cur$revision == 2L))
  expect_true(all(cur$y == 2))
  snap <- ledger_snapshot(L$con)
  expect_equal(snap$n_receipts, 2L)          # 两个批次都留下回执
  expect_true(check_ledger_invariants(L$con)$consistent)
  expect_true(all(check_ledger_invariants(L$con)$problems == list()))
})

test_that("同一批次两个进程同时提交：一个生效一个重放，不重复", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  f <- monthly_file(d, "same", mrows(paste0("R", 1:30),
                                     rep("2024-02", 30), 1L))
  sh <- function(tag) cli_cmd("import_batch.R",
    c("--db", L$db, "--batch-id", "SAME", "--monthly", f),
    file.path(d, paste0("done_", tag)), file.path(d, paste0(tag, ".log")))
  p1 <- system2("bash", c("-c", shQuote(sh("p1"))), wait = FALSE)
  p2 <- system2("bash", c("-c", shQuote(sh("p2"))), wait = FALSE)
  c1 <- wait_done(file.path(d, "done_p1"), timeout = 60)
  c2 <- wait_done(file.path(d, "done_p2"), timeout = 60)
  expect_equal(c1, 0L); expect_equal(c2, 0L)
  expect_equal(ledger_snapshot(L$con)$n_monthly_versions, 30L)
  expect_equal(ledger_snapshot(L$con)$n_receipts, 1L)
  logs <- c(readLines(file.path(d, "p1.log"), warn = FALSE),
            readLines(file.path(d, "p2.log"), warn = FALSE))
  json <- logs[grepl('^\\{"batch_id"', logs)]
  expect_equal(length(json), 2L)
  expect_equal(sum(grepl('"replay":true', json)), 1L)
})

test_that("SIGKILL 停在提交前的导入进程：本批完全不可见，重启重传只生效一次", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  # 先提交一个基础批次，确认被杀进程不会改变已有数据
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "base",
      mrows(paste0("R", 1:10), rep("2024-01", 10), 1L, y = 1)),
      registration = NULL), list(batch_id = "BASE"))
  before <- ledger_snapshot(L$con)
  current_before <- query_current_monthly(L$con)

  rows <- mrows(paste0("R", 1:50), rep("2024-05", 50), 1L, y = 5)
  f <- monthly_file(d, "kill", rows)
  marker <- file.path(d, "paused.marker")
  done <- file.path(d, "done_kill")
  cmd <- cli_cmd("import_batch.R",
    c("--db", L$db, "--batch-id", "KILL", "--monthly", f,
      "--pause-before-commit", "120", "--pause-marker", marker),
    done, file.path(d, "kill.log"))
  # 后台启动，用 setsid 让进程成为独立进程组的组长，便于整组终止
  system2("bash", c("-c", shQuote(paste0("setsid bash -c ", shQuote(cmd)))),
          wait = FALSE)
  wait_file(marker, timeout = 60)
  pid <- as.integer(readLines(marker, warn = FALSE)[1])
  expect_true(is.finite(pid) && pid > 0)
  # 手动验证进程还活着且停在事务内
  expect_true(file.exists(file.path("/proc", pid)))
  # SIGKILL：不给 R 任何执行 on.exit 回滚的机会（SQLite WAL 在进程死亡后
  # 由下一个连接的回滚恢复保证事务不可见）
  system2("kill", c("-9", as.character(pid)))
  Sys.sleep(0.5)
  expect_false(dir.exists(file.path("/proc", pid)))

  # 下一个连接看账本：没有 KILL 回执、没有新增版本、当前表不变
  con2 <- open_ledger(L$db, quiet = TRUE)
  on.exit(close_ledger(con2), add = TRUE)
  expect_null(get_receipt(con2, "KILL"))
  after <- ledger_snapshot(con2)
  expect_equal(after$n_monthly_versions, before$n_monthly_versions)
  expect_equal(after$n_receipts, before$n_receipts)
  expect_equal(query_current_monthly(con2), current_before)
  expect_equal(nrow(query_current_monthly(con2)), 10L)
  expect_true(check_ledger_invariants(con2)$consistent)

  # 重启重传：只生效一次
  r <- ingest_batch(con2, list(monthly = f, registration = NULL),
                    list(batch_id = "KILL"))
  expect_equal(r$status, "committed")
  expect_equal(r$monthly$n_applied, 50L)
  expect_equal(query_current_monthly(con2)[month == "2024-05", .N], 50L)
  expect_equal(ledger_snapshot(con2)$n_monthly_versions,
               before$n_monthly_versions + 50L)
  # 再重传同一文件：重放，不重复
  r2 <- ingest_batch(con2, list(monthly = f, registration = NULL),
                     list(batch_id = "KILL"))
  expect_true(r2$replay)
  expect_equal(ledger_snapshot(con2)$n_monthly_versions,
               before$n_monthly_versions + 50L)
  expect_equal(ledger_snapshot(con2)$n_receipts, before$n_receipts + 1L)
})

test_that("CLI 失败退出码为 2，且 stdout 只有一行 JSON", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  bad <- write_csv_lines(file.path(d, "bad.csv"),
    c("region_id,month,revision,op,y,x1,x2", "R1,2024-13,1,upsert,1,0,0"))
  done <- file.path(d, "done"); log <- file.path(d, "log")
  cmd <- cli_cmd("import_batch.R",
    c("--db", L$db, "--batch-id", "BAD", "--monthly", bad), done, log)
  expect_equal(system2("bash", c("-c", shQuote(cmd))), 0L)
  expect_equal(wait_done(done), 2L)
  lines <- readLines(log, warn = FALSE)
  parsed <- jsonlite::fromJSON(lines[length(lines)])
  expect_false(parsed$ok)
  expect_equal(parsed$error_code, "file_validation_error")
})
