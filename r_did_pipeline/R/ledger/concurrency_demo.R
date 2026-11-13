#!/usr/bin/env Rscript
# =============================================================================
# concurrency_demo.R —— 真实多进程演练（可直接运行，含竞态与 SIGKILL）
#
# 用法:
#   Rscript R/ledger/concurrency_demo.R [--workdir DIR] [--pause 60]
#
# 步骤：
#   1. 初始化临时账本；
#   2. 同时启动两个 import_batch.R 进程，分别导入同一批业务键的 revision 1
#      与 revision 2（各自一个批次）。两者都会成功，最终当前版本必须是 r2；
#   3. 再同时启动两个进程导入同一个 batch_id：一个提交、一个重放；
#   4. 启动第三个进程并让它停在事务内（--pause-before-commit），标记文件出现
#      后 SIGKILL；核对无回执、当前表与历史表不变，重启重传只生效一次；
#   5. 输出每一步的实际回执与账本核对结果，任何不一致以退出码 1 结束。
# =============================================================================

here <- dirname(normalizePath(sub("--file=", "",
          grep("--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = FALSE))
source(file.path(here, "cli.R"), local = FALSE)
load_ledger_lib()

opts <- parse_cli(commandArgs(trailingOnly = TRUE))
workdir <- cli_opt(opts, "workdir",
                   file.path(tempdir(), paste0("ledger_cc_", Sys.getpid())))
pause_secs <- cli_num(opts, "pause", 60)
dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
db <- file.path(workdir, "ledger.sqlite")
unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE)

rscript <- file.path(R.home("bin"), "Rscript")
script <- function(x) file.path(here, x)
failed <- 0L
check <- function(name, ok, detail = "") {
  cat(sprintf("[%s] %s%s\n", if (isTRUE(ok)) "PASS" else "FAIL", name,
              if (nzchar(detail)) paste0(" — ", detail) else ""))
  if (!isTRUE(ok)) failed <<- failed + 1L
  invisible(isTRUE(ok))
}
run_cli_sync <- function(script_name, args) {
  cmd <- paste(c(shQuote(rscript), shQuote(script(script_name)),
                 vapply(args, shQuote, "")), collapse = " ")
  out <- suppressWarnings(system(paste(cmd, "2>/dev/null"), intern = TRUE))
  list(status = attr(out, "status") %||% 0L,
       json = tryCatch(jsonlite::fromJSON(out[length(out)]),
                       error = function(e) NULL))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

launch <- function(script_name, args, tag) {
  log <- file.path(workdir, paste0(tag, ".log"))
  done <- file.path(workdir, paste0(tag, ".done"))
  cmd <- paste(c(shQuote(rscript), shQuote(script(script_name)),
                 vapply(args, shQuote, "")), collapse = " ")
  sh <- sprintf("bash -c %s > %s 2>&1; echo $? > %s",
                shQuote(cmd), shQuote(log), shQuote(done))
  system2("bash", c("-c", shQuote(paste0("setsid ", sh))), wait = FALSE)
  list(log = log, done = done)
}
wait_done <- function(obj, timeout = 120) {
  t0 <- Sys.time()
  while (!file.exists(obj$done)) {
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout)
      stop("等待子进程超时: ", obj$done)
    Sys.sleep(0.05)
  }
  as.integer(readLines(obj$done, warn = FALSE)[1])
}
result_json <- function(obj) {
  lines <- readLines(obj$log, warn = FALSE)
  j <- lines[grepl('^\\{"', lines)]
  if (length(j) == 0L) return(NULL)
  jsonlite::fromJSON(j[length(j)])
}
mkfile <- function(name, rows) {
  p <- file.path(workdir, paste0(name, ".csv"))
  data.table::fwrite(data.table::as.data.table(rows), p)
  p
}

cat("工作目录:", workdir, "\n\n")

r <- run_cli_sync("init_ledger.R", c("--db", db, "--source-id", "SRC-CC"))
check("初始化账本", identical(r$status, 0L) && r$json$ok)

cat("\n== 1. 两个进程并发写同一批业务键（revision 1 / revision 2）==\n")
fa <- mkfile("cc_a", data.table::data.table(
  region_id = paste0("R", 1:40), month = "2024-01", revision = 1L,
  op = "upsert", y = 1, x1 = 0, x2 = 0))
fb <- mkfile("cc_b", data.table::data.table(
  region_id = paste0("R", 1:40), month = "2024-01", revision = 2L,
  op = "upsert", y = 2, x1 = 0, x2 = 0))
pa <- launch("import_batch.R", c("--db", db, "--batch-id", "CC-A",
                                 "--monthly", fa), "a")
pb <- launch("import_batch.R", c("--db", db, "--batch-id", "CC-B",
                                 "--monthly", fb), "b")
ca <- wait_done(pa); cb <- wait_done(pb)
check("并发进程 A 成功", ca == 0L, paste("exit", ca))
check("并发进程 B 成功", cb == 0L, paste("exit", cb))
con <- open_ledger(db, quiet = TRUE)
cur <- query_current_monthly(con)
check("最终当前版本均为 revision 2（无检查-写入竞态）",
      nrow(cur) == 40L && all(cur$revision == 2L) && all(cur$y == 2),
      sprintf("%d 行, revision=%s", nrow(cur),
              paste(unique(cur$revision), collapse = ",")))
check("两个批次都留下回执", ledger_snapshot(con)$n_receipts == 2L)

cat("\n== 2. 同一 batch_id 两个进程同时提交 ==\n")
fs <- mkfile("cc_same", data.table::data.table(
  region_id = paste0("S", 1:25), month = "2024-02", revision = 1L,
  op = "upsert", y = 9, x1 = 0, x2 = 0))
plan <- function(tag) launch("import_batch.R",
  c("--db", db, "--batch-id", "CC-SAME", "--monthly", fs), tag)
p1 <- plan("s1"); p2 <- plan("s2")
c1 <- wait_done(p1); c2 <- wait_done(p2)
check("两个进程都正常结束", c1 == 0L && c2 == 0L)
j1 <- result_json(p1); j2 <- result_json(p2)
replays <- sum(c(isTRUE(j1$replay), isTRUE(j2$replay)))
check("恰好一个生效、一个返回原回执", replays == 1L,
      sprintf("replay 数=%d", replays))
check("该批次只写入 25 个版本行",
      DBI::dbGetQuery(con,
        "SELECT COUNT(*) AS n FROM monthly_version WHERE batch_id='CC-SAME'")$n == 25L)

cat("\n== 3. 事务内暂停后 SIGKILL，重启重传 ==\n")
fbefore <- ledger_snapshot(con)
cur_before <- query_current_monthly(con)
hist_before <- query_monthly_history(con)
fk <- mkfile("cc_kill", data.table::data.table(
  region_id = paste0("K", 1:60), month = "2024-06", revision = 1L,
  op = "upsert", y = 6, x1 = 0, x2 = 0))
marker <- file.path(workdir, "paused.marker")
unlink(marker)
pk <- launch("import_batch.R", c("--db", db, "--batch-id", "CC-KILL",
                                 "--monthly", fk,
                                 "--pause-before-commit", as.character(pause_secs),
                                 "--pause-marker", marker), "kill")
t0 <- Sys.time()
while (!file.exists(marker)) {
  if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 60)
    stop("等待事务内暂停标记超时")
  Sys.sleep(0.05)
}
pid <- as.integer(readLines(marker, warn = FALSE)[1])
cat("暂停中的导入进程 PID:", pid, "（已持有写锁，尚未提交）\n")
alive <- file.exists(file.path("/proc", pid))
system2("kill", c("-9", as.character(pid)))
Sys.sleep(0.5)
check("进程确实被杀", alive && !file.exists(file.path("/proc", pid)))

con2 <- open_ledger(db, quiet = TRUE)
check("没有留下该批次回执", is.null(get_receipt(con2, "CC-KILL")))
check("当前月报表不变", identical(query_current_monthly(con2), cur_before))
check("历史版本表不变",
      identical(query_monthly_history(con2), hist_before))
check("版本行数与回执数不变",
      ledger_snapshot(con2)$n_monthly_versions ==
        fbefore$n_monthly_versions &&
      ledger_snapshot(con2)$n_receipts == fbefore$n_receipts)
rk <- run_cli_sync("import_batch.R",
  c("--db", db, "--batch-id", "CC-KILL", "--monthly", fk))
check("重启重传成功", identical(rk$status, 0L) && rk$json$monthly$n_applied == 60L)
rk2 <- run_cli_sync("import_batch.R",
  c("--db", db, "--batch-id", "CC-KILL", "--monthly", fk))
check("再次重传返回原回执", isTRUE(rk2$json$replay))
check("只生效一次（版本行 +60）",
      ledger_snapshot(con2)$n_monthly_versions ==
        fbefore$n_monthly_versions + 60L)
check("账本不变量成立", check_ledger_invariants(con2)$consistent)

cat("\n== 回执摘要 ==\n")
rc <- query_receipts(con2, limit = 10L)
print(rc[, .(batch_id, status, received_at)])
cat("\n== 当前数据摘要 ==\n")
print(query_current_monthly(con2)[, .N, by = month])
cat("\n== 历史记录摘要（每个业务键的版本数）==\n")
print(query_monthly_history(con2)[, .N, by = .(region_id, month)][
        , .N, by = N][order(N)])

cat(sprintf("\n==== 并发演练完成：%d 项失败 ====\n", failed))
cat("账本: ", db, "\n", sep = "")
if (failed > 0L) quit(save = "no", status = 1L)
