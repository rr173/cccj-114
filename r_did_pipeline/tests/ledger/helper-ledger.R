# =============================================================================
# helper-ledger.R —— 测试公共工具（临时库、临时批次文件、CLI 子进程）
# 所有测试只用临时目录，不写仓库内数据。
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(testthat)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

find_ledger_root <- function(start) {
  d <- normalizePath(start, mustWork = FALSE)
  while (!dir.exists(file.path(d, "R", "ledger")) && d != dirname(d))
    d <- dirname(d)
  if (!dir.exists(file.path(d, "R", "ledger")))
    stop("找不到仓库根（含 R/ledger）")
  d
}

# 测试环境变量优先（run_tests.R 会设置），其次按本文件位置向上查找
if (!nzchar(Sys.getenv("LEDGER_TEST_ROOT")))
  Sys.setenv(LEDGER_TEST_ROOT = find_ledger_root(getwd()))
LEDGER_TEST_ROOT <- find_ledger_root(Sys.getenv("LEDGER_TEST_ROOT"))

source(file.path(LEDGER_TEST_ROOT, "R", "ledger", "core.R"))
source(file.path(LEDGER_TEST_ROOT, "R", "ledger", "validate.R"))
source(file.path(LEDGER_TEST_ROOT, "R", "ledger", "ingest.R"))
source(file.path(LEDGER_TEST_ROOT, "R", "ledger", "export.R"))
source(file.path(LEDGER_TEST_ROOT, "R", "ledger", "query.R"))

LEDGER_TEST_R <- file.path(LEDGER_TEST_ROOT, "..", ".envs", "r-ledger", "bin",
                           "Rscript")
if (!file.exists(LEDGER_TEST_R)) LEDGER_TEST_R <- Sys.which("Rscript")

new_temp_ledger <- function(source_id = "SRC-TEST") {
  d <- tempfile("ledger_test_")
  dir.create(d, recursive = TRUE)
  db <- file.path(d, "ledger.sqlite")
  init_ledger(db, source_id)
  con <- open_ledger(db, quiet = TRUE)
  list(dir = d, db = db, con = con,
       close = function() if (DBI::dbIsValid(con)) close_ledger(con))
}

write_csv_lines <- function(path, lines) {
  writeLines(lines, path, useBytes = TRUE)
  path
}

# 快捷写批次文件
monthly_file <- function(dir, name, rows) {
  p <- file.path(dir, paste0(name, "_monthly.csv"))
  fwrite(as.data.table(rows), p)
  p
}

registration_file <- function(dir, name, rows) {
  p <- file.path(dir, paste0(name, "_registration.csv"))
  fwrite(as.data.table(rows), p)
  p
}

mrows <- function(region_id, month, revision = 1L, op = "upsert",
                  y = 1, x1 = 0, x2 = 0) {
  data.table(region_id = region_id, month = month, revision = revision,
             op = op, y = y, x1 = x1, x2 = x2)
}

rrows <- function(region_id, revision = 1L, op = "upsert",
                  first_treat_month = "never") {
  data.table(region_id = region_id, revision = revision, op = op,
             first_treat_month = first_treat_month)
}

# 运行 CLI 子进程（用于并发测试）；返回 shell 命令，由调用者启动
cli_cmd <- function(script, args, done_file, log_file) {
  quoted <- paste(sprintf("'%s'", gsub("'", "'\\''", args)), collapse = " ")
  sprintf("'%s' '%s' %s > '%s' 2>&1; echo $? > '%s'",
          LEDGER_TEST_R,
          file.path(LEDGER_TEST_ROOT, "R", "ledger", script),
          quoted, log_file, done_file)
}

wait_done <- function(done_file, timeout = 30) {
  t0 <- Sys.time()
  while (!file.exists(done_file)) {
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout)
      stop("等待子进程超时: ", done_file)
    Sys.sleep(0.05)
  }
  as.integer(readLines(done_file, warn = FALSE)[1])
}

wait_file <- function(path, timeout = 30) {
  t0 <- Sys.time()
  while (!file.exists(path)) {
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout)
      stop("等待标记文件超时: ", path)
    Sys.sleep(0.05)
  }
  invisible(TRUE)
}
