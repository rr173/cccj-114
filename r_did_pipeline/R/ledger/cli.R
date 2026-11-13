# =============================================================================
# cli.R —— 命令行公共层：参数解析、加载、JSON 结果与错误契约
#
# stdout 只写一行 JSON 结果；日志写 stderr；退出码：
#   0 成功
#   1 未预期错误（R 条件，含 bug）
#   2 可预期的校验/业务拒绝（ledger_error）
# =============================================================================

ledger_root <- function() {
  args <- commandArgs(FALSE)
  f <- grep("--file=", args, value = TRUE)
  if (length(f) > 0L) {
    script <- normalizePath(sub("--file=", "", f[1]), mustWork = FALSE)
    # 入口脚本位于 R/ 或 tests/ledger/ 下，向上两级即仓库根
    d <- dirname(script)
    for (i in 1:3) {
      if (dir.exists(file.path(d, "R", "ledger"))) return(d)
      d <- dirname(d)
    }
    return(dirname(dirname(script)))
  }
  normalizePath(getwd(), mustWork = FALSE)
}

load_ledger_lib <- function(files = c("core.R", "validate.R", "ingest.R",
                                      "export.R", "query.R")) {
  root <- ledger_root()
  for (f in files) {
    p <- file.path(root, "R", "ledger", f)
    if (!file.exists(p)) stop("缺少账本模块: ", p)
    source(p, local = FALSE)
  }
  Sys.setenv(LEDGER_ROOT = root)
  invisible(root)
}

parse_cli <- function(argv) {
  out <- list()
  i <- 1L
  while (i <= length(argv)) {
    a <- argv[[i]]
    if (!grepl("^--", a)) stop("无法识别的参数: ", a)
    key <- sub("^--", "", a)
    key <- gsub("-", "_", key)
    if (grepl("=", key, fixed = TRUE)) {
      kv <- strsplit(key, "=", fixed = TRUE)[[1]]
      out[[kv[1]]] <- kv[2]
      i <- i + 1L
      next
    }
    if (i == length(argv) || grepl("^--", argv[[i + 1L]])) {
      out[[key]] <- TRUE
      i <- i + 1L
    } else {
      out[[key]] <- argv[[i + 1L]]
      i <- i + 2L
    }
  }
  out
}

cli_opt <- function(opts, name, default = NULL) {
  if (!is.null(opts[[name]])) opts[[name]] else default
}

# --flag 无值时为 TRUE；数值型选项需要显式转换
cli_num <- function(opts, name, default = NULL) {
  v <- cli_opt(opts, name)
  if (is.null(v) || isTRUE(v)) return(default)
  suppressWarnings(as.numeric(v))
}

json_out <- function(obj) {
  cat(jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", digits = NA,
                       pretty = FALSE), "\n", sep = "")
  flush(stdout())
}

# 统一入口：tryCatch -> JSON 错误，退出码 2（业务）或 1（内部错误）
run_cli <- function(opts, fn) {
  t0 <- Sys.time()
  tryCatch({
    res <- fn(opts)
    res$ok <- TRUE
    json_out(res)
    invisible(0L)
  }, ledger_error = function(e) {
    json_out(list(ok = FALSE, error_code = e$code,
                  error = conditionMessage(e), details = e$details,
                  elapsed_ms = as.numeric(difftime(Sys.time(), t0,
                                                   units = "secs")) * 1000))
    quit(save = "no", status = 2L)
  }, error = function(e) {
    json_out(list(ok = FALSE, error_code = "internal_error",
                  error = conditionMessage(e),
                  elapsed_ms = as.numeric(difftime(Sys.time(), t0,
                                                   units = "secs")) * 1000))
    quit(save = "no", status = 1L)
  })
}

require_db <- function(opts) {
  db <- cli_opt(opts, "db", default_ledger_db())
  if (is.null(db) || is.na(db) || !nzchar(db))
    stop_ledger("missing_db_path", "必须提供 --db 或设置 LEDGER_DB")
  db
}

csv_or_null <- function(opts, name) {
  v <- cli_opt(opts, name)
  if (is.null(v) || isTRUE(v) || !nzchar(v)) NULL else v
}
