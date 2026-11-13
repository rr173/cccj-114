#!/usr/bin/env Rscript
# =============================================================================
# import_batch.R —— 导入一个批次（两类文件可同时提交）
#
# 用法:
#   Rscript R/ledger/import_batch.R --db data/ledger.sqlite --batch-id B001 \
#       --monthly incoming/B001_monthly.csv \
#       --registration incoming/B001_registration.csv
#
# 可选：--source-id（必须与库绑定一致）、--interrupt-at before-receipt
#       （事务内可控测试中断点，仅用于故障注入测试）。
#
# 成功批量写入一个事务：版本行 + 当前指针 + 回执 + 逐条结果。
# 同 (source_id,batch_id) 原文件重传返回原回执（"replay":true），不重复生效。
# =============================================================================

here <- dirname(normalizePath(sub("--file=", "",
          grep("--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = FALSE))
source(file.path(here, "cli.R"), local = FALSE)
load_ledger_lib()

opts <- parse_cli(commandArgs(trailingOnly = TRUE))

run_cli(opts, function(o) {
  db <- require_db(o)
  con <- open_ledger(db)
  on.exit(close_ledger(con), add = TRUE)
  files <- list(monthly = csv_or_null(o, "monthly"),
                registration = csv_or_null(o, "registration"))
  ## CSV 里不允许出现 --xxx=yyy 这类参数被误当文件名
  for (nm in names(files)) {
    v <- files[[nm]]
    if (!is.null(v) && grepl("^--", v))
      stop_ledger("invalid_argument", paste0("--", nm, " 不是文件路径: ", v))
  }
  intr <- cli_opt(o, "interrupt_at")
  if (!is.null(intr) && !isTRUE(intr)) intr <- gsub("-", "_", intr)
  res <- ingest_batch(con, files, list(
    batch_id = cli_opt(o, "batch_id"),
    source_id = cli_opt(o, "source_id"),
    interrupt_at = intr,
    pause_before_commit = cli_num(o, "pause_before_commit"),
    pause_marker = cli_opt(o, "pause_marker")))
  res
})
