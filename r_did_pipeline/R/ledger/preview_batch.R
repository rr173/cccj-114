#!/usr/bin/env Rscript
# =============================================================================
# preview_batch.R —— 批次预演：生成影响报告但不提交数据
#
# 用法:
#   Rscript R/ledger/preview_batch.R --db data/ledger.sqlite --batch-id D09 \
#       --monthly incoming/d09_monthly.csv \
#       --registration incoming/d09_registration.csv \
#       --report reports/d09_preview.json
#
# 与正式导入使用完全相同的文件校验、批内冲突判断、修订比较与重放规则；
# 报告写到 --report 指定的位置（全部生成成功后原子发布，失败不覆盖已有报告）。
# 预演不在账本中留下任何痕迹，同一 batch_id 之后仍可正式导入。
#
# stdout 输出一行 JSON 摘要；退出码：
#   0 预演完成并已发布报告（verdict = would_commit / replay / would_reject）
#   2 预演本身被拒绝（如 batch_id 已有不同内容的成功回执、参数或库问题）
#   1 内部错误
# 测试钩子（仅用于并发演练）：--pause-inside-tx 秒 --pause-marker 文件
# =============================================================================

here <- dirname(normalizePath(sub("--file=", "",
          grep("--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = FALSE))
source(file.path(here, "cli.R"), local = FALSE)
load_ledger_lib(c("core.R", "validate.R", "ingest.R", "export.R", "query.R",
                  "preview.R"))

opts <- parse_cli(commandArgs(trailingOnly = TRUE))

run_cli(opts, function(o) {
  db <- require_db(o)
  con <- open_ledger(db)
  on.exit(close_ledger(con), add = TRUE)
  files <- list(monthly = csv_or_null(o, "monthly"),
                registration = csv_or_null(o, "registration"))
  for (nm in names(files)) {
    v <- files[[nm]]
    if (!is.null(v) && grepl("^--", v))
      stop_ledger("invalid_argument", paste0("--", nm, " 不是文件路径: ", v))
  }
  report_path <- cli_opt(o, "report")
  if (is.null(report_path) || isTRUE(report_path) || !nzchar(report_path))
    stop_ledger("missing_report_path", "必须提供 --report 报告输出路径")
  report <- preview_batch(con, files, list(
    batch_id = cli_opt(o, "batch_id"),
    source_id = cli_opt(o, "source_id"),
    pause_inside_tx = cli_num(o, "pause_inside_tx"),
    pause_marker = cli_opt(o, "pause_marker")))
  published <- write_preview_report(report, report_path)
  preview_report_summary(report, published)
})
