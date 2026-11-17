#!/usr/bin/env Rscript
# =============================================================================
# verify_coverage.R —— 覆盖核验：用覆盖约定对照账本当前状态，输出缺口与例外
#
# 用法:
#   Rscript R/ledger/verify_coverage.R --db data/ledger.sqlite \
#       --coverage coverage/2024H1.csv --from 2024-01 --to 2024-06 \
#       --report reports/coverage_2024H1.json
#
# 覆盖约定文件（CSV，UTF-8，首行表头，列顺序不限）：
#   region_id,start_month,end_month,status,reason
#   * status = expected（应报）或 exempt（豁免，必须带非空 reason）；
#   * 闭区间 [start_month, end_month]（YYYY-MM），不倒置；
#   * 同一地区的区间不能重叠；格式错误/月份无效/区间倒置/相互矛盾时
#     核验整体失败（退出码 2），并指出问题行，不写报告。
#
# 只处理与 [--from, --to] 相交的约定（区间按范围裁切）；未被任何约定覆盖的
# 月份作为“约定缺失”单独报告，不默认为豁免。核验在单个只读事务（固定快照）
# 内读取账本，不新建账本记录、回执或导出发布记录；报告全部生成成功后原子
# 发布到 --report 指定位置，失败不覆盖已有报告。
#
# stdout 输出一行 JSON 摘要；退出码：
#   0 核验完成，无缺口也无待处理项（verdict = clean）
#   3 核验完成，有待处理项（verdict = findings；报告已完整发布）
#   2 输入错误或核验被拒绝（约定文件非法、范围非法、库问题等；不写报告）
#   1 内部错误
# 测试钩子（仅并发演练用）：--pause-inside-snapshot 秒 --pause-marker 文件
# =============================================================================

here <- dirname(normalizePath(sub("--file=", "",
          grep("--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = FALSE))
source(file.path(here, "cli.R"), local = FALSE)
load_ledger_lib(c("core.R", "validate.R", "export.R", "coverage.R"))

opts <- parse_cli(commandArgs(trailingOnly = TRUE))

run_cli(opts, function(o) {
  db <- require_db(o)
  con <- open_ledger(db)
  on.exit(close_ledger(con), add = TRUE)
  cov_path <- cli_opt(o, "coverage")
  if (is.null(cov_path) || isTRUE(cov_path) || !nzchar(cov_path))
    stop_ledger("missing_coverage_path", "必须提供 --coverage 覆盖约定文件")
  if (grepl("^--", cov_path))
    stop_ledger("invalid_argument", paste0("--coverage 不是文件路径: ",
                                           cov_path))
  report_path <- cli_opt(o, "report")
  if (is.null(report_path) || isTRUE(report_path) || !nzchar(report_path))
    stop_ledger("missing_report_path", "必须提供 --report 报告输出路径")
  report <- verify_coverage(con, cov_path,
    from = cli_opt(o, "from"), to = cli_opt(o, "to"),
    opts = list(pause_inside_snapshot = cli_num(o, "pause_inside_snapshot"),
                pause_marker = cli_opt(o, "pause_marker")))
  published <- write_coverage_report(report, report_path)
  coverage_report_summary(report, published)
}, exit_status = function(res)
  if (identical(res$verdict, "findings")) 3L else 0L)
