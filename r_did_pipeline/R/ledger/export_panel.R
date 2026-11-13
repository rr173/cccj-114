#!/usr/bin/env Rscript
# =============================================================================
# export_panel.R —— 在一致读取视图内导出现行分析与来源表
#
# 用法:
#   Rscript R/ledger/export_panel.R --db data/ledger.sqlite --out exports
#
# 产出 <out>/<release_id>/：
#   panel.csv                        region_id,month,y,x1,x2,first_treat_month,treat
#   missing_registration.csv         待补登记清单（地区-月份逐行）
#   missing_registration_summary.csv 按地区的待补汇总
#   provenance.csv                   逐行来源（月报/登记 revision 与批次）
#   manifest.json                    发布清单与逐文件哈希
#
# 写临时目录 -> 重命名发布；失败时调用者此前的导出目录保持不变。
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
  out <- cli_opt(o, "out")
  if (is.null(out) || isTRUE(out) || !nzchar(out))
    stop_ledger("missing_output_dir", "必须提供 --out 导出根目录")
  export_panel(con, list(out_dir = out,
                         release_tag = cli_opt(o, "release_tag")))
})
