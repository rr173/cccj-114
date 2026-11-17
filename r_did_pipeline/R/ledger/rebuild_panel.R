#!/usr/bin/env Rscript
# =============================================================================
# rebuild_panel.R —— 按已提交导入批次边界重建分析面板（只读，时间旅行）
#
# 用法:
#   Rscript R/ledger/rebuild_panel.R --db data/ledger.sqlite \
#       --batch-id D03 --out rebuilds
#
# 产出 <out>/rebuild-seq<NNNNNNNNNN>-<batch_id>/：
#   panel.csv                        与普通导出同形同规则的地区月表
#   missing_registration.csv         待补登记清单（地区-月份逐行）
#   missing_registration_summary.csv 按地区的待补汇总
#   provenance.csv                   逐行来源（月报/登记 revision 与批次）
#   manifest.json                    边界身份：batch_id、提交顺序、
#                                    账本/来源身份、基线信息、文件校验值
#
# 边界规则：
#   * --batch-id 必须是当前账本内一次成功提交的导入批次，或保留基线标识
#     "__baseline__"；未知批次、失败回执、升级基线以前的旧 batch_id 一律
#     以退出码 2 明确拒绝（JSON 带 error_code）；
#   * 全部读取在单个只读快照内完成，边界之后的更正不会泄漏；
#   * 纯只读：不改写当前指针、修订历史、回执或提交边界，也不登记
#     export_release；历史输出不会成为后续普通导出的当前状态；
#   * 结果目录确定性命名；目录已存在或写出失败时不覆盖任何已有结果。
# =============================================================================

here <- dirname(normalizePath(sub("--file=", "",
          grep("--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = FALSE))
source(file.path(here, "cli.R"), local = FALSE)
load_ledger_lib(c("core.R", "validate.R", "ingest.R", "export.R", "query.R",
                  "boundary.R", "rebuild.R"))

opts <- parse_cli(commandArgs(trailingOnly = TRUE))

run_cli(opts, function(o) {
  db <- require_db(o)
  con <- open_ledger(db)
  on.exit(close_ledger(con), add = TRUE)
  out <- cli_opt(o, "out")
  if (is.null(out) || isTRUE(out) || !nzchar(out))
    stop_ledger("missing_output_dir", "必须提供 --out 重建结果根目录")
  rebuild_panel(con, list(out_dir = out, batch_id = cli_opt(o, "batch_id")))
})
