#!/usr/bin/env Rscript
# =============================================================================
# run_pipeline.R —— 端到端可重复管线
#
# 用法:
#   Rscript R/run_pipeline.R config/analysis.json
#
# 输出（每个分析版本独立目录 output/<version_id>/）:
#   identity.json             结果身份（快照/配置哈希、窗口、对照策略、种子、
#                             预检验窗口、聚合权重、聚类层级、R/did 版本）
#   group_time_att.csv        组别-时间 ATT、SE、点态/统一 CI、样本量、对照
#   event_study.csv           汇总曲线（did 与手工重算双列）
#   event_study_weights.csv   事件时间逐单元格权重
#   group_summary.csv / calendar_summary.csv
#   pretrend_test.csv         预处理检验
#   twfe_diagnostic.csv       TWFE 对照（非目标估计）
#   identifiability.csv       全部组别-时点可识别性与不可识别原因
#   excluded_regions.csv      排除地区及原因
#   missing_months.csv        缺月/缺值登记（未填充证据）
#   corrections_log.csv       处理时间更正留痕
#   diagnostics.json          诊断汇总
#   fig_*.png                 静态图（无仪表盘）
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L)
  stop("用法: Rscript R/run_pipeline.R <config.json> [--tag TAG]")
cfg_path <- args[1]
tag <- if (length(args) >= 3L && args[2] == "--tag") args[3] else NULL

here <- dirname(normalizePath(sub("--file=", "",
             grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
if (is.na(here) || !dir.exists(here)) here <- "R"
invisible(lapply(c("utils.R", "validate_data.R", "identifiability.R",
                   "estimate.R", "aggregate.R", "diagnostics.R"),
                 function(f) source(file.path(here, f))))

cfg <- load_config(cfg_path)
valid <- load_or_validate(cfg)

identity <- build_identity(cfg, cfg$data_path, valid$meta,
                           valid$meta$n_rows_analytic)
version_id <- if (!is.null(tag)) paste0(identity$version_id, "_", tag) else
  identity$version_id
out_dir <- ensure_dir(cfg$output_dir, version_id)
init_log(file.path("logs", paste0(version_id, ".log")))
log_msg("==== 管线开始 | 版本 ", version_id, " ====")
log_msg("结果身份: ", jsonlite::toJSON(identity$hash_inputs, auto_unbox = TRUE))

idtbl <- build_identifiability(valid$analytic, cfg)
est <- run_att_gt(valid$analytic, idtbl, cfg)
gt <- est$gt
if (is.null(est$att)) {
  # 无任何可识别单元格：只落盘不可识别表与诊断，不产生伪估计/伪聚合
  log_msg("跳过聚合：不存在可识别单元格（对照策略无合法对照）")
  es_obj <- list(
    event_study = data.table(), cell_weights = data.table(),
    weight_sums = data.table(),
    pretrend = data.table(test = "pre_trend_wald", window = NA_character_,
                          statistic = NA_real_, p_value = NA_real_,
                          n_event_periods = 0L,
                          note = "不可识别，未执行检验"),
    group_summary = data.table(), calendar_summary = data.table(),
    max_manual_vs_did_diff = NA_real_)
} else {
  es_obj <- run_aggregation(est$att, gt, cfg, valid$analytic)
}
twfe <- run_twfe_diagnostic(valid$analytic, cfg)

# ---- 落盘 -------------------------------------------------------------------
safe_csv <- function(dt, path) {
  if (is.null(dt) || nrow(dt) == 0L) {
    log_msg("跳过空结果: ", path)
  } else write_csv(dt, path)
}
# 分析面板也落盘，便于审查缺月未填充与排除规则
write_csv(valid$analytic, file.path(out_dir, "analytic_panel.csv"))
write_csv(gt, file.path(out_dir, "group_time_att.csv"))
safe_csv(es_obj$event_study, file.path(out_dir, "event_study.csv"))
safe_csv(es_obj$cell_weights, file.path(out_dir, "event_study_weights.csv"))
safe_csv(es_obj$weight_sums, file.path(out_dir, "weight_sums.csv"))
safe_csv(es_obj$group_summary, file.path(out_dir, "group_summary.csv"))
safe_csv(es_obj$calendar_summary, file.path(out_dir, "calendar_summary.csv"))
safe_csv(es_obj$pretrend, file.path(out_dir, "pretrend_test.csv"))
write_csv(twfe, file.path(out_dir, "twfe_diagnostic.csv"))
write_csv(idtbl, file.path(out_dir, "identifiability.csv"))
write_csv(valid$excluded_regions, file.path(out_dir, "excluded_regions.csv"))
write_csv(valid$missing_register, file.path(out_dir, "missing_months.csv"))
if (nrow(valid$corrections_log) > 0L)
  write_csv(valid$corrections_log, file.path(out_dir, "corrections_log.csv"))
write_csv(valid$time_map, file.path(out_dir, "time_map.csv"))
write_csv(valid$cohort_map, file.path(out_dir, "cohort_map.csv"))

if (!is.null(est$att)) {
  diag <- build_diagnostics(valid, gt, es_obj, twfe, idtbl, out_dir,
                            version_id, cfg)
} else {
  jsonlite::write_json(list(version_id = version_id, panel = valid$meta,
    identifiability_summary = idtbl[, .(n_cells = .N,
      n_identifiable = sum(identifiable),
      n_unidentifiable = sum(!identifiable))],
    nonid_reasons = idtbl[identifiable == FALSE, .N, by = nonid_reason],
    note = "无合法对照组，未生成估计与图形"),
    file.path(out_dir, "diagnostics.json"),
    pretty = TRUE, auto_unbox = TRUE, null = "null")
  log_msg("写出降级 diagnostics.json（无可识别单元格）")
}

identity_out <- c(identity, list(
  tag = tag,
  config_path = normalizePath(cfg_path, mustWork = FALSE),
  output_dir = out_dir,
  files = list(
    group_time_att = "group_time_att.csv",
    event_study = "event_study.csv",
    event_study_weights = "event_study_weights.csv",
    pretrend = "pretrend_test.csv",
    identifiability = "identifiability.csv",
    excluded_regions = "excluded_regions.csv",
    missing_months = "missing_months.csv"
  )
))
jsonlite::write_json(identity_out, file.path(out_dir, "identity.json"),
                     pretty = TRUE, auto_unbox = TRUE, null = "null")
# 固化实际生效配置
jsonlite::write_json(cfg, file.path(out_dir, "config_effective.json"),
                     pretty = TRUE, auto_unbox = TRUE, null = "null")
log_msg("==== 管线完成 | 版本目录 ", out_dir, " ====")
close_log()
