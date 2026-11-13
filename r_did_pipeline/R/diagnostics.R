# =============================================================================
# diagnostics.R —— 可审查诊断输出（静态图与诊断表，不做仪表盘）
# =============================================================================

make_plots <- function(gt, es_obj, out_dir, version_id, cfg) {
  library(ggplot2)
  es <- es_obj$event_study
  pretrend <- es_obj$pretrend

  # ---- 图 1：事件研究汇总曲线（点态 + 统一置信带）---------------------------
  # 图形标签使用英文，避免运行环境缺少 CJK 字体时出现方框；数据产物仍为中文。
  p1 <- ggplot(es, aes(event_time, att_did)) +
    geom_ribbon(aes(ymin = ci_u_lower_did, ymax = ci_u_upper_did),
                fill = "steelblue", alpha = 0.18) +
    geom_errorbar(aes(ymin = ci_p_lower_did, ymax = ci_p_upper_did),
                  width = 0.15, colour = "grey30") +
    geom_point(colour = "steelblue", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    geom_vline(xintercept = -0.5, linetype = "dotted", colour = "grey40") +
    labs(title = paste0("Event-study ATT (", cfg$es_weight, " weights)"),
         subtitle = paste0(version_id, " | control: ", cfg$control_group,
                           " | pre-trend p=", round(pretrend$p_value[1], 4)),
         x = "Event time (relative to first full-exposure month)",
         y = "Aggregated ATT",
         caption = "Shade=simultaneous band; bars=pointwise 95% CI; NA cells excluded") +
    theme_bw(base_size = 11)
  ggsave(file.path(out_dir, "fig_event_study.png"), p1,
         width = 9, height = 5.5, dpi = 150)

  # ---- 图 2：组别-时间 ATT 热图（NA 显式灰色，绝不画成零）-------------------
  heat <- copy(gt)
  heat[, label := ifelse(is.na(att), "NA", sprintf("%.2f", att))]
  p2 <- ggplot(heat, aes(factor(t), factor(g))) +
    geom_tile(aes(fill = ifelse(is.na(att), NA_real_, att)),
              colour = "white") +
    geom_text(aes(label = label), size = 3) +
    scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                         midpoint = 0, na.value = "grey85",
                         name = "ATT") +
    labs(title = "Group-time ATT heatmap (grey = not identifiable, kept NA)",
         x = "Time index t", y = "Treatment cohort g (0 = never-treated)") +
    theme_bw(base_size = 11)
  ggsave(file.path(out_dir, "fig_group_time_heatmap.png"), p2,
         width = 9, height = 6, dpi = 150)

  # ---- 图 3：事件期权重柱状图（两套权重总和都应=1）--------------------------
  wlong <- rbind(
    es_obj$weight_sums[, .(event_time, weight_sum = weight_sum_equal,
                           scheme = "equal")],
    es_obj$weight_sums[, .(event_time, weight_sum = weight_sum_group,
                           scheme = "group")])
  wsum_plot <- ggplot(wlong, aes(factor(event_time), weight_sum,
                                 fill = scheme)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.7) +
    geom_hline(yintercept = 1, colour = "red", linetype = "dashed") +
    coord_cartesian(ylim = c(0, 1.1)) +
    scale_fill_manual(values = c("equal" = "steelblue",
                                 "group" = "darkorange")) +
    labs(title = "Event-time weight sums (both schemes must equal 1)",
         x = "Event time", y = "Weight sum", fill = "Weight scheme") +
    theme_bw(base_size = 11)
  ggsave(file.path(out_dir, "fig_weight_sums.png"), wsum_plot,
         width = 9, height = 4.5, dpi = 150)

  log_msg("诊断图已写出: fig_event_study.png / fig_group_time_heatmap.png / ",
          "fig_weight_sums.png")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

build_diagnostics <- function(valid, gt, es_obj, twfe, idtbl, out_dir,
                              version_id, cfg) {
  make_plots(gt, es_obj, out_dir, version_id, cfg)

  diag <- list(
    version_id = version_id,
    panel = valid$meta,
    identifiability_summary = idtbl[, .(
      n_cells = .N,
      n_identifiable = sum(identifiable),
      n_unidentifiable = sum(!identifiable)), ],
    nonid_reasons = idtbl[identifiable == FALSE, .N, by = nonid_reason],
    pretrend = es_obj$pretrend,
    twfe_comparison = twfe,
    target_vs_twfe = data.table(
      twfe_estimate = twfe$estimate,
      event_study_post_mean = es_obj$event_study[event_time >= 0,
                                                  mean(att_did, na.rm = TRUE)],
      note = "二者差异即异质效应下 TWFE 偏差的描述性证据；目标估计以 CS 为准"
    ),
    aggregation_check = data.table(
      es_weight = cfg$es_weight,
      max_manual_vs_did_abs_diff = es_obj$max_manual_vs_did_diff,
      simple_vs_did_overall_diff = es_obj$simple_vs_did_diff,
      weight_sum_tolerance = 1e-8,
      all_weight_sums_equal_one = all(
        abs(es_obj$weight_sums$weight_sum_equal - 1) <= 1e-8) &&
        all(abs(es_obj$weight_sums$weight_sum_group - 1) <= 1e-8)
    )
  )
  jsonlite::write_json(diag, file.path(out_dir, "diagnostics.json"),
                       pretty = TRUE, auto_unbox = TRUE, null = "null")
  log_msg("诊断清单写出: diagnostics.json")
  invisible(diag)
}
