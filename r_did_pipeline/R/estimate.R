# =============================================================================
# estimate.R —— Callaway–Sant'Anna 组别-时间 ATT（did::att_gt）
#
# 明确不使用普通双向固定效应（TWFE）系数作为目标估计；TWFE 仅在诊断中作为
# 异质效应偏差的对照展示。不可识别单元格在 did 内部即为 NA，本模块保持 NA
# 并与预飞行原因合并，绝不写入 0。
# =============================================================================

run_att_gt <- function(analytic, idtbl, cfg) {
  library(did)

  # did 2.1.2 仅原生支持单位级（及单位内恒定）聚类变量。
  # 时间聚类改用 att_gt 影响函数按时间索引重新聚合（见下），不传入会被
  # did 拒绝的时变 clustervars；推断方法在结果身份与日志中如实记录。
  cluster_name <- NULL
  est_df <- as.data.frame(analytic)
  time_cluster <- identical(cfg$cluster_level, "time")
  if (!is.null(cfg$cluster_var) && !is.na(cfg$cluster_var) &&
      nzchar(cfg$cluster_var)) {
    if (!cfg$cluster_var %in% names(est_df))
      stop("聚类变量不存在: ", cfg$cluster_var)
    cluster_name <- cfg$cluster_var
    log_msg("聚类层级: 自定义列 ", cfg$cluster_var, "（did bootstrap）")
  } else if (time_cluster) {
    log_msg("聚类层级: 时间（月份）—— 影响函数按时间聚合，非 did 原生 bootstrap")
  } else {
    est_df$.cluster_id <- est_df$region_id
    cluster_name <- ".cluster_id"
    log_msg("聚类层级: 地区（did bootstrap）")
  }

  set.seed(cfg$seed)
  log_msg("调用 did::att_gt | control=", cfg$control_group,
          " | seed=", cfg$seed, " | biters=", cfg$biters,
          " | 协变量=", paste(unlist(cfg$covariates), collapse = "+"))

  # did 2.1.2 对照参数值为无连字符写法；配置层保留需求文档的连字符写法
  did_control <- switch(cfg$control_group,
                        "never-treated"     = "nevertreated",
                        "not-yet-treated"   = "notyettreated")
  att_err <- NULL
  att <- tryCatch(did::att_gt(
    yname        = cfg$col_outcome,
    tname        = "t",
    idname       = "id",
    gname        = "g",
    xformla      = stats::as.formula(paste("~",
                     paste(unlist(cfg$covariates), collapse = " + "))),
    data         = est_df,
    panel        = TRUE,
    control_group = did_control,
    anticipation = 0L,
    bstrap       = !time_cluster,   # 时间聚类走影响函数，关闭 bootstrap
    biters       = cfg$biters,
    cband        = !time_cluster,
    clustervars  = if (time_cluster) NULL else cluster_name,
    est_method   = "reg",
    base_period  = "varying",
    print_details = FALSE,
    cores        = 1L
  ), error = function(e) {
    att_err <<- conditionMessage(e)
    NULL
  })

  # ---- 全部单元格不可识别：不调用/不伪造估计，输出 NA + 原因表 ---------------
  if (is.null(att)) {
    if (nrow(idtbl[identifiable == TRUE]) > 0L)
      stop("存在可识别单元格但 att_gt 失败: ", att_err)
    gt0 <- copy(idtbl)
    gt0[, `:=`(att = NA_real_, se = NA_real_, se_method = NA_character_,
               critical_val_pointwise = NA_real_,
               critical_val_uniform = NA_real_, n_post = NA_integer_,
               ci_pointwise_lower = NA_real_, ci_pointwise_upper = NA_real_,
               ci_uniform_lower = NA_real_, ci_uniform_upper = NA_real_,
               n_obs_cell = NA_integer_, n_distinct_units = NA_integer_)]
    setcolorder(gt0, c("g", "group_month", "t", "month", "event_time",
                       "identifiable", "nonid_reason", "att", "se", "se_method",
                       "ci_pointwise_lower", "ci_pointwise_upper",
                       "ci_uniform_lower", "ci_uniform_upper",
                       "critical_val_pointwise", "critical_val_uniform",
                       "n_post", "n_obs_cell", "n_distinct_units",
                       "n_treated_units", "n_control_units",
                       "control_groups", "n_units"))
    log_msg("无任何可识别单元格：输出不可识别表，att 全部为 NA（不输出零）")
    return(invisible(list(att = NULL, gt = gt0)))
  }

  # ---- 原始组别-时点结果（did 2.1.2：att_gt 仅给点态临界值 c）---------------
  # 时间聚类说明：did 2.1.2 不接受时变 clustervars，且仅暴露按单位聚合的影响
  # 函数，无法在包内重构严格的时间聚类方差。此时保留 did 的单位稳健分析 SE，
  # 并在 se_method 列与结果身份中显式标注，绝不静默宣称时间聚类精度。
  se_method <- if (time_cluster)
    "unit_robust_analytical(did_time_cluster_not_supported_v2.1.2)" else
    "unit_cluster_bootstrap"
  c_point <- if (time_cluster) stats::qnorm(0.975) else unname(att$c)
  did_res <- data.table(
    g              = att$group,
    t              = att$t,
    att            = att$att,
    se             = att$se,
    se_method      = se_method,
    critical_val_pointwise = c_point,
    n_post         = att$n
  )
  did_res[, `:=`(ci_pointwise_lower = att - critical_val_pointwise * se,
                 ci_pointwise_upper = att + critical_val_pointwise * se)]
  # 单元格层不伪造统一临界值；统一带由 aggte(cband=TRUE) 在聚合层给出
  did_res[, `:=`(critical_val_uniform = NA_real_,
                 ci_uniform_lower = NA_real_,
                 ci_uniform_upper = NA_real_)]

  # ---- 以预飞行全网格为基（全部目标单元格），did 结果全外并入 ---------------
  # did 只对可识别单元格返回结果；不可识别单元格保持 NA + 原因，绝不补零。
  base <- copy(idtbl)
  gt <- merge(base, did_res, by = c("g", "t"), all = TRUE)

  # ---- 有效样本量：该 (g,t) 实际参与估计的处理/对照地区数 -------------------
  eff <- analytic[, .(
    n_obs_cell       = .N,
    n_distinct_units = uniqueN(region_id)
  ), by = .(g, t)]
  gt <- merge(gt, eff, by = c("g", "t"), all.x = TRUE)

  # 口径交叉校验：可识别必须有有限估计；不可识别必须保持 NA，任何冲突报错
  finite_on_nonid <- gt[(identifiable == FALSE) & is.finite(att)]
  if (nrow(finite_on_nonid) > 0L)
    stop("预飞行判定不可识别但 did 给出有限值，口径冲突: ",
         paste(finite_on_nonid$g, finite_on_nonid$t, collapse = "/"))
  missing_on_id <- gt[(identifiable == TRUE) & is.na(att)]
  if (nrow(missing_on_id) > 0L)
    stop("预飞行判定可识别但 did 未返回估计: ",
         paste(missing_on_id$g, missing_on_id$t, collapse = "/"))
  zeroed <- gt[(identifiable == FALSE) & !is.na(att) & att == 0]
  if (nrow(zeroed) > 0L)
    stop("不可识别单元格出现零值，违反不输出零的约束")

  setorder(gt, g, t)
  front <- c("g", "group_month", "t", "month", "event_time",
             "identifiable", "nonid_reason", "att", "se", "se_method",
             "ci_pointwise_lower", "ci_pointwise_upper",
             "ci_uniform_lower", "ci_uniform_upper",
             "critical_val_pointwise", "critical_val_uniform",
             "n_post", "n_obs_cell", "n_distinct_units",
             "n_treated_units", "n_control_units", "control_groups", "n_units")
  front <- intersect(front, names(gt))
  setcolorder(gt, c(front, setdiff(names(gt), front)))

  log_msg("att_gt 完成: ", nrow(gt[!is.na(att)]),
          " 个可估计单元格；不可识别 ", nrow(gt[is.na(att)]), " 个（NA + 原因）")
  invisible(list(att = att, gt = gt))
}

# ---- 传统 TWFE 诊断（仅作偏差对照，不替代目标估计）---------------------------
run_twfe_diagnostic <- function(analytic, cfg) {
  covs <- unlist(cfg$covariates)
  fml <- stats::as.formula(paste(
    cfg$col_outcome, "~ treat +", paste(covs, collapse = " + "),
    "+ factor(t) + factor(id)"))
  fit <- stats::lm(fml, data = analytic)
  cf <- stats::coef(summary(fit))
  tw <- cf["treat", , drop = FALSE]
  data.table(
    estimator = "twfe_event_pooled",
    coefficient = "treat",
    estimate = unname(tw[1L, "Estimate"]),
    se       = unname(tw[1L, "Std. Error"]),
    t_value  = unname(tw[1L, "t value"]),
    p_value  = unname(tw[1L, "Pr(>|t|)"]),
    note = "普通双向固定效应，存在异质处理效应时可能有偏；仅作对照，非目标估计"
  )
}
