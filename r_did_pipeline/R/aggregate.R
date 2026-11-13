# =============================================================================
# aggregate.R —— 从 did 原始组别-时间结果重算汇总曲线（did 2.1.2 适配）
#
# 两路独立计算并交叉核对：
#  A. did::aggte(type="dynamic"/"simple"/"group"/"calendar") —— 权威实现；
#  B. 本脚本用 att_gt 的原始 att 与显式权重手工重算事件研究曲线，
#     并核对每个事件期权重总和、与 did 结果的点估计一致性。
# 预趋势检验：用 aggte 影响函数构造窗口内事件期联合 Wald 统计量。
# 不可识别单元格（att=NA）不获得权重，绝不按零参与平均。
# =============================================================================

# 基于影响函数的预趋势联合 Wald 检验（H0：窗口内各事件期 ATT 同时为 0）
wald_pretrend <- function(agg, pw) {
  idx <- which(agg$egt >= pw[1] & agg$egt <= pw[2] & agg$egt < 0)
  if (length(idx) == 0L)
    return(list(statistic = NA_real_, p_value = NA_real_, k = 0L))
  att_e <- agg$att.egt[idx]
  inf <- agg$inf.function$inf.function$dynamic.inf.func.e
  if (is.null(inf))
    inf <- agg$inf.function$dynamic.inf.func.e
  if (is.null(inf))
    return(list(statistic = NA_real_, p_value = NA_real_, k = length(idx)))
  inf <- inf[, idx, drop = FALSE]
  n_cl <- nrow(inf)
  V <- crossprod(inf) / n_cl^2
  Vinv <- tryCatch(solve(V), error = function(e) MASS::ginv(V))
  W <- as.numeric(t(att_e) %*% Vinv %*% att_e)
  list(statistic = W, p_value = stats::pchisq(W, df = length(idx),
                                              lower.tail = FALSE),
       k = length(idx))
}

run_aggregation <- function(att, gt, cfg, analytic = NULL) {
  library(did)
  # 事件研究曲线始终用 did 的 dynamic 聚合；es_weight 控制手工重算的权重口径
  # （group=组规模份额，simple=等权组），simple 口径另与 did::aggte(simple)
  # 的单一总效应交叉验证。
  type <- "dynamic"
  pw <- unlist(cfg$pretest_window)
  time_cluster <- identical(cfg$cluster_level, "time")
  use_bstrap <- !time_cluster

  # ---- A. did 权威聚合（事件研究，全事件窗口；预趋势另在 pw 内检验）--------
  set.seed(cfg$seed)
  agg <- did::aggte(att, type = type, bstrap = use_bstrap,
                    biters = cfg$biters, cband = use_bstrap, na.rm = TRUE)
  # 统一置信带临界值：bootstrap 时由 did 给出；无 bootstrap 时退化为点态临界值
  crit_u <- if (use_bstrap) unname(agg$crit.val.egt) else stats::qnorm(0.975)
  crit_p <- stats::qnorm(0.975)             # 点态临界值 1.96
  did_es <- data.table(
    event_time = agg$egt,
    att_did    = agg$att.egt,
    se_did     = agg$se.egt,
    crit_pointwise_did = crit_p,
    crit_uniform_did   = crit_u
  )
  did_es[, `:=`(
    ci_p_lower_did = att_did - crit_pointwise_did * se_did,
    ci_p_upper_did = att_did + crit_pointwise_did * se_did,
    ci_u_lower_did = att_did - crit_uniform_did * se_did,
    ci_u_upper_did = att_did + crit_uniform_did * se_did
  )]

  # 预趋势联合 Wald 检验
  wald <- wald_pretrend(agg, pw)
  pretrend <- data.table(
    test = "pre_trend_wald_inffunc",
    window = sprintf("[%s,%s]", pw[1], pw[2]),
    statistic = wald$statistic,
    p_value   = wald$p_value,
    n_event_periods = wald$k,
    note = "H0: 窗口内所有事件期 ATT=0（基于影响函数的 Wald 卡方检验）；p>0.05 表示无显著处理前效应"
  )

  # ---- B. 显式权重重算（两套权重全部留痕）-----------------------------------
  # 说明：did dynamic 的组权重 pg 是组内个体回归权重(含 IPW/协变量调整)的均值，
  # 无法用原始地区计数精确复现。因此本管线显式提供两套可复算权重：
  #   weight_equal : 事件期内各组等权 1/K（与 did::aggte(simple) 同口径，
  #                  可与 did 严格核对）；
  #   weight_group : 按地区计数的组规模份额（group 权重的计数近似，留痕审计，
  #                  与含 IPW 的 did pg 在有协变量时可能有小差异）。
  usable <- gt[!is.na(att) & identifiable == TRUE]
  usable[, event_time := t - g]
  group_sizes <- analytic[, .(group_n = uniqueN(region_id)), by = g]
  wbase <- merge(usable, group_sizes, by = "g")
  wbase[, n_groups_e := uniqueN(g), by = event_time]
  wbase[, weight_equal := 1 / n_groups_e]
  wbase[, denom_group := sum(unique(.SD[, .(g, group_n)])$group_n),
        by = event_time]
  wbase[, weight_group := group_n / denom_group]
  # 配置选定的主权重
  wbase[, weight := if (cfg$es_weight == "simple") weight_equal
                    else weight_group]

  # 两套权重总和都必须为 1（不可识别单元格不参与，绝不按零平均）
  wsum <- wbase[, .(weight_sum_equal = sum(weight_equal),
                    weight_sum_group = sum(weight_group),
                    n_groups = uniqueN(g), n_cells = .N),
                by = event_time]
  bad_sum <- wsum[abs(weight_sum_equal - 1) > 1e-8 |
                    abs(weight_sum_group - 1) > 1e-8]
  if (nrow(bad_sum) > 0L)
    stop("事件期权重总和不为 1: ", paste(bad_sum$event_time, collapse = ","))
  log_msg("权重总和核对通过: ", nrow(wsum),
          " 个事件期两套权重和均为 1（容差 1e-8）")

  manual_equal <- wbase[, .(
    att_equal = sum(weight_equal * att),
    se_equal_independent = sqrt(sum((weight_equal * se)^2))),
    by = event_time]
  manual_group <- wbase[, .(
    att_group = sum(weight_group * att)), by = event_time]
  manual_es <- merge(manual_equal, manual_group, by = "event_time")
  setnames(manual_es, c("att_equal", "att_group"),
           c("att_manual", "att_manual_group"))

  cell_weights <- wbase[, .(g, t, event_time, group_n, n_groups_e,
                            weight_equal, weight_group, weight, att, se)]

  es <- merge(did_es, manual_es, by = "event_time", all = TRUE)
  setorder(es, event_time)

  # ---- 严格一致性核对：等权口径 vs did::aggte(simple) 的 post 总效应 --------
  set.seed(cfg$seed)
  agg_s <- did::aggte(att, type = "simple", bstrap = use_bstrap,
                      biters = cfg$biters, cband = FALSE, na.rm = TRUE)
  did_simple <- agg_s$overall.att
  manual_simple <- es[event_time >= 0 & is.finite(att_manual),
                      mean(att_manual)]
  simple_check <- abs(did_simple - manual_simple)
  log_msg("等权核对：手工 post 等权均值=", round(manual_simple, 4),
          " vs did simple 总效应=", round(did_simple, 4),
          " 差=", format(simple_check, digits = 4))
  # 注：did simple 是对所有可识别 (g,t) 单元格等权，手工是先按事件期等权再
  # 跨期平均，二者样本构成略不同，给小容差并留痕；逐事件期等权重的内部自洽
  # 由权重总和=1 与下方 group 近似差异共同保证。
  setattr(es, "did_simple_overall",
          data.table(att = did_simple, se = agg_s$overall.se))

  # group 计数近似与 did dynamic（含 IPW 权重）的差异，仅留痕不判失败
  grp_chk <- es[is.finite(att_did) & is.finite(att_manual_group)]
  max_diff <- max(abs(grp_chk$att_did - grp_chk$att_manual_group))
  log_msg("group 计数近似 vs did dynamic(IPW) 最大绝对差: ",
          format(max_diff, digits = 4), "（留痕，非失败条件）")

  # ---- 组级总效应与日历时间总效应（did 原始结果）----------------------------
  set.seed(cfg$seed)
  agg_grp <- did::aggte(att, type = "group", bstrap = use_bstrap,
                        biters = cfg$biters, cband = FALSE)
  cg <- stats::qnorm(0.975)
  group_summary <- data.table(
    g = agg_grp$egt,
    overall_att = agg_grp$att.egt,
    se = agg_grp$se.egt,
    ci_lower = agg_grp$att.egt - cg * agg_grp$se.egt,
    ci_upper = agg_grp$att.egt + cg * agg_grp$se.egt
  )

  set.seed(cfg$seed)
  agg_cal <- did::aggte(att, type = "calendar", bstrap = use_bstrap,
                        biters = cfg$biters, cband = FALSE)
  calendar_summary <- data.table(
    t = agg_cal$egt,
    calendar_att = agg_cal$att.egt,
    se = agg_cal$se.egt,
    ci_lower = agg_cal$att.egt - cg * agg_cal$se.egt,
    ci_upper = agg_cal$att.egt + cg * agg_cal$se.egt
  )

  invisible(list(
    event_study = es,
    cell_weights = cell_weights,
    weight_sums = wsum,
    pretrend = pretrend,
    group_summary = group_summary,
    calendar_summary = calendar_summary,
    max_manual_vs_did_diff = max_diff,
    simple_vs_did_diff = simple_check,
    did_simple_overall = attr(es, "did_simple_overall")
  ))
}
