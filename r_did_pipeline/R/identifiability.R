# =============================================================================
# identifiability.R —— 组别-时点 ATT 的预飞行可识别性检查
#
# 依据 Callaway & Sant'Anna (2021) 与 did::att_gt 的丢组规则：
#  * never-treated 对照：每个处理组都需要存在永不处理地区；
#  * not-yet-treated 对照：(g,t) 需要存在 g' > t（或 g'=0）的对照地区；
#  * 处理时点之前的期也需要对照组（基础期识别）；
#  * 组内地区数低于 did 阈值 min_group_size 时该组被丢；
#  * 数据中实际不存在的 (g,t) 组合不产生估计目标。
# 不可识别的单元格 identifiable=FALSE 并给出原因；att_gt 对应 NA，绝不补零。
# =============================================================================

build_identifiability <- function(analytic, cfg) {
  min_n <- cfg$min_group_size
  group_size <- analytic[, .(n_units = uniqueN(region_id)), by = g]

  groups <- sort(unique(analytic[g > 0L, g]))
  times  <- sort(unique(analytic$t))
  never_units <- unique(analytic[g == 0L, region_id])

  # 每个时点有哪些“可作对照”的地区组
  grid <- analytic[, .(g = unique(g)), by = t]

  rows <- vector("list", length(groups) * length(times))
  k <- 0L
  for (gg in groups) {
    gsize <- group_size[g == gg, n_units]
    g_units <- unique(analytic[g == gg, region_id])
    # 组级识别：not-yet-treated + varying base period 要求存在严格更晚处理
    # 的队列（g'>g）或永不处理组，否则该组在整个面板内都没有合格对照，
    # did 会整组丢弃。复刻该规则，标记整组不可识别并说明原因。
    has_later <- any(groups > gg)
    group_level_dead <- cfg$control_group == "not-yet-treated" &&
      !has_later && length(never_units) == 0L
    for (tt in times) {
      k <- k + 1L
      observed <- nrow(analytic[g == gg & t == tt]) > 0L
      reason <- NA_character_
      id_ok  <- TRUE
      control_g <- integer(0)
      control_n <- 0L

      if (!observed) {
        id_ok <- FALSE
        reason <- "group_time_cell_not_observed"
      } else if (group_level_dead) {
        id_ok <- FALSE
        reason <- "no_later_cohort_for_not_yet_treated_group"
      } else if (tt == min(times)) {
        # varying base period 下首期无 t-1 可做差，did 不返回该单元格
        id_ok <- FALSE
        reason <- "no_earlier_period_for_varying_base_period"
      } else if (length(gsize) == 0L || gsize < min_n) {
        id_ok <- FALSE
        reason <- sprintf("group_below_min_size(n=%s,min=%s)",
                          ifelse(length(gsize), gsize, 0L), min_n)
      } else {
        if (cfg$control_group == "never-treated") {
          control_g <- 0L
          control_n <- length(never_units)
          if (control_n == 0L) {
            id_ok <- FALSE
            reason <- "no_never_treated_units_available"
          } else if (control_n < min_n) {
            id_ok <- FALSE
            reason <- sprintf("never_treated_below_min_size(n=%s,min=%s)",
                              control_n, min_n)
          }
        } else {
          # not-yet-treated：在 t 期尚未处理的“其他”组（g'=0 或 g'>t），
          # 严格排除当前处理组自身
          avail <- unique(analytic[t == tt & (g == 0L | g > tt) & g != gg, g])
          control_g <- sort(avail)
          control_n <- uniqueN(analytic[t == tt & g %in% control_g, region_id])
          if (length(control_g) == 0L) {
            id_ok <- FALSE
            reason <- "not_yet_treated_controls_exhausted"
          } else if (control_n < min_n) {
            id_ok <- FALSE
            reason <- sprintf("control_pool_below_min_size(n=%s,min=%s)",
                              control_n, min_n)
          }
        }
      }

      rows[[k]] <- data.table(
        g = gg, t = tt,
        event_time = tt - gg,
        identifiable = id_ok,
        nonid_reason = reason,
        n_treated_units = length(g_units),
        n_control_units = control_n,
        control_groups = if (length(control_g))
          paste(control_g, collapse = ";") else NA_character_
      )
    }
  }
  idtbl <- rbindlist(rows)
  idtbl <- merge(idtbl, group_size, by = "g", all.x = TRUE)

  # 时间索引 -> 实际月份，便于审查
  tm <- analytic[, .(t, month = as.character(month))]
  tm <- unique(tm)
  idtbl <- merge(idtbl, tm, by = "t", all.x = TRUE)
  cm <- analytic[g > 0L, .(g, group_month = as.character(unique(first_treat))),
                 by = g]
  idtbl <- merge(idtbl, unique(analytic[, .(g,
                  group_month = ifelse(is.infinite(first_treat), "never",
                                       as.character(first_treat)))]),
                 by = "g", all.x = TRUE)
  setorder(idtbl, g, t)
  setcolorder(idtbl, c("g", "group_month", "t", "month", "event_time",
                       "identifiable", "nonid_reason", "n_units",
                       "n_treated_units", "n_control_units",
                       "control_groups"))

  n_bad <- nrow(idtbl[identifiable == FALSE])
  log_msg("可识别性检查: ", nrow(idtbl), " 个组别-时点单元格, ",
          n_bad, " 个不可识别（将记录原因，不输出零）")
  if (n_bad > 0L)
    log_msg("不可识别原因分布: ",
            paste(names(table(idtbl$nonid_reason)),
                  as.integer(table(idtbl$nonid_reason)),
                  sep = "=", collapse = " | "))
  invisible(idtbl)
}
