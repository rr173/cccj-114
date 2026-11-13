# =============================================================================
# validate_data.R —— 输入快照 -> 校验 -> 分析面板
#
# 明确规则：
#  * 缺月只登记、绝不向前/向后填充成虚构观测；
#  * 处理状态 treat 必须与首次暴露月 first_treat_month 一致，冲突地区隔离；
#  * 协变量在地区所有行完全缺失 -> 按预设规则排除并列明；
#  * 处理时间更正文件可把地区改到其他队列（含永不处理组），改动全部留痕；
#  * 结果 y 缺失的行不进入分析样本，但不作为缺月填充。
# =============================================================================

load_or_validate <- function(cfg) {
  log_msg("读取输入快照: ", cfg$data_path)
  dt <- data.table::fread(cfg$data_path, na.strings = c("", "NA", "NaN"))

  required <- c(cfg$col_unit, cfg$col_time, cfg$col_outcome,
                cfg$col_treat, cfg$col_first_treat)
  miss_cols <- setdiff(required, names(dt))
  if (length(miss_cols) > 0L)
    stop("快照缺少必需列: ", paste(miss_cols, collapse = ", "))
  # 若快照自带 t/g/id 等保留列名，先改名避免与管线内部索引冲突
  for (reserved in c("t", "g", "id"))
    if (reserved %in% names(dt))
      setnames(dt, reserved, paste0("raw_", reserved))
  covs <- unlist(cfg$covariates)
  miss_covs <- setdiff(covs, names(dt))
  if (length(miss_covs) > 0L)
    stop("配置声明的协变量列不存在: ", paste(miss_covs, collapse = ", "))

  setnames(dt, old = required,
           new = c("region_id", "month_raw", "y", "treat", "first_treat_raw"))

  # ---- 时间字段：首次完整暴露月 ---------------------------------------------
  dt[, month := parse_month(month_raw)]
  if (anyNA(dt$month)) stop("存在无法解析的月份值")

  never_flag <- rep(FALSE, nrow(dt))
  if (!is.null(cfg$never_treat_code) && !is.na(cfg$never_treat_code))
    never_flag <- as.character(dt$first_treat_raw) == cfg$never_treat_code
  dt[, is_never := never_flag]
  dt[, first_treat := as.Date(rep(NA_character_, .N)), by = region_id]
  dt[, first_treat := {
    v <- first_treat
    if (any(!is_never)) v[!is_never] <- parse_month(first_treat_raw[!is_never])
    v
  }, by = .(region_id)]
  # 永不处理行 / 纯永不处理组：first_treat = Inf（did 队列约定 g=0）
  all_never_regions <- dt[, .(all_never = all(is_never)), by = region_id][
    all_never == TRUE, region_id]
  dt[(is_never) | (region_id %in% all_never_regions), first_treat := as.Date(Inf)]
  dt[, is_never := NULL]
  if (anyNA(dt$first_treat))
    stop("存在既非合法月份又非 never 编码的首次处理月")

  # ---- 处理时间更正（留痕：每个改动一行）------------------------------------
  corrections_log <- data.table()
  has_correction <- !is.null(cfg$correction_path) &&
    length(cfg$correction_path) == 1L && !is.na(cfg$correction_path) &&
    nzchar(cfg$correction_path)
  if (has_correction) {
    log_msg("应用处理时间更正文件: ", cfg$correction_path)
    corr <- data.table::fread(cfg$correction_path,
                              na.strings = c("", "NA", "NaN"))
    stopifnot(all(c("region_id", "correct_first_treat_month") %in% names(corr)))
    c_never <- rep(FALSE, nrow(corr))
    if (!is.null(cfg$never_treat_code) && !is.na(cfg$never_treat_code))
      c_never <- as.character(corr$correct_first_treat_month) == cfg$never_treat_code
    corr[, correct_ft := as.Date(NA)]
    if (any(!c_never))
      corr[!c_never, correct_ft := parse_month(correct_first_treat_month[!c_never])]
    corr[c_never, correct_ft := as.Date(Inf)]

    before <- unique(dt[, .(region_id, old_ft = first_treat)])
    corr <- merge(corr, before, by = "region_id", all.x = TRUE)
    unknown <- corr[is.na(old_ft)]
    if (nrow(unknown) > 0L)
      stop("更正文件引用了快照中不存在的地区: ",
           paste(unique(unknown$region_id), collapse = ","))
    corrections_log <- corr[, .(region_id,
                                old_first_treat = old_ft,
                                new_first_treat = correct_ft)]
    ft_lookup <- corr[, .(region_id, correct_ft)]
    dt <- merge(dt, ft_lookup, by = "region_id", all.x = TRUE,
                sort = FALSE)
    dt[!is.na(correct_ft), first_treat := correct_ft]
    dt[, correct_ft := NULL]
    if (anyNA(dt$first_treat))
      stop("处理时间更正后存在缺失的首次处理月")
    log_msg("处理时间更正地区数: ", nrow(corr))
  }

  # ---- 主键唯一性 -----------------------------------------------------------
  dup <- dt[, .N, by = .(region_id, month)][N > 1L]
  if (nrow(dup) > 0L)
    stop("存在重复的地区-月份记录: ",
         paste(dup$region_id[1:min(5, nrow(dup))], collapse = ","))

  # ---- 规则 1：地区内首次处理月必须恒定 --------------------------------------
  coh_check <- dt[, .(n_ft = uniqueN(first_treat)), by = region_id][n_ft > 1L]
  if (nrow(coh_check) > 0L)
    stop("地区内首次处理月不恒定（更正后仍冲突）: ",
         paste(coh_check$region_id, collapse = ","))

  # ---- 规则 2：treat 与首次暴露月交叉校验（冲突地区隔离）---------------------
  dt[, expected_treat := data.table::fifelse(
    is.infinite(first_treat), 0L,
    data.table::fifelse(month >= first_treat, 1L, 0L))]
  conflict <- dt[!is.na(treat) & treat != expected_treat]
  conflict_regions <- unique(conflict$region_id)
  quarantined <- data.table()
  if (length(conflict_regions) > 0L) {
    quarantined <- conflict[, .(
      reason = "treat_status_conflict",
      detail = paste0("观察 treat=", treat, " 但按首次暴露月应为 ",
                      expected_treat, " @", month),
      n_rows = .N), by = .(region_id)]
    log_msg("处理状态冲突，隔离地区数: ", length(conflict_regions))
  }

  # ---- 规则 3：协变量全缺地区排除（预设规则，唯一支持值 exclude）-------------
  cov_miss <- dt[, lapply(.SD, function(z) all(is.na(z))), .SDcols = covs,
                 by = region_id]
  full_miss_regions <- cov_miss[Reduce(`|`, lapply(covs, function(v) cov_miss[[v]])),
                                region_id]
  excluded_cov <- data.table(region_id = full_miss_regions)[
    , `:=`(reason = "covariates_fully_missing",
           detail = paste("全部行的协变量缺失:", paste(covs, collapse = ",")))]

  excluded_regions <- rbind(quarantined[, .(region_id, reason, detail)],
                            excluded_cov, fill = TRUE)

  # ---- 规则 4：缺月检测（只登记，不填充）------------------------------------
  # 缺月定义：地区自身观测跨度 [首月, 末月] 内缺失的月份。
  spans <- dt[, .(min_m = min(month), max_m = max(month)), by = region_id]
  full_grid <- spans[, {
    allm <- seq.Date(min_m, max_m, by = "month")
    .(month = allm)
  }, by = region_id]
  observed <- dt[, .(region_id, month, y_present = !is.na(y))]
  grid_dt <- merge(full_grid, observed, by = c("region_id", "month"), all.x = TRUE)
  missing_months <- grid_dt[is.na(y_present), .(region_id, missing_month = month)]
  log_msg("登记缺月数（不填充）: ", nrow(missing_months))

  # 结果缺失但行存在：单独登记，区别于缺月
  outcome_na <- dt[is.na(y), .(region_id, month,
                               detail = "行存在但结果 y 缺失")]
  setnames(outcome_na, "month", "missing_month")
  outcome_na[, `:=`(reason = "outcome_na_row")]
  outcome_na <- outcome_na[, .(region_id, missing_month, reason, detail)]
  missing_months[, `:=`(reason = "missing_row", detail = "月份完全无记录")]
  missing_register <- rbind(missing_months, outcome_na, fill = TRUE)

  # ---- 样本窗口裁剪 ---------------------------------------------------------
  win_start <- if (!is.null(cfg$window_start)) parse_month(cfg$window_start)
               else min(dt$month)
  win_end   <- if (!is.null(cfg$window_end)) parse_month(cfg$window_end)
               else max(dt$month)
  log_msg("样本窗口: ", win_start, " 至 ", win_end)
  d <- dt[month >= win_start & month <= win_end]

  # ---- 构建分析面板 ---------------------------------------------------------
  d <- d[!region_id %in% excluded_regions$region_id]
  n_before <- nrow(d)
  # 行级完整观测：y、treat、协变量均非缺；缺月行本来就不存在，不会被虚构
  complete_expr <- parse(text = paste(
    c("!is.na(y)", "!is.na(treat)",
      paste0("!is.na(", covs, ")")), collapse = " & "))
  dropped_rows <- d[!eval(complete_expr),
                    .(n_rows = .N), by = .(region_id)]
  analytic <- d[eval(complete_expr)]
  log_msg("行级完整观测: ", nrow(analytic), " / 窗口内 ", n_before,
          "；缺行/缺值丢弃行: ", n_before - nrow(analytic))

  # 地区在完整样本中彻底消失 -> 补登排除原因
  vanished <- setdiff(unique(d$region_id), unique(analytic$region_id))
  if (length(vanished) > 0L) {
    extra <- data.table(
      region_id = vanished,
      reason = "no_complete_rows_in_window",
      detail = "窗口内没有 y/treat/协变量完整的行（缺月不填充）")
    excluded_regions <- rbind(excluded_regions, extra, fill = TRUE)
  }

  # ---- 时间/队列整数索引（did 约定：never-treated g=0）----------------------
  time_map <- data.table(month = sort(unique(analytic$month)))[, t := seq_len(.N)]
  analytic <- merge(analytic, time_map, by = "month")
  cohort_dates <- sort(unique(analytic$first_treat[is.finite(analytic$first_treat)]))
  cohort_map <- data.table(first_treat = cohort_dates)
  cohort_map <- merge(cohort_map, time_map, by.x = "first_treat",
                      by.y = "month", all.x = TRUE)
  setnames(cohort_map, "t", "g")
  analytic <- merge(analytic, cohort_map, by = "first_treat", all.x = TRUE)
  analytic[is.infinite(first_treat), g := 0L]

  out_of_window <- analytic[is.na(g) & !is.infinite(first_treat)]
  if (nrow(out_of_window) > 0L) {
    extra <- unique(out_of_window[, .(
      region_id,
      reason = "cohort_starts_outside_window",
      detail = paste("首次暴露月不在样本时间轴上:", first_treat))])
    excluded_regions <- rbind(excluded_regions, extra, fill = TRUE)
    analytic <- analytic[!region_id %in% extra$region_id]
    log_msg("队列落在窗口时间轴外，排除地区数: ", uniqueN(extra$region_id))
  }

  analytic[, `:=`(id = as.integer(factor(region_id)))]
  setorder(analytic, region_id, t)

  meta <- list(
    window_start = as.character(win_start),
    window_end   = as.character(win_end),
    n_periods    = nrow(time_map),
    n_regions_input       = uniqueN(dt$region_id),
    n_regions_analytic    = uniqueN(analytic$region_id),
    n_rows_window         = n_before,
    n_rows_analytic       = nrow(analytic),
    n_missing_register    = nrow(missing_register),
    n_quarantined         = length(conflict_regions),
    n_covariate_excluded  = length(full_miss_regions),
    n_corrections         = nrow(corrections_log),
    covariates            = covs,
    never_treated_units   = uniqueN(analytic[g == 0L, region_id]),
    treated_cohorts       = length(cohort_dates)
  )

  log_msg("分析面板: ", meta$n_regions_analytic, " 个地区, ",
          meta$n_periods, " 期, ", meta$n_rows_analytic, " 行；",
          "永不处理地区 ", meta$never_treated_units, " 个；处理队列 ",
          meta$treated_cohorts, " 个")

  invisible(list(
    analytic         = analytic,
    time_map         = time_map,
    cohort_map       = cohort_map,
    excluded_regions = excluded_regions,
    missing_register = missing_register,
    corrections_log  = corrections_log,
    dropped_rows     = dropped_rows,
    meta             = meta
  ))
}
