#!/usr/bin/env Rscript
# =============================================================================
# make_synthetic.R —— 生成合成地区-月份面板（已知真值，用于管线验证）
#
# 用法:
#   Rscript R/make_synthetic.R <out_csv> [--seed N] [--no-never]
#                              [--scenario full|clean]
#
# 设计真值：
#  * 严格平行趋势、无任何处理前效应（DGP 中处理效应仅在 post 期出现）；
#  * 三个处理队列，队列效应异质（2 / 4 / 6），确保 TWFE 与 CS 目标估计不同；
#  * 动态爬坡：post 期效应按 min(event+1,3)/3 展开；
#  * full 场景注入：缺月（整行缺失，绝不填充）、结果缺失行、协变量全缺地区、
#    treat 状态冲突地区；
#  * --no-never：不设永不处理组，用于 not-yet-treated 对照耗尽验证。
# =============================================================================

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
out_csv <- args[1]
opt <- list(seed = 42L, no_never = FALSE, scenario = "full")
i <- 2L
while (i <= length(args)) {
  if (args[i] == "--seed") opt$seed <- as.integer(args[i + 1L])
  if (args[i] == "--no-never") opt$no_never <- TRUE
  if (args[i] == "--scenario") opt$scenario <- args[i + 1L]
  i <- i + 1L
}
set.seed(opt$seed)

n_regions <- 60L
n_months <- 24L
months <- seq.Date(as.Date("2022-01-01"), by = "month", length.out = n_months)
# 队列首次完整暴露月（时间索引，1 基）
cohort_t <- c(6L, 10L, 14L, 20L)
cohort_effect <- c(2, 4, 6, 3)          # 异质队列效应
names(cohort_effect) <- cohort_t

# 地区分配：4 个队列各 8 个处理地区（共 32），其余 28 个为永不处理
regions <- data.table(region_id = sprintf("R%03d", seq_len(n_regions)))
n_per_cohort <- 8L
if (opt$no_never) {
  g_assign <- rep(cohort_t, length.out = n_regions)
} else {
  g_assign <- c(rep(cohort_t, each = n_per_cohort),
                rep(0L, n_regions - 4L * n_per_cohort))
}
regions[, g_t := sample(g_assign)]      # 随机洗牌队列标签（按地区一一对应）
regions[, alpha := rnorm(.N, 0, 1)]     # 地区固定效应
regions[, x1 := rnorm(.N)]
regions[, x2 := rnorm(.N)]
beta <- c(0.5, -0.3)

# 长表面板：所有处理相关列均由同一份 g_t 派生，保证内部一致
dt <- regions[, .(t = seq_len(n_months)),
              by = .(region_id, g_t, alpha, x1, x2)]
dt[, month := months[t]]
# 向量化取队列暴露月：months[g_t] 逐行索引，禁止用 ifelse 包裹向量索引
dt[, first_treat_month := "never"]
dt[g_t > 0L, first_treat_month := as.character(months[g_t])]
dt[, time_fe := sin(t / 3) + 0.05 * t]       # 共同时间效应
# 动态异质处理效应（仅 post，pre 严格为 0）
dt[, event := ifelse(g_t == 0L, NA_integer_, t - g_t)]
dt[, true_effect := 0]
for (gg in cohort_t) {
  idx <- which(dt$g_t == gg)
  ramp <- pmin(pmax(dt$event[idx] + 1L, 0L), 3L) / 3
  dt$true_effect[idx] <- cohort_effect[as.character(gg)] *
    ifelse(dt$event[idx] >= 0L, ramp, 0)
}
dt[, treat := fifelse(g_t == 0L, 0L, as.integer(t >= g_t))]
dt[, y := alpha + time_fe + beta[1] * x1 + beta[2] * x2 +
       true_effect + rnorm(.N, 0, 0.5)]

# ---- full 场景注入数据质量问题 ----------------------------------------------
.injected <- new.env()
if (opt$scenario == "full") {
  # 1) 缺月：R001 删掉第 8、9 月整行（缺月，不填充）
  dt <- dt[!(region_id == "R001" & t %in% c(8L, 9L))]
  # 2) 结果缺失但行保留：R002 第 12 月 y=NA
  dt[region_id == "R002" & t == 12L, y := NA_real_]
  # 3) 协变量全缺地区：R059 的 x1/x2 全部置 NA（应被排除并列明，无论其队列）
  dt[region_id == "R059", `:=`(x1 = NA_real_, x2 = NA_real_)]
  .injected$cov_missing <- "R059"
  # 4) 受控 treat 冲突：取编号最大的处理地区（避开协变量全缺的 R059）
  treated_regions <- sort(unique(dt[g_t > 0L & region_id != "R059", region_id]))
  conflict_region <- tail(treated_regions, 1L)
  conflict_row <- dt[region_id == conflict_region & treat == 1L][1]
  dt[region_id == conflict_region & t == conflict_row$t, treat := 0L]
  .injected$conflict <- conflict_region
  message("注入 treat 冲突地区: ", conflict_region, " @t=", conflict_row$t)
} else {
  .injected$cov_missing <- NA_character_
  .injected$conflict <- NA_character_
}

dt[, `:=`(g_t = NULL, alpha = NULL, time_fe = NULL, event = NULL)]
setorder(dt, region_id, t)
setcolorder(dt, c("region_id", "month", "y", "treat",
                  "first_treat_month", "x1", "x2", "true_effect", "t"))
dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
fwrite(dt, out_csv)
message("写出合成快照: ", out_csv, " | 行数 ", nrow(dt),
        " | 永不处理地区 ", uniqueN(dt[first_treat_month == "never", region_id]),
        " | no_never=", opt$no_never)

# 注入清单，供验证脚本动态读取（不硬编码具体地区编号）
manifest <- data.table(
  snapshot = basename(out_csv), seed = opt$seed,
  missing_month_region = "R001", missing_months = "8;9",
  outcome_na_region = "R002",
  cov_missing_region = .injected$cov_missing,
  conflict_region = .injected$conflict)
manifest_path <- sub("\\.csv$", "_manifest.csv", out_csv)
fwrite(manifest, manifest_path)
message("写出注入清单: ", manifest_path)

# ---- 处理时间更正文件：把一个真实处理地区移到 t=14 队列 ----------------------
corr_path <- file.path(dirname(out_csv), "..", "corrections",
                       sub("\\.csv$", "_corrections.csv", basename(out_csv)))
# 选当前不在 t=14 队列的处理地区，保证更正确实改变队列
cand <- sort(unique(dt[first_treat_month != as.character(months[14L]) &
                         first_treat_month != "never", region_id]))
corr_region <- cand[1]
corr <- data.table(region_id = corr_region,
                   correct_first_treat_month = as.character(months[14L]))
fwrite(corr, corr_path)
message("写出更正文件: ", corr_path, "（", corr_region, " -> ",
        as.character(months[14L]), "）")
