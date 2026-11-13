#!/usr/bin/env Rscript
# =============================================================================
# validate_pipeline.R —— 合成数据端到端验收（8 项硬性断言）
#
# 用法: Rscript R/validate_pipeline.R
# 退出码 0 = 全部通过；非 0 = 断言失败并打印明细。
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

ROOT <- normalizePath(file.path(dirname(
  sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), ".."),
  mustWork = FALSE)
if (!dir.exists(ROOT)) ROOT <- getwd()
setwd(ROOT)
invisible(lapply(c("utils.R", "validate_data.R", "identifiability.R",
                   "estimate.R", "aggregate.R"),
                 function(f) source(file.path("R", f))))

results <- data.table(check = character(), pass = logical(),
                      detail = character())
record <- function(name, ok, detail = "") {
  results <<- rbind(results, data.table(check = name, pass = isTRUE(ok),
                                        detail = as.character(detail)))
  cat(sprintf("[%s] %s %s\n", if (isTRUE(ok)) "PASS" else "FAIL", name, detail))
}

# ---- 0. 生成合成数据 ---------------------------------------------------------
run0 <- function(cmd) {
  cat("\n$", cmd, "\n")
  stopifnot(system(cmd) == 0L)
}
rbin <- file.path(ROOT, "..", ".envs", "r-did", "bin", "Rscript")
if (!file.exists(rbin)) rbin <- "Rscript"
run0(paste(rbin, "R/make_synthetic.R data/snapshots/synthetic_full.csv --seed 42"))
run0(paste(rbin, "R/make_synthetic.R data/snapshots/synthetic_no_never.csv",
           "--seed 42 --no-never"))
run0(paste(rbin, "R/make_synthetic.R data/snapshots/synthetic_clean.csv",
           "--seed 42 --scenario clean"))

manifest <- fread("data/snapshots/synthetic_full_manifest.csv")
CONFLICT_R <- manifest$conflict_region
COVMISS_R  <- manifest$cov_missing_region
corr_meta <- fread("data/corrections/synthetic_full_corrections.csv")
CORR_R <- corr_meta$region_id
CORR_NEW <- corr_meta$correct_first_treat_month

latest_dir <- function(tag_contains) {
  dirs <- list.dirs("output", recursive = FALSE)
  dirs <- dirs[grepl(tag_contains, basename(dirs))]
  stopifnot(length(dirs) >= 1L)
  dirs[which.max(file.info(dirs)$mtime)]
}

safe_read <- function(p) if (file.exists(p)) fread(p) else data.table()
run_one <- function(cfg_file, tag) {
  run0(paste(rbin, "R/run_pipeline.R", cfg_file, "--tag", tag))
  d <- latest_dir(paste0("_", tag, "$"))
  list(dir = d,
       gt = safe_read(file.path(d, "group_time_att.csv")),
       es = safe_read(file.path(d, "event_study.csv")),
       idt = safe_read(file.path(d, "identifiability.csv")),
       w  = safe_read(file.path(d, "weight_sums.csv")),
       pt = safe_read(file.path(d, "pretrend_test.csv")),
       tw = safe_read(file.path(d, "twfe_diagnostic.csv")),
       id = fromJSON(file.path(d, "identity.json")),
       ex = safe_read(file.path(d, "excluded_regions.csv")),
       mm = fread(file.path(d, "missing_months.csv")))
}

# =============================================================================
# 场景 1：not-yet-treated 主分析
# =============================================================================
cat("\n========== 场景 1: not-yet-treated ==========\n")
A <- run_one("config/nyt.json", "nyt")

# 检查 1：无处理前效应（窗口 [-3,-1] 事件期 ATT 联合检验 p>0.05，且各期 CI 含 0）
pre <- A$es[event_time %in% (-3:-1)]
pre_ci_ok <- all(pre$ci_u_lower_did <= 0 & pre$ci_u_upper_did >= 0)
record("1a_预趋势联合检验不显著", A$pt$p_value > 0.05,
       sprintf("p=%.3f", A$pt$p_value))
record("1b_处理前各事件期统一CI含0", pre_ci_ok,
       sprintf("n=%d 期", nrow(pre)))

# 检查 2：已知分期处理效应——post 各事件期点估计接近真值曲线
# 真值 event e>=0: 队列效应 * min(e+1,3)/3
gt_true <- copy(A$gt[!is.na(att)])
gt_true[, `:=`(e = t - g)]
base_eff <- c("6" = 2, "10" = 4, "14" = 6, "20" = 3)
gt_true[, true_att := base_eff[as.character(g)] *
         pmin(pmax(e + 1L, 0L), 3L) / 3]
gt_true <- gt_true[e >= 0]
cell_bias <- gt_true[, .(mae = mean(abs(att - true_att)))]
record("2_分期处理效应点估计接近真值", cell_bias$mae < 0.6,
       sprintf("post 单元格 MAE=%.3f", cell_bias$mae))

# 检查 3：异质效应下 TWFE 与 CS 目标估计不同
cs_post <- A$es[event_time >= 0, mean(att_did, na.rm = TRUE)]
record("3_TWFE与CS目标估计不同",
       abs(A$tw$estimate - cs_post) > 0.1,
       sprintf("TWFE=%.3f CS_post均值=%.3f 差=%.3f",
               A$tw$estimate, cs_post, abs(A$tw$estimate - cs_post)))

# 检查 4：缺月不填充 + 协变量全缺排除 + 状态冲突隔离
mm_ok <- nrow(A$mm[region_id == "R001" & reason == "missing_row"]) == 2L
record("4a_缺月登记且未被填充", mm_ok,
       sprintf("missing_register 行数=%d", nrow(A$mm)))
cov_ex <- A$ex[region_id == COVMISS_R & reason == "covariates_fully_missing"]
record("4b_协变量全缺地区被排除并列明", nrow(cov_ex) == 1L,
       sprintf("地区=%s", COVMISS_R))
conf_ex <- A$ex[region_id == CONFLICT_R & reason == "treat_status_conflict"]
record("4c_treat冲突地区被隔离", nrow(conf_ex) == 1L,
       sprintf("地区=%s", CONFLICT_R))
# 分析面板中不得出现被排除地区；60 个输入地区 - 2 个排除 = 58
panelA <- fread(file.path(A$dir, "analytic_panel.csv"))
record("4d_被排除地区不进入分析面板",
       !any(c(COVMISS_R, CONFLICT_R) %in% unique(panelA$region_id)) &&
         uniqueN(panelA$region_id) == 58L,
       sprintf("分析面板地区数=%d", uniqueN(panelA$region_id)))
# 缺月没有变成虚构观测：R001 在 t=8,9 无任何面板行
record("4e_缺月未产生虚构观测行",
       nrow(panelA[region_id == "R001" & t %in% c(8L, 9L)]) == 0L &&
         nrow(panelA[region_id == "R001"]) == 22L,
       sprintf("R001 面板行数=%d（应为22）", nrow(panelA[region_id == "R001"])))

# 检查 5：权重总和=1，手工等权重算可由逐单元格权重复算，CI 有限
ws_ok <- all(abs(A$w$weight_sum_equal - 1) <= 1e-8) &&
         all(abs(A$w$weight_sum_group - 1) <= 1e-8)
record("5a_两套事件期权重总和均为1", ws_ok,
       sprintf("n=%d 事件期", nrow(A$w)))
# 自洽性：用 event_study_weights 的等权逐单元格重算，必须等于 att_manual
cw <- fread(file.path(A$dir, "event_study_weights.csv"))
recomp <- cw[, .(re = sum(weight_equal * att)), by = event_time]
rec_merge <- merge(recomp,
                   A$es[!is.na(att_manual), .(event_time, att_manual)],
                   by = "event_time")
self_diff <- max(abs(rec_merge$re - rec_merge$att_manual))
record("5b_等权逐单元格重算与汇总曲线自洽", self_diff <= 1e-10,
       sprintf("max_self_diff=%.2e", self_diff))
# group 计数近似 vs did dynamic(IPW) 差异留痕（有协变量时允许小差异）
gd <- max(abs(A$es$att_did - A$es$att_manual_group), na.rm = TRUE)
record("5b2_group近似与did差异已留痕(IPW口径,容差0.3)", gd <= 0.3,
       sprintf("max_group_diff=%.3f", gd))
record("5c_置信区间完整且有限",
       all(is.finite(A$es$ci_p_lower_did), is.finite(A$es$ci_p_upper_did),
           is.finite(A$es$ci_u_lower_did), is.finite(A$es$ci_u_upper_did)))

# 检查 6：不可识别单元格不输出零（NA + 原因）；主场景可能全部可识别，
# 真正的不可识别覆盖在场景 5（对照耗尽）与场景 5b（无 never 组）验证
nonid <- A$idt[identifiable == FALSE]
record("6a_不可识别单元格均带原因(主场景可为0)",
       all(!is.na(nonid$nonid_reason)))
gt_nonid <- A$gt[identifiable == FALSE]
record("6b_不可识别单元格绝不输出零",
       all(is.na(gt_nonid$att)) && !any(gt_nonid$att == 0, na.rm = TRUE),
       sprintf("不可识别单元格=%d", nrow(gt_nonid)))

# =============================================================================
# 场景 2：never-treated 对照
# =============================================================================
cat("\n========== 场景 2: never-treated ==========\n")
B <- run_one("config/never.json", "never")
record("7a_never-treated分析可完成", nrow(B$gt[!is.na(att)]) > 0)
record("7b_never策略下不可识别原因正确",
       !any(grepl("not_yet_treated", B$idt$nonid_reason)))
# 两种对照策略的版本身份必须不同
record("7c_对照策略变化生成新版本", A$id$version_id != B$id$version_id)

# =============================================================================
# 场景 3：处理时间更正 -> 新版本 + 留痕
# =============================================================================
cat("\n========== 场景 3: 处理时间更正 ==========\n")
C <- run_one("config/nyt_corrected.json", "corrected")
corr_log <- fread(file.path(C$dir, "corrections_log.csv"))
record("8a_更正留痕存在且指向新队列",
       nrow(corr_log[region_id == CORR_R]) == 1L &&
         corr_log[region_id == CORR_R, as.character(new_first_treat)] ==
         CORR_NEW,
       sprintf("%s -> %s", CORR_R, CORR_NEW))
record("8b_更正生成不同版本身份", C$id$version_id != A$id$version_id)
record("8c_更正文件哈希进入身份",
       !is.null(C$id$hash_inputs$correction_md5) &&
         is.na(C$id$hash_inputs$correction_md5) == FALSE)

# =============================================================================
# 场景 4：样本窗口变化 + simple 权重 + 时间聚类 + 不同种子 -> 新版本
# =============================================================================
cat("\n========== 场景 4: 窗口/权重/聚类/种子变化 ==========\n")
D <- run_one("config/nyt_window.json", "window")
record("9a_窗口变化生成新版本", D$id$version_id != A$id$version_id)
record("9b_身份记录种子/聚类/权重/预检验窗口",
       D$id$hash_inputs$seed == 777 &&
         D$id$hash_inputs$cluster_level == "time" &&
         D$id$hash_inputs$es_weight == "simple")

# =============================================================================
# 场景 5：无永不处理组，not-yet-treated 对照耗尽
# =============================================================================
cat("\n========== 场景 5: 对照耗尽 ==========\n")
E <- run_one("config/nyt_no_never.json", "nonever")
exh <- E$idt[grepl("controls_exhausted|no_later_cohort", nonid_reason)]
record("10_对照耗尽单元格被标记(含原因,非零)",
       nrow(exh) >= 1L && all(exh$identifiable == FALSE) &&
         all(!is.na(exh$nonid_reason)))
# 进一步：所有不可识别单元格在结果表中必须是 NA，绝不能出现零或有限估计
gt_exh <- E$gt[identifiable == FALSE]
record("10b_耗尽单元格结果全为NA无零值",
       all(is.na(gt_exh$att)) && nrow(gt_exh) >= 1L,
       sprintf("不可识别=%d 个", nrow(gt_exh)))
# 无永不处理组时使用 never-treated 策略应整体不可识别
cfg_never_fail <- jsonlite::fromJSON("config/nyt_no_never.json")
cfg_never_fail$control_group <- "never-treated"
jsonlite::write_json(cfg_never_fail, "config/_tmp_never_on_no_never.json",
                     auto_unbox = TRUE, null = "null")
F <- run_one("config/_tmp_never_on_no_never.json", "neverfail")
record("11_无never组时never策略全部不可识别且无估计",
       nrow(F$idt) > 0L && all(F$idt$identifiable == FALSE) &&
         all(grepl("no_never_treated|no_earlier_period",
                   F$idt$nonid_reason)) &&
         nrow(F$gt[is.finite(att)]) == 0L,
       sprintf("不可识别=%d / %d", sum(F$idt$identifiable == FALSE),
               nrow(F$idt)))

# =============================================================================
# 场景 6：随机种子复现（同配置同种子两次运行，估计与 SE 完全一致）
# =============================================================================
cat("\n========== 场景 6: 种子复现 ==========\n")
G <- run_one("config/nyt.json", "repro")
g1 <- A$gt[order(g, t), .(g, t, att, se)]
g2 <- G$gt[order(g, t), .(g, t, att, se)]
record("12_同种子结果逐单元格复现",
       identical(g1$att, g2$att) && identical(g1$se, g2$se))
# 改种子 -> bootstrap SE 允许变化，但点估计应相同
cfg2 <- jsonlite::fromJSON("config/nyt.json"); cfg2$seed <- 999L
jsonlite::write_json(cfg2, "config/_tmp_seed999.json", auto_unbox = TRUE)
H <- run_one("config/_tmp_seed999.json", "seed999")
record("13_改种子点估计不变身份变化",
       max(abs(A$gt$att - H$gt$att), na.rm = TRUE) < 1e-10 &&
         H$id$version_id != A$id$version_id)

# ---- 汇总 -------------------------------------------------------------------
cat("\n================ 验收汇总 ================\n")
print(results)
n_fail <- sum(!results$pass)
cat(sprintf("\n通过 %d / %d；失败 %d\n", sum(results$pass), nrow(results), n_fail))
quit(status = if (n_fail == 0L) 0L else 1L)
