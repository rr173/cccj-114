# =============================================================================
# test-pipeline-handoff.R —— 导出月表能被现有分析入口的“读取/校验”层直接消费
# 不调用 did 拟合（环境中不保证 did 可用），只验证数据契约。
# =============================================================================

test_that("导出的 panel.csv 通过现有 validate_data.R 的校验并保持缺月/缺值", {
  root <- LEDGER_TEST_ROOT
  suppressPackageStartupMessages({
    library(data.table); library(jsonlite)
  })
  source(file.path(root, "R", "utils.R"))
  source(file.path(root, "R", "validate_data.R"))

  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  months <- c("2024-01", "2024-02", "2024-04")  # 2024-03 真实缺月
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", rbind(
           mrows("R1", months, 1L, y = c(1, 2, 4), x1 = 0.1, x2 = 0.2),
           mrows("R2", months, 1L, y = c(1, NA, 4), x1 = 0.3, x2 = 0.4),
           mrows("R3", months, 1L))),
         registration = registration_file(d, "r", rbind(
           rrows("R1", 1L, "upsert", "2024-02"),
           rrows("R2", 1L, "upsert", "never")))),
    list(batch_id = "B1"))
  out <- file.path(d, "exports")
  exp <- export_panel(L$con, list(out_dir = out))
  panel_path <- file.path(exp$release_dir, "panel.csv")

  cfg <- load_config(file.path(root, "config", "ledger_export.json"))
  cfg$data_path <- panel_path
  cfg$window_start <- NULL
  cfg$window_end <- NULL
  valid <- load_or_validate(cfg)
  # R3 无登记 -> 不影响分析入口：它在导出阶段就不在 panel 中
  expect_equal(sort(unique(valid$analytic$region_id)), c("R1", "R2"))
  # 缺月未补：R1 只有 1、2、4 月
  expect_false("2024-03" %in% format(valid$analytic$month, "%Y-%m"))
  # 行存在但 y 缺失：R2 2024-02 不进入完整观测分析面板，进入缺值登记
  expect_true(any(valid$missing_register$region_id == "R2" &
                  format(valid$missing_register$missing_month, "%Y-%m") ==
                    "2024-02"))
  # treat 与导出保持一致
  expect_true(all(valid$analytic[region_id == "R1" & month >= as.Date("2024-02-01"),
                                 treat] == 1))
  expect_true(all(valid$analytic[region_id == "R1" & month < as.Date("2024-02-01"),
                                 treat] == 0))
  expect_true(all(valid$analytic[region_id == "R2", treat] == 0))
})

test_that("月表列名与现有配置契约一致", {
  expect_equal(PANEL_COLS,
               c("region_id", "month", "y", "x1", "x2",
                 "first_treat_month", "treat"))
  root <- LEDGER_TEST_ROOT
  cfg <- jsonlite::fromJSON(file.path(root, "config", "ledger_export.json"),
                            simplifyVector = TRUE)
  expect_equal(cfg$col_unit, "region_id")
  expect_equal(cfg$col_time, "month")
  expect_equal(cfg$col_outcome, "y")
  expect_equal(cfg$col_treat, "treat")
  expect_equal(cfg$col_first_treat, "first_treat_month")
  expect_equal(unlist(cfg$covariates), c("x1", "x2"))
  expect_equal(cfg$never_treat_code, "never")
})
