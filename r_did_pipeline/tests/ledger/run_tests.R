#!/usr/bin/env Rscript
# =============================================================================
# run_tests.R —— 接入账本自动测试总入口
#
# 用法:
#   Rscript tests/ledger/run_tests.R            # 全部测试
#   Rscript tests/ledger/run_tests.R ingest     # 只跑匹配的文件
#
# 测试全部使用临时目录与临时 SQLite 库，不依赖 did 拟合成功。
# =============================================================================

here <- dirname(normalizePath(sub("--file=", "",
          grep("--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = FALSE))
root <- normalizePath(file.path(here, "..", ".."), mustWork = TRUE)
setwd(root)
Sys.setenv(LEDGER_TEST_ROOT = root)

suppressPackageStartupMessages(library(testthat))
filter <- commandArgs(trailingOnly = TRUE)
files <- sort(list.files(here, pattern = "^test-.*\\.R$", full.names = TRUE))
if (length(filter) > 0L)
  files <- files[grepl(paste(filter, collapse = "|"), basename(files),
                       ignore.case = TRUE)]
if (length(files) == 0L) stop("没有匹配的测试文件")

cat("仓库根: ", root, "\n")
cat("测试文件:\n"); cat(paste0("  ", basename(files), "\n"), sep = "")

results <- lapply(files, function(f) {
  cat("\n==================== ", basename(f), " ====================\n", sep = "")
  as.data.frame(test_file(f, reporter = "summary", env = new.env(parent = globalenv())))
})
df <- do.call(rbind, results)
failed <- sum(df$failed)
warned <- sum(df$warning)
skipped <- sum(df$skipped)
cat("\n============================================================\n")
cat(sprintf("总计: %d 通过 / %d 失败 / %d 警告 / %d 跳过\n",
            sum(df$passed), failed, warned, skipped))
if (failed > 0L || warned > 0L) quit(save = "no", status = 1L)
cat("全部通过\n")
