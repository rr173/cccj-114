#!/usr/bin/env Rscript
# =============================================================================
# make_demo_batches.R —— 生成可直接运行的演示批次（幂等，覆盖 data/ledger_demo）
#
# 用法:
#   Rscript R/ledger/make_demo_batches.R [输出目录]
#
# 产物（每个场景一个或多个批次文件，均为真实 CSV，可直接 import_batch.R）：
#   d01_monthly.csv / d01_registration.csv
#       R001、R002（登记为 never）各 24 个月；R003 只有月报，暂未登记
#   d02_monthly.csv / d02_registration.csv
#       R001 的 2022-06 修正（revision 2，y 变化）；R003 登记后补（2022-06）
#   d03_monthly.csv
#       R001 的 2022-07 revision 3；同批 R001 2022-08 revision 5
#       （乱序 + 更高版本同时到达）
#   d04_monthly.csv
#       R002 的 2022-03 撤回（revision 2, delete）
#   d05_monthly.csv
#       过时来件：R001 2022-07 revision 2（低于已生效的 3）
#   d06_monthly.csv
#       R002 的 2022-03 revision 3, upsert（恢复已撤回月份）
#   d07_registration.csv
#       R003 登记更正为 never（revision 2），用于验证跨月重算 treat
#   d08_registration_bad.csv
#       R002 登记 revision 1 的内容冲突（与库内 revision 1 不同）——制造整批失败
# =============================================================================

library(data.table)

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) args[[1]] else
  file.path("data", "ledger_demo")
dir.create(root, recursive = TRUE, showWarnings = FALSE)
mdir <- file.path(root, "monthly")
rdir <- file.path(root, "registration")
dir.create(mdir, showWarnings = FALSE, recursive = TRUE)
dir.create(rdir, showWarnings = FALSE, recursive = TRUE)

snap <- file.path("data", "snapshots", "synthetic_clean.csv")
if (!file.exists(snap))
  stop("缺少基础快照: ", snap, "（先运行 R/make_synthetic.R）")
d <- fread(snap, colClasses = "character")
base <- d[region_id %in% c("R001", "R002", "R003"),
          .(region_id, month = substr(month, 1, 7),
            revision = 1L, op = "upsert", y, x1, x2)]
fts <- unique(d[region_id %in% c("R001", "R002", "R003"),
                .(region_id, ft = first_treat_month)])
fts[, first_treat_month := ifelse(ft == "never", "never", substr(ft, 1, 7))][
  , ft := NULL]

fwrite(base, file.path(mdir, "d01_monthly.csv"))
fwrite(fts[region_id %in% c("R001", "R002"),
           .(region_id, revision = 1L, op = "upsert", first_treat_month)],
       file.path(rdir, "d01_registration.csv"))

corr <- base[region_id == "R001" & month == "2022-06"]
corr[, `:=`(revision = 2L,
            y = as.character(as.numeric(y) + 0.5))]
fwrite(corr, file.path(mdir, "d02_monthly.csv"))
fwrite(fts[region_id == "R003",
           .(region_id, revision = 1L, op = "upsert", first_treat_month)],
       file.path(rdir, "d02_registration.csv"))

d3 <- base[region_id == "R001" & month == "2022-07"][, revision := 3L]
d3 <- rbind(d3,
  base[region_id == "R001" & month == "2022-08"][, revision := 5L])
fwrite(d3, file.path(mdir, "d03_monthly.csv"))

fwrite(base[region_id == "R002" & month == "2022-03"][
         , `:=`(revision = 2L, op = "delete", y = "", x1 = "", x2 = "")],
       file.path(mdir, "d04_monthly.csv"))

fwrite(base[region_id == "R001" & month == "2022-07"][
         , `:=`(revision = 2L, y = as.character(as.numeric(y) + 9))],
       file.path(mdir, "d05_monthly.csv"))

fwrite(base[region_id == "R002" & month == "2022-03"][, revision := 3L],
       file.path(mdir, "d06_monthly.csv"))

fwrite(data.table(region_id = "R003", revision = 2L, op = "upsert",
                  first_treat_month = "never"),
       file.path(rdir, "d07_registration.csv"))

conflict <- fts[region_id == "R002"]
conflict[, first_treat_month := ifelse(first_treat_month == "never",
                                       "2022-09", "never")]
fwrite(conflict[, .(region_id, revision = 1L, op = "upsert",
                    first_treat_month)],
       file.path(rdir, "d08_registration_bad.csv"))

cat("演示批次已写出:", normalizePath(root), "\n")
for (f in list.files(root, recursive = TRUE))
  cat("  ", f, "\n")
