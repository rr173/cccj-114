#!/usr/bin/env Rscript
# =============================================================================
# ledger_demo.R —— 端到端演示：把“运营逐批导入 -> 一致导出”走一遍并逐条核对
#
# 用法（临时库，不触碰仓库数据）:
#   Rscript R/ledger/ledger_demo.R
#   Rscript R/ledger/ledger_demo.R --workdir /tmp/ledger_demo
#
# 覆盖需求中的场景：
#   1. 月报先到、登记后到的解挂（R003）
#   2. 乱序修订与重复报送（d03 乱序/更高版本同行，d01 重传 -> replay）
#   3. 删除后旧版本重传不复活；更高版本 upsert 恢复
#   4. 登记更正后跨月重算 treat（R003: 2022-06 -> never）
#   5. 混合批次有一条冲突 -> 两张表都不变
#   6. 一致读取视图导出 + 临时目录发布
#   7. 事务内中断点（--interrupt-at before-receipt）
# 并发进程与 SIGKILL 场景见 tests/ledger/test_concurrency.R
# =============================================================================

here <- dirname(normalizePath(sub("--file=", "",
          grep("--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = FALSE))
source(file.path(here, "cli.R"), local = FALSE)
load_ledger_lib()

opts <- parse_cli(commandArgs(trailingOnly = TRUE))
workdir <- cli_opt(opts, "workdir",
                   file.path(tempdir(), paste0("ledger_demo_", Sys.getpid())))
dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
root <- getwd()
demo_src <- file.path(root, "data", "ledger_demo")

# 确保演示批次存在（幂等）
if (!file.exists(file.path(demo_src, "monthly", "d01_monthly.csv"))) {
  old <- getwd(); on.exit(setwd(old), add = TRUE)
  setwd(root); source(file.path(here, "make_demo_batches.R"), local = TRUE)
}

db <- file.path(workdir, "ledger.sqlite")
exports <- file.path(workdir, "exports")
unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE)
if (dir.exists(exports)) unlink(exports, recursive = TRUE, force = TRUE)

results <- list()
check <- function(name, ok, detail = "") {
  results[[length(results) + 1L]] <<- list(check = name, pass = isTRUE(ok),
                                           detail = as.character(detail))
  cat(sprintf("[%s] %s%s\n", if (isTRUE(ok)) "PASS" else "FAIL", name,
              if (nzchar(detail)) paste0(" — ", detail) else ""))
  invisible(isTRUE(ok))
}

M <- function(f) file.path(demo_src, "monthly", f)
Rd <- function(f) file.path(demo_src, "registration", f)

init_ledger(db, "SRC-DEMO")
con <- open_ledger(db, quiet = TRUE)
on.exit(close_ledger(con), add = TRUE)

imp <- function(batch, monthly = NULL, registration = NULL, ...) {
  ingest_batch(con, list(monthly = monthly, registration = registration),
               c(list(batch_id = batch), list(...)))
}
latest_export <- function() {
  dirs <- list.dirs(exports, recursive = FALSE)
  dirs <- dirs[!grepl("^\\.", basename(dirs))]
  d <- dirs[which.max(file.info(dirs)$mtime)]
  list(dir = d,
       panel = data.table::fread(file.path(d, "panel.csv")),
       missing = data.table::fread(file.path(d, "missing_registration.csv")),
       prov = data.table::fread(file.path(d, "provenance.csv")))
}
panel_for <- function(p, rid) p[region_id == rid]

cat("\n== 场景 1：月报先到、登记后到，未登记地区只进待补清单 ==\n")
r1 <- imp("D01", M("d01_monthly.csv"), Rd("d01_registration.csv"))
check("D01 提交成功", identical(r1$status, "committed"))
check("D01 月报 72 行全部入库", r1$monthly$n_applied == 72L)
e1 <- export_panel(con, list(out_dir = exports)); x1 <- latest_export()
check("首次导出：R003 的 24 个月全部进入待补清单",
      all(x1$missing$region_id == "R003") && nrow(x1$missing) == 24L,
      sprintf("待补 %d 行", nrow(x1$missing)))
check("首次导出：月表只有 R001、R002（48 行）",
      nrow(x1$panel) == 48L && !"R003" %in% x1$panel$region_id)
check("未登记地区不被默认成 never",
      !"R003" %in% x1$panel$region_id)

cat("\n== 场景 2：登记补报后解挂；修订乱序与重复报送 ==\n")
r2 <- imp("D02", M("d02_monthly.csv"), Rd("d02_registration.csv"))
check("D02 同时含 R001 修订与 R003 登记",
      r2$monthly$n_applied == 1L && r2$registration$n_applied == 1L)
e2 <- export_panel(con, list(out_dir = exports)); x2 <- latest_export()
check("R003 解挂后出现在月表（72 行）", nrow(x2$panel) == 72L)
check("R003 的 treat 按登记月计算（2022-06 起为 1）",
      all(panel_for(x2$panel, "R003")[month >= "2022-06", treat] == 1L) &&
      all(panel_for(x2$panel, "R003")[month < "2022-06", treat] == 0L))
check("R001 2022-06 已用 revision 2 的值",
      panel_for(x2$panel, "R001")[month == "2022-06", y] ==
        as.numeric(data.table::fread(M("d02_monthly.csv"))$y))

r3 <- imp("D03", M("d03_monthly.csv"))
check("D03 批内乱序：两条都应用（replace）",
      r3$monthly$n_applied == 2L &&
        all(vapply(r3$outcomes, function(o) o$outcome, "") == "replace"))
replay <- imp("D01", M("d01_monthly.csv"), Rd("d01_registration.csv"))
check("同批次原文件重传返回原回执且不重复生效",
      isTRUE(replay$replay) && replay$monthly$n_applied == 72L)

cat("\n== 场景 3：删除后旧版本重传不复活；更高版本 upsert 恢复 ==\n")
imp("D04", M("d04_monthly.csv"))
cur <- query_current_monthly(con)
check("R002 2022-03 当前为 delete（revision 2）",
      cur[region_id == "R002" & month == "2022-03", op] == "delete")
e4 <- export_panel(con, list(out_dir = exports)); x4 <- latest_export()
check("撤回月份不再出现在月表", !"2022-03" %in% panel_for(x4$panel, "R002")$month)
check("撤回月份不进入待补登记清单（该月已无有效月报）",
      !("2022-03" %in% x4$missing[region_id == "R002", month]))
r5 <- imp("D05", M("d05_monthly.csv"))
check("旧版本 upsert 后到被记为 stale",
      identical(r5$outcomes[[1]]$outcome, "stale"))
check("旧版本未复活删除（仍为 delete）",
      query_current_monthly(con)[region_id == "R002" & month == "2022-03",
                                 op] == "delete")
hist <- query_monthly_history(con, "R002", "2022-03")
check("历史账本保留 delete 版本与过时来件",
      nrow(hist) == 2L && any(hist$op == "delete"))
r6 <- imp("D06", M("d06_monthly.csv"))
check("更高版本 upsert 恢复撤回月份（restore）",
      identical(r6$outcomes[[1]]$outcome, "restore"))
e6 <- export_panel(con, list(out_dir = exports)); x6 <- latest_export()
check("恢复后该月重新出现在月表",
      "2022-03" %in% panel_for(x6$panel, "R002")$month)

cat("\n== 场景 4：登记更正后跨月重算 treat ==\n")
r7 <- imp("D07", NULL, Rd("d07_registration.csv"))
check("R003 登记更正到 never（revision 2）", r7$registration$n_applied == 1L)
e7 <- export_panel(con, list(out_dir = exports)); x7 <- latest_export()
r3panel <- panel_for(x7$panel, "R003")
check("更正后 R003 所有现存月份 treat 均为 0",
      nrow(r3panel) == 24L && all(r3panel$treat == 0L) &&
        all(r3panel$first_treat_month == "never"))
prov3 <- x7$prov[region_id == "R003"]
check("来源表记录登记 revision 2 与批次 D07",
      all(prov3$registration_revision == 2L) &&
        all(prov3$registration_batch_id == "D07"))

cat("\n== 场景 5：混合批次有同版本内容冲突，两张表都不变 ==\n")
snap_before <- ledger_snapshot(con)
monthly_before <- query_current_monthly(con)
reg_before <- query_current_registration(con)
conflict_err <- tryCatch({
  # 同批：R001 一条可应用的 revision 6，R002 登记 revision 1 内容冲突
  tmpm <- file.path(workdir, "d09_monthly.csv")
  data.table::fwrite(data.table::data.table(
    region_id = "R001", month = "2022-09", revision = 6L, op = "upsert",
    y = -1.234, x1 = 0, x2 = 0), tmpm)
  imp("D09", tmpm, Rd("d08_registration_bad.csv"))
  NULL
}, ledger_error = function(e) e)
check("冲突批次以 revision_content_conflict 失败",
      !is.null(conflict_err) &&
        identical(conflict_err$code, "revision_content_conflict"))
check("失败批次不留回执", is.null(get_receipt(con, "D09")))
check("失败后月报当前表完全不变",
      identical(query_current_monthly(con), monthly_before))
check("失败后登记当前表完全不变",
      identical(query_current_registration(con), reg_before))
fixed <- imp("D09", file.path(workdir, "d09_monthly.csv"))
check("修正文件后同 batch_id 可重新导入",
      identical(fixed$status, "committed") && fixed$monthly$n_applied == 1L)

cat("\n== 场景 6：一致读取视图内三件输出互相自洽 ==\n")
e8 <- export_panel(con, list(out_dir = exports)); x8 <- latest_export()
check("月表与来源表逐行同键同序",
      nrow(x8$panel) == nrow(x8$prov) &&
        identical(x8$panel[, .(region_id, month)],
                  x8$prov[, .(region_id, month)]))
check("月表 + 待补清单 = 全部当前有效月报行",
      nrow(x8$panel) + nrow(x8$missing) ==
        nrow(query_current_monthly(con)[op == "upsert"]))
check("来源表 revision/批次与当前表一致",
      all(x8$prov$monthly_revision ==
            query_current_monthly(con)[op == "upsert"][
              match(paste(x8$prov$region_id, x8$prov$month),
                    paste(region_id, month)), revision]))
check("输出目录发布为独立 release，未覆盖旧导出",
      length(list.dirs(exports, recursive = FALSE)) >= 4L)
check("账本自检无不变量破坏", check_ledger_invariants(con)$consistent)

cat("\n== 场景 7：事务内中断点，整批不可见 ==\n")
before <- ledger_snapshot(con)
interrupted <- tryCatch({
  imp("D10", M("d01_monthly.csv"), interrupt_at = "before_receipt")
  NULL
}, ledger_error = function(e) e)
check("中断点触发 interrupted_at",
      !is.null(interrupted) && identical(interrupted$code, "interrupted_at"))
check("中断后没有回执、当前行数不变",
      is.null(get_receipt(con, "D10")) &&
        identical(ledger_snapshot(con)$n_monthly_versions,
                  before$n_monthly_versions))

cat(sprintf("\n==== 演示完成：%d 项通过 / %d 项失败 ====\n",
            sum(vapply(results, `[[`, TRUE, "pass")),
            sum(!vapply(results, `[[`, TRUE, "pass"))))
cat("账本: ", db, "\n导出: ", exports, "\n", sep = "")
if (!all(vapply(results, `[[`, TRUE, "pass"))) quit(save = "no", status = 1L)
