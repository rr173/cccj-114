# =============================================================================
# test-export.R —— 月表/待补清单/来源表、treat 重算、缺月与缺值保留、一致快照
# =============================================================================

test_that("没有登记的地区不进月表、只进待补清单，且不默认 never", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", rbind(
           mrows("R1", c("2024-01", "2024-02"), 1L),
           mrows("R2", c("2024-01", "2024-02"), 1L))),
         registration = registration_file(d, "r", rrows("R1"))),
    list(batch_id = "B1"))
  t <- compute_export_tables(L$con)
  expect_equal(sort(unique(t$panel$region_id)), "R1")
  expect_equal(nrow(t$panel), 2L)
  expect_equal(nrow(t$missing), 2L)
  expect_true(all(t$missing$region_id == "R2"))
  expect_equal(t$missing_summary$n_months, 2L)
  expect_equal(t$missing_summary$months, "2024-01 2024-02")
  expect_equal(t$missing_summary$reason, "no_registration")
})

test_that("登记撤回后该地区全部月份转入待补清单", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R1", c("2024-01", "2024-02"))),
         registration = registration_file(d, "r", rrows("R1", 1L))),
    list(batch_id = "B1"))
  expect_equal(nrow(compute_export_tables(L$con)$panel), 2L)
  ingest_batch(L$con,
    list(monthly = NULL,
         registration = registration_file(d, "r2",
           rrows("R1", 2L, op = "delete", first_treat_month = NA))),
    list(batch_id = "B2"))
  t <- compute_export_tables(L$con)
  expect_equal(nrow(t$panel), 0L)
  expect_equal(nrow(t$missing), 2L)
  expect_true(all(t$missing$reason == "registration_withdrawn"))
})

test_that("treat 按 month 是否达到 first_treat_month 计算，never 恒为 0", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  months <- c("2023-12", "2024-01", "2024-02", "2024-03")
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", rbind(
           mrows("R1", months), mrows("R2", months))),
         registration = registration_file(d, "r", rbind(
           rrows("R1", 1L, "upsert", "2024-02"),
           rrows("R2", 1L, "upsert", "never")))),
    list(batch_id = "B1"))
  p <- compute_export_tables(L$con)$panel
  r1 <- p[region_id == "R1"]
  expect_equal(r1$treat, c(0L, 0L, 1L, 1L))
  expect_equal(r1$first_treat_month, rep("2024-02", 4))
  expect_true(all(p[region_id == "R2", treat] == 0L))
})

test_that("登记更正后跨月重算 treat 与 first_treat_month", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  months <- c("2024-01", "2024-02", "2024-03")
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R1", months)),
         registration = registration_file(d, "r", rrows("R1", 1L, "upsert",
                                                       "2024-03"))),
    list(batch_id = "B1"))
  expect_equal(compute_export_tables(L$con)$panel$treat, c(0L, 0L, 1L))
  ingest_batch(L$con,
    list(monthly = NULL,
         registration = registration_file(d, "r2",
           rrows("R1", 2L, "upsert", "2024-02"))),
    list(batch_id = "B2"))
  p <- compute_export_tables(L$con)$panel
  expect_equal(p$treat, c(0L, 1L, 1L))          # 更正作用于该地区所有月份
  expect_true(all(p$first_treat_month == "2024-02"))
  # 再更正为 never：全部 0
  ingest_batch(L$con,
    list(monthly = NULL,
         registration = registration_file(d, "r3",
           rrows("R1", 3L, "upsert", "never"))),
    list(batch_id = "B3"))
  expect_true(all(compute_export_tables(L$con)$panel$treat == 0L))
})

test_that("真实缺月与数值缺失都保留，不补观测", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  # R1 只有 01 和 03（02 缺行）；03 的 x2 显式缺失
  rows <- rbind(
    mrows("R1", "2024-01", 1L, y = 1, x1 = 0.1, x2 = 0.2),
    mrows("R1", "2024-03", 1L, y = 3, x1 = 0.3, x2 = NA))
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", rows),
         registration = registration_file(d, "r", rrows("R1"))),
    list(batch_id = "B1"))
  p <- compute_export_tables(L$con)$panel
  expect_equal(nrow(p), 2L)
  expect_equal(p$month, c("2024-01", "2024-03"))
  expect_false("2024-02" %in% p$month)
  expect_true(is.na(p[month == "2024-03", x2]))
  expect_equal(p[month == "2024-03", y], 3)
})

test_that("来源表逐行记录月报/登记 revision 与批次", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R1", c("2024-01", "2024-02"),
                                              1L)),
         registration = registration_file(d, "r", rrows("R1", 1L))),
    list(batch_id = "B1"))
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m2", mrows("R1", "2024-02", 2L, y = 7)),
         registration = NULL), list(batch_id = "B2"))
  ingest_batch(L$con,
    list(monthly = NULL,
         registration = registration_file(d, "r2",
           rrows("R1", 2L, "upsert", "2024-03"))),
    list(batch_id = "B3"))
  t <- compute_export_tables(L$con)
  p <- t$provenance
  expect_equal(nrow(p), 2L)
  jan <- p[month == "2024-01"]
  feb <- p[month == "2024-02"]
  expect_equal(jan$monthly_revision, 1L)
  expect_equal(jan$monthly_batch_id, "B1")
  expect_equal(feb$monthly_revision, 2L)
  expect_equal(feb$monthly_batch_id, "B2")
  expect_true(all(p$registration_revision == 2L))
  expect_true(all(p$registration_batch_id == "B3"))
})

test_that("导出发布不覆盖既有 release，且写临时目录后重命名", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R1", "2024-01")),
         registration = registration_file(d, "r", rrows("R1"))),
    list(batch_id = "B1"))
  out <- file.path(d, "exports")
  r1 <- export_panel(L$con, list(out_dir = out))
  r2 <- export_panel(L$con, list(out_dir = out))
  expect_false(identical(r1$release_dir, r2$release_dir))
  expect_true(all(file.exists(unlist(r1$files))))
  expect_true(all(file.exists(unlist(r2$files))))
  expect_false(any(grepl("^\\.staging", list.files(out))))
  rel <- query_releases(L$con)
  expect_equal(nrow(rel), 2L)
})

test_that("导出失败不破坏调用者此前的导出", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", mrows("R1", "2024-01")),
         registration = registration_file(d, "r", rrows("R1"))),
    list(batch_id = "B1"))
  out <- file.path(d, "exports")
  first <- export_panel(L$con, list(out_dir = out))
  before_files <- sort(list.files(first$release_dir))
  before_md5 <- unname(tools::md5sum(file.path(first$release_dir,
                                               "panel.csv")))
  # 让 staging 目录创建失败（根目录变成只读不可靠，改用非法 out 子路径）
  err <- tryCatch(export_panel(L$con, list(
    out_dir = file.path(first$release_dir, "panel.csv", "nested"))),
    error = function(e) e)
  expect_true(inherits(err, "error"))
  expect_equal(sort(list.files(first$release_dir)), before_files)
  expect_equal(unname(tools::md5sum(file.path(first$release_dir,
                                              "panel.csv"))), before_md5)
  expect_equal(nrow(query_releases(L$con)), 1L)
})

test_that("导出在同一读取快照内完成（并发写入不产生半新半旧）", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "m", rbind(
           mrows("R1", c("2024-01", "2024-02"), 1L, y = 1),
           mrows("R2", c("2024-01", "2024-02"), 1L, y = 2))),
         registration = registration_file(d, "r", rbind(
           rrows("R1", 1L, "upsert", "2024-02"),
           rrows("R2", 1L, "upsert", "2024-02")))),
    list(batch_id = "B1"))
  # 另一个连接在导出查询开始后提交：将 R2 的登记改到 2024-01
  con2 <- open_ledger(L$db, quiet = TRUE)
  on.exit(close_ledger(con2), add = TRUE)
  seen <- with_read_snapshot(L$con, function() {
    t1 <- compute_export_tables(L$con)
    ingest_batch(con2,
      list(monthly = NULL,
           registration = registration_file(d, "r2",
             rrows("R2", 2L, "upsert", "2024-01"))),
      list(batch_id = "B2"))
    t2 <- compute_export_tables(L$con)
    list(t1 = t1, t2 = t2)
  })
  expect_equal(seen$t1$panel, seen$t2$panel)
  # 快照外才看到新登记
  after <- compute_export_tables(L$con)
  expect_equal(after$panel[region_id == "R2" & month == "2024-01", treat], 1L)
})

test_that("空账本导出得到合法空表", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  out <- file.path(L$dir, "exports")
  r <- export_panel(L$con, list(out_dir = out))
  expect_equal(r$n_panel, 0L)
  p <- fread(file.path(r$release_dir, "panel.csv"))
  expect_equal(nrow(p), 0L)
  expect_equal(names(p), PANEL_COLS)
  m <- fread(file.path(r$release_dir, "missing_registration.csv"))
  expect_equal(names(m), MISSING_COLS)
})
