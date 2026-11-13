# =============================================================================
# test-cli.R —— 命令行入口的端到端行为（初始化/导入/查询/导出）
# 与 README 中的运行命令一一对应，确保文档命令可直接执行。
# =============================================================================

test_that("README 中的 CLI 命令可直接运行", {
  d <- tempfile("cli_test_"); dir.create(d)
  db <- file.path(d, "ledger.sqlite")
  exports <- file.path(d, "exports")
  root <- LEDGER_TEST_ROOT
  run <- function(script, args) {
    cmd <- paste(c(shQuote(LEDGER_TEST_R), shQuote(file.path(root, "R",
      "ledger", script)), vapply(args, shQuote, "")), collapse = " ")
    out <- suppressWarnings(system(paste(cmd, "2>/dev/null"), intern = TRUE))
    status <- attr(out, "status")
    list(status = if (is.null(status)) 0L else status,
         json = if (length(out) > 0L) jsonlite::fromJSON(
           out[length(out)]) else NULL)
  }

  r <- run("init_ledger.R", c("--db", db, "--source-id", "SRC-CLI"))
  expect_equal(r$status, 0L)
  expect_equal(r$json$action, "created")
  expect_equal(r$json$schema_version, 1L)
  expect_true("monthly_version" %in% r$json$tables)

  # 幂等初始化
  r <- run("init_ledger.R", c("--db", db, "--source-id", "SRC-CLI"))
  expect_equal(r$json$action, "exists")
  # 改绑被拒
  r <- run("init_ledger.R", c("--db", db, "--source-id", "OTHER"))
  expect_equal(r$status, 2L)
  expect_equal(r$json$error_code, "source_id_conflict")

  # 演示批次位于仓库 data/ledger_demo
  demo <- file.path(root, "data", "ledger_demo")
  if (!file.exists(file.path(demo, "monthly", "d01_monthly.csv"))) {
    system(paste(shQuote(LEDGER_TEST_R),
                 shQuote(file.path(root, "R", "ledger",
                                   "make_demo_batches.R")), "2>/dev/null"))
  }
  r <- run("import_batch.R", c("--db", db, "--batch-id", "D01",
    "--monthly", file.path(demo, "monthly", "d01_monthly.csv"),
    "--registration", file.path(demo, "registration", "d01_registration.csv")))
  expect_equal(r$status, 0L)
  expect_equal(r$json$monthly$n_applied, 72L)
  expect_equal(r$json$registration$n_applied, 2L)

  # 重放
  r <- run("import_batch.R", c("--db", db, "--batch-id", "D01",
    "--monthly", file.path(demo, "monthly", "d01_monthly.csv"),
    "--registration", file.path(demo, "registration", "d01_registration.csv")))
  expect_true(r$json$replay)

  # 换内容用同 batch_id -> 拒绝
  r <- run("import_batch.R", c("--db", db, "--batch-id", "D01",
    "--monthly", file.path(demo, "monthly", "d02_monthly.csv")))
  expect_equal(r$status, 2L)
  expect_equal(r$json$error_code, "batch_replay_conflict")

  # 查询
  r <- run("query_ledger.R", c("--db", db, "--what", "snapshot"))
  expect_equal(r$json$source_id, "SRC-CLI")
  expect_equal(r$json$n_receipts, 1L)
  r <- run("query_ledger.R", c("--db", db, "--what", "current-monthly",
                               "--region-id", "R001"))
  expect_equal(r$json$n, 24L)
  r <- run("query_ledger.R", c("--db", db, "--what", "receipts",
                               "--batch-id", "D01"))
  expect_equal(r$json$n, 1L)
  r <- run("query_ledger.R", c("--db", db, "--what", "check"))
  expect_true(r$json$consistent)

  # 导出
  r <- run("export_panel.R", c("--db", db, "--out", exports))
  expect_equal(r$status, 0L)
  expect_equal(r$json$n_panel, 48L)
  expect_equal(r$json$n_missing_registration, 24L)
  expect_true(file.exists(file.path(r$json$release_dir, "panel.csv")))
  expect_true(file.exists(file.path(r$json$release_dir,
                                    "missing_registration.csv")))
  expect_true(file.exists(file.path(r$json$release_dir, "provenance.csv")))
  manifest <- jsonlite::fromJSON(file.path(r$json$release_dir,
                                           "manifest.json"))
  expect_equal(manifest$release_id, r$json$release_id)
  p <- data.table::fread(file.path(r$json$release_dir, "panel.csv"))
  expect_equal(names(p), PANEL_COLS)
  expect_true(all(p$treat %in% c(0L, 1L)))

  # 错误输入：缺列
  bad <- file.path(d, "bad.csv")
  writeLines(c("region_id,month,revision", "R1,2024-01,1"), bad)
  r <- run("import_batch.R", c("--db", db, "--batch-id", "X",
                               "--monthly", bad))
  expect_equal(r$status, 2L)
  expect_equal(r$json$error_code, "file_format_error")
})

test_that("缺少 --db 时给出可读错误", {
  root <- LEDGER_TEST_ROOT
  cmd <- paste(shQuote(LEDGER_TEST_R),
               shQuote(file.path(root, "R", "ledger", "import_batch.R")),
               "--batch-id X 2>/dev/null")
  out <- suppressWarnings(system(cmd, intern = TRUE))
  j <- jsonlite::fromJSON(out[length(out)])
  expect_equal(j$error_code, "missing_db_path")
})
