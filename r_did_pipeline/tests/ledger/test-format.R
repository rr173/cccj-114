# =============================================================================
# test-format.R —— 文件格式、月份/修订号/数值校验、批内冲突
# =============================================================================

test_that("合法月份边界", {
  expect_true(is_valid_month("2024-01"))
  expect_true(is_valid_month("2024-12"))
  expect_false(is_valid_month("2024-13"))
  expect_false(is_valid_month("2024-00"))
  expect_false(is_valid_month("2024-1"))
  expect_false(is_valid_month("2024-01-01"))
  expect_false(is_valid_month("2024/01"))
  expect_false(is_valid_month(""))
  expect_false(is_valid_month("2023-02-30"))
  expect_false(is_valid_month("2024-02-29"))  # 不是月份格式，非闰年日期
  expect_true(is_valid_month("2024-02"))
})

test_that("revision 必须是严格正整数", {
  expect_equal(parse_revision("1", 2)$value, 1L)
  expect_equal(parse_revision(" 7 ", 2)$value, 7L)
  for (bad in c("0", "-1", "+1", "1.0", "01", "abc", "", "1e3",
                "999999999999"))
    expect_false(is.null(parse_revision(bad, 2)$error), info = bad)
})

test_that("数值允许显式缺失但拒绝非数值字符串与无穷值", {
  expect_true(is.na(parse_numeric_cell("", 2, "y")$value))
  expect_true(is.na(parse_numeric_cell("NA", 2, "y")$value))
  expect_equal(parse_numeric_cell("-1.5e2", 2, "y")$value, -150)
  expect_false(is.null(parse_numeric_cell("abc", 2, "y")$error))
  expect_false(is.null(parse_numeric_cell("Inf", 2, "y")$error))
  expect_false(is.null(parse_numeric_cell("-Inf", 2, "y")$error))
  expect_false(is.null(parse_numeric_cell("NaN", 2, "y")$error))
  expect_false(is.null(parse_numeric_cell("1,5", 2, "y")$error))
  expect_false(is.null(parse_numeric_cell("1e999", 2, "y")$error))
})

test_that("first_treat_month 只接受 YYYY-MM 或 never", {
  expect_equal(parse_first_treat_cell("never", 2)$value, "never")
  expect_equal(parse_first_treat_cell("2023-08", 2)$value, "2023-08")
  expect_false(is.null(parse_first_treat_cell("NeVeR", 2)$error))
  expect_false(is.null(parse_first_treat_cell("2023-13", 2)$error))
  expect_false(is.null(parse_first_treat_cell("2023-08-01", 2)$error))
})

test_that("表头必须恰好匹配，多余或缺少列都拒绝", {
  d <- tempfile(); dir.create(d)
  p <- write_csv_lines(file.path(d, "a.csv"),
    c("region_id,month,revision,op,y,x1,x2,extra",
      "R1,2024-01,1,upsert,1,0,0,x"))
  expect_error(parse_batch_file(p, LEDGER_KIND_MONTHLY), "表头必须恰好")
  p2 <- write_csv_lines(file.path(d, "b.csv"),
    c("region_id,month,revision,op,y,x1", "R1,2024-01,1,upsert,1,0"))
  expect_error(parse_batch_file(p2, LEDGER_KIND_MONTHLY), "表头必须恰好")
  p3 <- write_csv_lines(file.path(d, "c.csv"), c("region_id,month"))
  expect_error(parse_batch_file(p3, LEDGER_KIND_MONTHLY), "没有数据行")
})

test_that("delete 行不能携带业务内容", {
  d <- tempfile(); dir.create(d)
  p <- write_csv_lines(file.path(d, "x.csv"),
    c("region_id,month,revision,op,y,x1,x2", "R1,2024-01,2,delete,1,,"))
  err <- tryCatch(parse_batch_file(p, LEDGER_KIND_MONTHLY),
                  ledger_error = function(e) e)
  expect_equal(err$code, "file_validation_error")
  p2 <- write_csv_lines(file.path(d, "y.csv"),
    c("region_id,revision,op,first_treat_month", "R1,2,delete,2024-01"))
  err2 <- tryCatch(parse_batch_file(p2, LEDGER_KIND_REGISTRATION),
                   ledger_error = function(e) e)
  expect_equal(err2$code, "file_validation_error")
})

test_that("批内同键同版本内容一致折叠为重复，不同内容整批失败", {
  d <- tempfile(); dir.create(d)
  dup <- write_csv_lines(file.path(d, "dup.csv"),
    c("region_id,month,revision,op,y,x1,x2",
      "R1,2024-01,2,upsert,1.5,NA,2",
      "R1,2024-01,2,upsert,1.5,NA,2"))
  p <- parse_batch_file(dup, LEDGER_KIND_MONTHLY)
  expect_equal(nrow(p$rows), 1L)
  expect_equal(p$n_duplicate_in_file, 1L)

  # 即使同批还有更高版本，也必须报冲突
  bad <- write_csv_lines(file.path(d, "bad.csv"),
    c("region_id,month,revision,op,y,x1,x2",
      "R1,2024-01,2,upsert,1.5,NA,2",
      "R1,2024-01,2,upsert,1.6,NA,2",
      "R1,2024-01,3,upsert,9,9,9"))
  err <- tryCatch(parse_batch_file(bad, LEDGER_KIND_MONTHLY),
                  ledger_error = function(e) e)
  expect_equal(err$code, "same_revision_conflict")
})

test_that("显式缺失与零/字面 NA 不被混淆", {
  d <- tempfile(); dir.create(d)
  p <- write_csv_lines(file.path(d, "n.csv"),
    c("region_id,month,revision,op,y,x1,x2",
      "R1,2024-01,1,upsert,,NA,0"))
  x <- parse_batch_file(p, LEDGER_KIND_MONTHLY)$rows
  expect_true(is.na(x$y))
  expect_true(is.na(x$x1))
  expect_equal(x$x2, 0)
})

test_that("同一业务键不同 revision 的行不会互相折叠", {
  d <- tempfile(); dir.create(d)
  p <- write_csv_lines(file.path(d, "r.csv"),
    c("region_id,month,revision,op,y,x1,x2",
      "R1,2024-01,1,upsert,1,1,1",
      "R1,2024-01,2,upsert,2,2,2",
      "R1,2024-02,1,upsert,3,3,3"))
  x <- parse_batch_file(p, LEDGER_KIND_MONTHLY)$rows
  expect_equal(nrow(x), 3L)
})
