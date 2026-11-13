# =============================================================================
# test-revision-semantics.R —— 随机到达顺序下的修订语义不变量
# 只做数据拼接层面的核对，不涉及 did 估计。
# =============================================================================

test_that("任意到达顺序下：当前版本 = 最高 revision，且等于该顺序的参考实现", {
  set.seed(20241113)
  for (trial in seq_len(10)) {
    L <- new_temp_ledger()
    d <- L$dir
    keys <- data.table(region_id = paste0("R", 1:6),
                       month = rep(c("2024-01", "2024-02"), each = 3))
    # 每个键生成 1..4 的版本，随机打乱行的到达顺序
    versions <- rbindlist(lapply(seq_len(nrow(keys)), function(i) {
      k <- keys[i]
      nrev <- sample(1:4, 1)
      data.table(region_id = k$region_id, month = k$month,
                 revision = seq_len(nrev),
                 y = round(runif(nrev), 6), x1 = 0, x2 = 0, op = "upsert")
    }))
    versions <- versions[!duplicated(versions, by = c("region_id", "month",
                                                      "revision"))]
    set.seed(1000 + trial)
    versions <- versions[sample(.N)]
    # 参考实现：每个键取最高 revision 的 y（同版本不允许不同内容）
    ref <- versions[order(revision), .SD[.N], by = c("region_id", "month")]
    # 逐行分成多个批次提交，打乱批次内与批次间顺序
    chunks <- split(versions, sample(seq_len(nrow(versions)) %% 5 + 1))
    for (b in seq_along(chunks)) {
      f <- file.path(d, paste0("batch_", b, ".csv"))
      fwrite(chunks[[b]][, .(region_id, month, revision, op, y, x1, x2)], f)
      r <- ingest_batch(L$con, list(monthly = f, registration = NULL),
                        list(batch_id = paste0("B", trial, "_", b)))
      expect_equal(r$status, "committed")
    }
    cur <- query_current_monthly(L$con)
    setorder(cur, region_id, month)
    setorder(ref, region_id, month)
    expect_equal(cur$region_id, ref$region_id)
    expect_equal(cur$month, ref$month)
    expect_equal(cur$revision, ref$revision)
    expect_equal(cur$y, ref$y)
    expect_true(check_ledger_invariants(L$con)$consistent)
    L$close()
  }
})

test_that("低版本永远不会覆盖高版本（history 与 current 同时核对）", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  for (r in c(5L, 2L, 9L, 1L, 7L, 3L)) {
    f <- monthly_file(d, paste0("r", r),
                      mrows("R1", "2024-01", r, y = r))
    ingest_batch(L$con, list(monthly = f, registration = NULL),
                 list(batch_id = paste0("B", r)))
  }
  # 另一个批次再次报送 revision 9 的相同内容：duplicate，不新增版本
  f9b <- monthly_file(d, "r9_again", mrows("R1", "2024-01", 9L, y = 9))
  r9b <- ingest_batch(L$con, list(monthly = f9b, registration = NULL),
                      list(batch_id = "B9_again"))
  expect_equal(r9b$outcomes[[1]]$outcome, "duplicate")
  cur <- query_current_monthly(L$con)
  expect_equal(cur$revision, 9L)
  expect_equal(cur$y, 9)
  hist <- query_monthly_history(L$con, "R1", "2024-01")
  expect_equal(sort(hist$revision), c(5L, 9L))  # 版本表按 revision 唯一
  # 同版本同内容的第二次到达记为 duplicate，进入 arrival_log
  dup <- query_arrivals(L$con)
  expect_equal(nrow(dup[revision == 9 & outcome == "duplicate"]), 1L)})

test_that("delete 之后低版本 upsert 与 delete 都不会复活", {
  L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
  d <- L$dir
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "a", mrows("R1", "2024-01", 3L, y = 3)),
         registration = NULL), list(batch_id = "B1"))
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "b", mrows("R1", "2024-01", 4L,
      op = "delete", y = NA, x1 = NA, x2 = NA)), registration = NULL),
    list(batch_id = "B2"))
  for (r in c(1L, 2L, 3L)) {
    ingest_batch(L$con,
      list(monthly = monthly_file(d, paste0("u", r),
        mrows("R1", "2024-01", r, y = r)), registration = NULL),
      list(batch_id = paste0("U", r)))
  }
  cur <- query_current_monthly(L$con)
  expect_equal(cur$op, "delete")
  expect_equal(cur$revision, 4L)
  t <- compute_export_tables(L$con)
  expect_equal(nrow(t$panel), 0L)
  # 只有更高 revision 的 upsert 才能恢复
  ingest_batch(L$con,
    list(monthly = monthly_file(d, "r5", mrows("R1", "2024-01", 5L, y = 5)),
         registration = NULL), list(batch_id = "B5"))
  expect_equal(query_current_monthly(L$con)$op, "upsert")
  expect_equal(query_current_monthly(L$con)$revision, 5L)
})

test_that("到达顺序不影响最终结果（同内容不同顺序）", {
  build <- function(order) {
    L <- new_temp_ledger(); on.exit(L$close(), add = TRUE)
    d <- L$dir
    for (r in order) {
      ingest_batch(L$con,
        list(monthly = monthly_file(d, paste0("r", r),
          mrows("R1", c("2024-01", "2024-02"), r, y = r)),
          registration = registration_file(d, paste0("reg", r),
            rrows("R1", r, "upsert",
                  if (r %% 2 == 0) "2024-02" else "never"))),
        list(batch_id = paste0("B", r)))
    }
    t <- compute_export_tables(L$con)
    L$close()
    t
  }
  a <- build(c(1L, 2L, 3L, 4L))
  b <- build(c(4L, 1L, 3L, 2L))
  expect_equal(a$panel, b$panel)
  expect_equal(a$provenance[, .(region_id, month, monthly_revision,
                                registration_revision)],
               b$provenance[, .(region_id, month, monthly_revision,
                                registration_revision)])
})
