# =============================================================================
# query.R —— 账本查询：当前数据、历史版本、回执与逐条批次判定
# 所有查询都不加写锁，可与其他进程的导入/导出并行。
# =============================================================================

query_current_monthly <- function(con, region_id = NULL, month = NULL) {
  where <- "WHERE 1=1"
  args <- list()
  if (!is.null(region_id)) {
    where <- paste(where, "AND v.region_id = ?"); args <- c(args, list(region_id))
  }
  if (!is.null(month)) {
    where <- paste(where, "AND v.month = ?"); args <- c(args, list(month))
  }
  sql <- paste0("
    SELECT v.region_id, v.month, v.revision, v.op, v.y, v.x1, v.x2,
           v.batch_id, v.received_at, v.record_id
      FROM monthly_current c JOIN monthly_version v ON v.record_id = c.record_id
     ", where, "
     ORDER BY v.region_id, v.month")
  as.data.table(if (length(args) > 0L) DBI::dbGetQuery(con, sql, args)
                else DBI::dbGetQuery(con, sql))
}

query_current_registration <- function(con, region_id = NULL) {
  where <- "WHERE 1=1"
  args <- list()
  if (!is.null(region_id)) {
    where <- paste(where, "AND v.region_id = ?"); args <- c(args, list(region_id))
  }
  sql <- paste0("
    SELECT v.region_id, v.revision, v.op, v.first_treat_month,
           v.batch_id, v.received_at, v.record_id
      FROM registration_current c JOIN registration_version v
        ON v.record_id = c.record_id
     ", where, "
     ORDER BY v.region_id")
  as.data.table(if (length(args) > 0L) DBI::dbGetQuery(con, sql, args)
                else DBI::dbGetQuery(con, sql))
}

# 历史：每个业务键的全部版本（含过时来件与已撤回的 delete 行），按 revision。
query_monthly_history <- function(con, region_id = NULL, month = NULL) {
  where <- character()
  args <- list()
  if (!is.null(region_id)) {
    where <- c(where, "region_id = ?"); args <- c(args, list(region_id))
  }
  if (!is.null(month)) {
    where <- c(where, "month = ?"); args <- c(args, list(month))
  }
  sql <- paste(
    "SELECT record_id, region_id, month, revision, op, y, x1, x2,",
    "       batch_id, received_at, payload",
    "  FROM monthly_version",
    if (length(where) > 0L) paste(" WHERE", paste(where, collapse = " AND ")),
    " ORDER BY region_id, month, revision")
  as.data.table(if (length(args) > 0L) DBI::dbGetQuery(con, sql, args)
                else DBI::dbGetQuery(con, sql))
}

query_registration_history <- function(con, region_id = NULL) {
  where <- character()
  args <- list()
  if (!is.null(region_id)) {
    where <- c(where, "region_id = ?"); args <- c(args, list(region_id))
  }
  sql <- paste(
    "SELECT record_id, region_id, revision, op, first_treat_month,",
    "       batch_id, received_at, payload",
    "  FROM registration_version",
    if (length(where) > 0L) paste(" WHERE", paste(where, collapse = " AND ")),
    " ORDER BY region_id, revision")
  as.data.table(if (length(args) > 0L) DBI::dbGetQuery(con, sql, args)
                else DBI::dbGetQuery(con, sql))
}

query_receipts <- function(con, batch_id = NULL, limit = 100L) {
  args <- list()
  sql <- paste(
    "SELECT batch_id, source_id, file_md5, status, received_at, summary_json",
    "  FROM receipt")
  if (!is.null(batch_id)) {
    sql <- paste(sql, "WHERE batch_id = ?"); args <- list(batch_id)
  } else {
    sql <- paste(sql, "ORDER BY received_at DESC, batch_id LIMIT ?")
    args <- list(as.integer(limit))
  }
  as.data.table(DBI::dbGetQuery(con, sql, args))
}

query_outcomes <- function(con, batch_id = NULL, limit = 5000L) {
  args <- list()
  sql <- paste(
    "SELECT batch_id, kind, region_id, month, revision, outcome, csv_row",
    "  FROM ingest_outcome")
  if (!is.null(batch_id)) {
    sql <- paste(sql, "WHERE batch_id = ?"); args <- list(batch_id)
  } else {
    sql <- paste(sql, "ORDER BY outcome_id DESC LIMIT ?")
    args <- list(as.integer(limit))
  }
  as.data.table(DBI::dbGetQuery(con, sql, args))
}

# 过时/重复来件的原始内容（不改变当前版本，但可追溯）
query_arrivals <- function(con, batch_id = NULL, limit = 5000L) {
  args <- list()
  sql <- paste(
    "SELECT batch_id, kind, region_id, month, revision, op, y, x1, x2,",
    "       first_treat_month, outcome, csv_row, received_at",
    "  FROM arrival_log")
  if (!is.null(batch_id)) {
    sql <- paste(sql, "WHERE batch_id = ?"); args <- list(batch_id)
  } else {
    sql <- paste(sql, "ORDER BY arrival_id DESC LIMIT ?")
    args <- list(as.integer(limit))
  }
  as.data.table(DBI::dbGetQuery(con, sql, args))
}
query_releases <- function(con, limit = 50L) {
  as.data.table(DBI::dbGetQuery(con, "
    SELECT release_id, source_id, created_at, monthly_n, registration_n,
           json_extract(manifest_json, '$.sqlite_data_version') AS data_version
      FROM export_release
     ORDER BY created_at DESC, release_id DESC
     LIMIT ?", list(as.integer(limit))))
}

ledger_snapshot <- function(con) {
  meta <- ledger_meta(con)
  list(
    source_id = meta$source_id,
    schema_version = as.integer(meta$schema_version),
    created_at = meta$created_at,
    db_path = ledger_path_of(con),
    journal_mode = DBI::dbGetQuery(con, "PRAGMA journal_mode")[[1]],
    n_receipts = DBI::dbGetQuery(con,
      "SELECT COUNT(*) AS n FROM receipt")$n,
    n_monthly_versions = DBI::dbGetQuery(con,
      "SELECT COUNT(*) AS n FROM monthly_version")$n,
    n_monthly_current = DBI::dbGetQuery(con,
      "SELECT COUNT(*) AS n FROM monthly_current")$n,
    n_registration_versions = DBI::dbGetQuery(con,
      "SELECT COUNT(*) AS n FROM registration_version")$n,
    n_registration_current = DBI::dbGetQuery(con,
      "SELECT COUNT(*) AS n FROM registration_current")$n,
    n_releases = DBI::dbGetQuery(con,
      "SELECT COUNT(*) AS n FROM export_release")$n)
}

# ---- 一致性自检（用于测试与运维）--------------------------------------------
# 检出不变量：
#  1. 每个 current 指针的 revision 必须是该键的历史最高 revision；
#  2. 每个 current 指针必须指向同键版本；
#  3. 版本行的 op/内容约束（delete 无内容）。
check_ledger_invariants <- function(con) {
  bad <- list()
  m <- DBI::dbGetQuery(con, "
    SELECT c.region_id, c.month, v.revision AS cur_rev, t.max_rev
      FROM monthly_current c
      JOIN monthly_version v ON v.record_id = c.record_id
      JOIN (SELECT region_id, month, MAX(revision) AS max_rev
              FROM monthly_version GROUP BY region_id, month) t
        ON t.region_id = c.region_id AND t.month = c.month
     WHERE v.revision <> t.max_rev")
  if (nrow(m) > 0L) bad$monthly_current_not_max <- m
  r <- DBI::dbGetQuery(con, "
    SELECT c.region_id, v.revision AS cur_rev, t.max_rev
      FROM registration_current c
      JOIN registration_version v ON v.record_id = c.record_id
      JOIN (SELECT region_id, MAX(revision) AS max_rev
              FROM registration_version GROUP BY region_id) t
        ON t.region_id = c.region_id
     WHERE v.revision <> t.max_rev")
  if (nrow(r) > 0L) bad$registration_current_not_max <- r
  d1 <- DBI::dbGetQuery(con, "
    SELECT record_id FROM monthly_version
     WHERE op = 'delete' AND (y IS NOT NULL OR x1 IS NOT NULL OR x2 IS NOT NULL)")
  if (nrow(d1) > 0L) bad$monthly_delete_has_payload <- d1
  d2 <- DBI::dbGetQuery(con, "
    SELECT record_id FROM registration_version
     WHERE op = 'delete' AND first_treat_month IS NOT NULL")
  if (nrow(d2) > 0L) bad$registration_delete_has_payload <- d2
  bare_receipts <- DBI::dbGetQuery(con, "
    SELECT r.batch_id FROM receipt r
     WHERE NOT EXISTS (SELECT 1 FROM ingest_outcome o WHERE o.batch_id = r.batch_id)")
  # 允许“只有重复行/空应用”的批次没有 outcome，但 committed 批次必须至少
  # 有版本行或 outcome 之一（防止空事务也留回执，这不是错误，只做提示）。
  list(consistent = length(bad) == 0L, problems = bad,
       receipts_without_outcomes = bare_receipts$batch_id)
}
