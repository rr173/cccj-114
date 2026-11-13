# =============================================================================
# export.R —— 一致读取视图内生成：地区月表、待补登记清单、逐行来源
#
# 关键规则：
#  * 只输出“当前有效月报”（op=upsert）与“当前有效登记”（op=upsert）；
#  * 有月报但没有有效登记（从未登记 / 登记已撤回）的地区不出现在月表，
#    逐月列入 missing_registration.csv，绝不默认 never；
#  * 登记更正后，下一次导出对同一地区所有现存月份重算 treat：
#      first_treat_month = never -> treat = 0
#      否则 treat = as.integer(month >= first_treat_month)
#  * 真实缺月保持缺行，数值缺失保持空单元格，不补观测；
#  * 四个输出在同一只读事务（固定快照）中查询，写临时目录，全部成功后才
#    以目录重命名发布；任何失败都不触碰调用者此前已有的导出目录。
# =============================================================================

PANEL_COLS <- c("region_id", "month", "y", "x1", "x2",
                "first_treat_month", "treat")
MISSING_COLS <- c("region_id", "month", "monthly_record_id",
                  "monthly_revision", "monthly_batch_id", "reason")
PROVENANCE_COLS <- c(
  "region_id", "month",
  "monthly_record_id", "monthly_revision", "monthly_batch_id",
  "monthly_received_at",
  "registration_record_id", "registration_revision",
  "registration_batch_id", "registration_received_at")

# 只读快照事务：BEGIN 后的第一条查询固定快照；
# 期间其他进程提交的批次不会渗入本次导出的任何输出。
with_read_snapshot <- function(con, code) {
  DBI::dbExecute(con, "BEGIN")
  ok <- FALSE
  on.exit({
    if (!ok) try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
  }, add = TRUE)
  res <- code()
  DBI::dbExecute(con, "COMMIT")
  ok <- TRUE
  res
}

empty_export_tables <- function() {
  list(
    panel = data.table(region_id = character(), month = character(),
                       y = numeric(), x1 = numeric(), x2 = numeric(),
                       first_treat_month = character(), treat = integer()),
    provenance = data.table(region_id = character(), month = character(),
                            monthly_record_id = integer(),
                            monthly_revision = integer(),
                            monthly_batch_id = character(),
                            monthly_received_at = character(),
                            registration_record_id = integer(),
                            registration_revision = integer(),
                            registration_batch_id = character(),
                            registration_received_at = character()),
    missing = data.table(region_id = character(), month = character(),
                         monthly_record_id = integer(),
                         monthly_revision = integer(),
                         monthly_batch_id = character(),
                         reason = character()),
    missing_summary = data.table(region_id = character(), n_months = integer(),
                                 first_month = character(),
                                 last_month = character(),
                                 months = character(), reason = character()))
}

compute_export_tables <- function(con) {
  q <- "
    SELECT m.region_id, m.month, m.y, m.x1, m.x2,
           m.record_id   AS monthly_record_id,
           m.revision    AS monthly_revision,
           m.batch_id    AS monthly_batch_id,
           m.received_at AS monthly_received_at,
           r.record_id   AS registration_record_id,
           r.revision    AS registration_revision,
           r.batch_id    AS registration_batch_id,
           r.received_at AS registration_received_at,
           r.op          AS registration_op,
           r.first_treat_month
      FROM monthly_current mc
      JOIN monthly_version m ON m.record_id = mc.record_id
      LEFT JOIN registration_current rc ON rc.region_id = mc.region_id
      LEFT JOIN registration_version r ON r.record_id = rc.record_id
     WHERE m.op = 'upsert'
     ORDER BY m.region_id, m.month"
  raw <- as.data.table(DBI::dbGetQuery(con, q))
  if (nrow(raw) == 0L) return(empty_export_tables())

  # JSON 往返统一类型：当前表里没有行时，LEFT JOIN 的 NULL 列可能整列为
  # logical，导致后续比较/写盘出现意外的 type 转换。
  for (col in c("registration_record_id", "registration_revision"))
    set(raw, j = col, value = as.integer(raw[[col]]))
  for (col in c("y", "x1", "x2"))
    set(raw, j = col, value = as.numeric(raw[[col]]))
  for (col in c("region_id", "month", "monthly_batch_id",
                "monthly_received_at", "registration_batch_id",
                "registration_received_at", "registration_op",
                "first_treat_month"))
    set(raw, j = col, value = as.character(raw[[col]]))

  # 有效登记 = 当前登记存在、op=upsert、first_treat_month 合法且非空
  effective <- !is.na(raw$registration_record_id) &
    !is.na(raw$registration_op) & raw$registration_op == "upsert" &
    !is.na(raw$first_treat_month) & nzchar(raw$first_treat_month)
  joined <- raw[effective]
  missing <- raw[!effective]

  panel <- data.table(
    region_id = joined$region_id,
    month = joined$month,
    y = joined$y, x1 = joined$x1, x2 = joined$x2,
    first_treat_month = joined$first_treat_month)
  panel[, treat := data.table::fifelse(
    panel$first_treat_month == "never", 0L,
    as.integer(panel$month >= panel$first_treat_month))]

  prov <- joined[, .(region_id, month,
                     monthly_record_id, monthly_revision, monthly_batch_id,
                     monthly_received_at,
                     registration_record_id, registration_revision,
                     registration_batch_id, registration_received_at)]

  # 待补登记清单按地区汇总时，同时标明原因：从未登记 / 登记已撤回 /
  # 登记内容不完整（后者只可能来自旧版本数据，当前写入路径不会产生）
  miss <- missing[, .(region_id, month, monthly_record_id,
                      monthly_revision, monthly_batch_id,
                      registration_record_id, registration_op)]
  if (nrow(miss) == 0L) {
    miss <- data.table(region_id = character(), month = character(),
                       monthly_record_id = integer(),
                       monthly_revision = integer(),
                       monthly_batch_id = character(),
                       reason = character())
    miss_summary <- empty_export_tables()$missing_summary
  } else {
    miss[, reason := data.table::fcase(
      is.na(registration_record_id), "no_registration",
      registration_op == "delete", "registration_withdrawn",
      default = "registration_incomplete")]
    miss_summary <- miss[, .(
      n_months = .N,
      first_month = min(month),
      last_month = max(month),
      months = paste(month, collapse = " "),
      reason = paste(unique(reason), collapse = "+")), by = region_id]
    setorder(miss_summary, region_id)
    miss[, c("registration_record_id", "registration_op") := NULL]
  }

  setcolorder(panel, PANEL_COLS)
  setcolorder(prov, PROVENANCE_COLS)
  setcolorder(miss, MISSING_COLS)
  if (nrow(panel) > 0L) setorder(panel, region_id, month)
  if (nrow(prov) > 0L) setorder(prov, region_id, month)
  if (nrow(miss) > 0L) setorder(miss, region_id, month)
  list(panel = panel, provenance = prov,
       missing = miss, missing_summary = miss_summary)
}

# 发布 ID：时间戳 + 输出内容哈希（同一内容重复导出得到不同时间戳，
# 目录不会互相覆盖；同时 contents_hash 可用于比对一致性）。
release_id_for <- function(tables, source_id) {
  key <- paste(
    "source", source_id,
    "panel", paste(capture.output(data.table::fwrite(tables$panel)),
                   collapse = "\n"),
    "missing", paste(capture.output(data.table::fwrite(tables$missing)),
                     collapse = "\n"),
    sep = "\n")
  h <- substr(text_hash(key), 1, 8)
  # 时间戳 + 进程号：多个导出进程即使同一秒也不会争用同一目录名
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%OS3", tz = "UTC")
  stamp <- gsub("\\.", "", stamp)
  paste0("rel-", stamp, "-p", Sys.getpid(), "-", h)
}

write_table_strict <- function(dt, path) {
  data.table::fwrite(dt, path, na = "")
  invisible(path)
}

# opts: list(out_dir, release_tag = NULL)
export_panel <- function(con, opts) {
  t0 <- Sys.time()
  source_id <- ledger_source_id(con)
  out_dir <- opts$out_dir
  if (is.null(out_dir) || !nzchar(out_dir))
    stop_ledger("missing_output_dir", "必须提供 --out 导出根目录")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir <- normalizePath(out_dir, mustWork = TRUE)

  dv <- NULL
  snapshot_meta <- NULL
  tables <- with_read_snapshot(con, function() {
    dv <<- DBI::dbGetQuery(con, "PRAGMA data_version")[[1]]
    snapshot_meta <<- list(
      max_monthly_record_id = DBI::dbGetQuery(con,
        "SELECT COALESCE(MAX(record_id),0) AS v FROM monthly_version")$v,
      max_registration_record_id = DBI::dbGetQuery(con,
        "SELECT COALESCE(MAX(record_id),0) AS v FROM registration_version")$v,
      n_monthly_versions = DBI::dbGetQuery(con,
        "SELECT COUNT(*) AS v FROM monthly_version")$v,
      n_registration_versions = DBI::dbGetQuery(con,
        "SELECT COUNT(*) AS v FROM registration_version")$v)
    compute_export_tables(con)
  })

  release_id <- release_id_for(tables, source_id)
  if (!is.null(opts$release_tag) && nzchar(opts$release_tag))
    release_id <- paste0(release_id, "-", opts$release_tag)
  # 同一秒内重复导出相同内容时，追加序号而不是覆盖或报错
  base_id <- release_id
  n <- 1L
  while (file.exists(file.path(out_dir, release_id))) {
    n <- n + 1L
    release_id <- paste0(base_id, "-", n)
  }
  final_dir <- file.path(out_dir, release_id)
  staging <- file.path(out_dir,
    paste0(".staging-", release_id, "-", Sys.getpid()))
  if (file.exists(staging)) unlink(staging, recursive = TRUE, force = TRUE)
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)
  ok <- FALSE
  on.exit({
    if (!ok && dir.exists(staging))
      unlink(staging, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  files <- list(
    panel = "panel.csv",
    missing_registration = "missing_registration.csv",
    missing_registration_summary = "missing_registration_summary.csv",
    provenance = "provenance.csv")
  tryCatch({
    write_table_strict(tables$panel, file.path(staging, files$panel))
    write_table_strict(tables$missing,
                       file.path(staging, files$missing_registration))
    write_table_strict(tables$missing_summary,
                       file.path(staging, files$missing_registration_summary))
    write_table_strict(tables$provenance, file.path(staging, files$provenance))
  }, error = function(e) {
    stop_ledger("export_write_failed",
                paste0("写临时导出目录失败: ", conditionMessage(e)))
  })

  created_at <- now_iso()
  file_meta <- lapply(files, function(f) {
    p <- file.path(staging, f)
    list(name = f, md5 = unname(tools::md5sum(p)),
         n_rows = length(readLines(p, warn = FALSE)) - 1L)
  })
  manifest <- list(
    release_id = release_id,
    source_id = source_id,
    created_at = created_at,
    schema_version = LEDGER_SCHEMA_VERSION,
    db_path = ledger_path_of(con),
    sqlite_data_version = dv,
    snapshot_boundary = snapshot_meta,
    panel_columns = PANEL_COLS,
    n_panel = nrow(tables$panel),
    n_missing_registration = nrow(tables$missing),
    n_missing_registration_regions = nrow(tables$missing_summary),
    n_provenance = nrow(tables$provenance),
    files = file_meta)
  jsonlite::write_json(manifest, file.path(staging, "manifest.json"),
                       auto_unbox = TRUE, pretty = TRUE, null = "null")

  # ---- 发布：目录重命名是原子操作；失败时此前导出完全不受影响 ------------
  published <- file.rename(staging, final_dir)
  if (!isTRUE(published))
    stop_ledger("publish_failed",
                paste0("发布导出失败（重命名）: ", staging, " -> ", final_dir))
  ok <- TRUE

  # ---- 登记发布记录（独立短事务；失败则撤回这次发布，保留旧导出）----------
  manifest_json <- jsonlite::toJSON(manifest, auto_unbox = TRUE,
                                    null = "null", digits = NA)
  tryCatch({
    with_immediate_tx(con, function() {
      DBI::dbExecute(con, "
        INSERT INTO export_release(release_id, source_id, created_at,
          monthly_n, registration_n, manifest_json)
        VALUES (?,?,?,?,?,?)",
        list(release_id, source_id, created_at,
             nrow(tables$panel), nrow(tables$missing),
             as.character(manifest_json)))
      for (fm in file_meta) {
        DBI::dbExecute(con, "
          INSERT INTO export_file(release_id, file_name, md5, n_rows)
          VALUES (?,?,?,?)",
          list(release_id, fm$name, fm$md5, fm$n_rows))
      }
    })
  }, error = function(e) {
    unlink(final_dir, recursive = TRUE, force = TRUE)
    stop_ledger("release_record_failed",
                paste0("登记发布记录失败，已撤回本次导出: ",
                       conditionMessage(e)))
  })

  list(release_id = release_id,
       release_dir = final_dir,
       source_id = source_id,
       created_at = created_at,
       sqlite_data_version = dv,
       n_panel = nrow(tables$panel),
       n_missing_registration = nrow(tables$missing),
       n_missing_registration_regions = nrow(tables$missing_summary),
       n_provenance = nrow(tables$provenance),
       files = lapply(file_meta, function(fm)
         file.path(final_dir, fm$name)),
       elapsed_ms = as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000)
}
