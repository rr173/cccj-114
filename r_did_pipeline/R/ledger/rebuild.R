# =============================================================================
# rebuild.R —— 按已提交导入批次重建分析面板（schema v2，时间旅行只读路径）
#
# 目标：指定一个成功提交的批次边界，得到“该批次提交完成后账本所代表的
# 面板状态”，并产出与普通导出同形的 panel / 待补登记清单 / 逐行来源，
# 可直接交给现有分析管线重跑。
#
# 边界语义（与需求逐条对应）：
#  * 目标边界先经 resolve_rebuild_target 校验：必须是当前账本内的成功提交
#    边界（或保留的基线标识）；未知 batch_id、失败回执、升级基线以前的旧
#    batch_id 都被明确拒绝；
#  * 全部选择发生在**单个只读快照事务**（with_read_snapshot）内，期间其他
#    进程提交的更正不会渗入；边界之后到达的任何版本都不允许泄漏；
#  * 每个业务键在“截至该边界已提交”的版本中取最高 revision 决定状态，
#    与现行导出规则完全一致：delete 墓碑继续屏蔽，晚到的低 revision 不能
#    复活，更高 revision 的 upsert 可以恢复；
#  * 基线以前的状态只可能来自冻结基线（baseline_*），边界之后写入的版本行
#    用 commit_boundary.commit_seq 过滤；
#  * 重建是纯只读操作：不改写当前指针、版本历史、回执或提交边界，也不写
#    export_release（历史输出不会成为后续普通导出的当前状态）。
#
# 发布安全：写同级临时目录 -> 全部成功后目录重命名；若目标结果目录已存在
# 或任何写出环节失败，返回 ledger_error 且不覆盖调用者已有的结果目录。
# =============================================================================

REBUILD_REPORT_VERSION <- 1L

# ---- 截至某边界的有效月报（含 delete 墓碑）-----------------------------------
# 返回与 monthly_current JOIN monthly_version 同形的 data.table，
# 列：region_id, month, record_id, revision, op, y, x1, x2,
#     batch_id, received_at, payload, origin('baseline'|'version')
point_in_time_monthly <- function(con, commit_seq) {
  if (commit_seq <= 0L) {
    b <- DBI::dbGetQuery(con, "
      SELECT region_id, month, record_id, revision, op, y, x1, x2,
             batch_id, received_at, payload
        FROM baseline_monthly
       ORDER BY region_id, month")
    if (nrow(b) == 0L) {
      b <- data.table(region_id = character(), month = character(),
                      record_id = integer(), revision = integer(),
                      op = character(), y = numeric(), x1 = numeric(),
                      x2 = numeric(), batch_id = character(),
                      received_at = character(), payload = character())
    } else setDT(b)
    b[, origin := "baseline"]
    return(b)
  }
  # 基线冻结行（record_id <= 基线时的最大 id）与边界之前的版本行取并集，
  # 再按业务键选最高 revision。版本行用提交边界过滤而不是 received_at：
  # 时间戳可能同秒、可能回拨，只有 commit_seq 是账本提交顺序。
  bm <- DBI::dbGetQuery(con,
    "SELECT max_monthly_record_id AS v FROM baseline_meta
      WHERE baseline_id = ?", list(BASELINE_ID))
  base_max <- bm$v[[1]]
  v <- DBI::dbGetQuery(con, "
    SELECT v.region_id, v.month, v.record_id, v.revision, v.op,
           v.y, v.x1, v.x2, v.batch_id, v.received_at, v.payload,
           'version'  AS origin
      FROM monthly_version v
      JOIN commit_boundary b ON b.batch_id = v.batch_id
     WHERE b.commit_seq <= ?
    UNION ALL
    SELECT region_id, month, record_id, revision, op, y, x1, x2,
           batch_id, received_at, payload, 'baseline' AS origin
      FROM baseline_monthly
     WHERE record_id <= ?",
    list(as.integer(commit_seq), as.integer(base_max)))
  if (nrow(v) == 0L)
    return(data.table(region_id = character(), month = character(),
                      record_id = integer(), revision = integer(),
                      op = character(), y = numeric(), x1 = numeric(),
                      x2 = numeric(), batch_id = character(),
                      received_at = character(), payload = character(),
                      origin = character()))
  setDT(v)
  # 同 (region_id,month) 可能同时存在基线行与版本行（升级后该键又有新提交）：
  # 最高 revision 决定状态；revision 相同时版本行与基线行内容必然一致
  # （升级冻结的就是当时 current），优先取版本行以保留真实 record_id 链。
  v[, rnk := frank(-revision, ties.method = "min"), by = .(region_id, month)]
  top <- v[rnk == 1L]
  # 理论上同 revision 不会有两种 payload（写入路径已保证），这里再兜底去重
  top <- top[order(origin == "baseline")]   # 并列时 version 行在前
  top <- top[!duplicated(top, by = c("region_id", "month"))]
  top[, rnk := NULL]
  setorder(top, region_id, month)
  top
}

point_in_time_registration <- function(con, commit_seq) {
  if (commit_seq <= 0L) {
    b <- DBI::dbGetQuery(con, "
      SELECT region_id, record_id, revision, op, first_treat_month,
             batch_id, received_at, payload
        FROM baseline_registration
       ORDER BY region_id")
    if (nrow(b) == 0L) {
      b <- data.table(region_id = character(), record_id = integer(),
                      revision = integer(), op = character(),
                      first_treat_month = character(),
                      batch_id = character(), received_at = character(),
                      payload = character())
    } else setDT(b)
    b[, origin := "baseline"]
    return(b)
  }
  bm <- DBI::dbGetQuery(con,
    "SELECT max_registration_record_id AS v FROM baseline_meta
      WHERE baseline_id = ?", list(BASELINE_ID))
  base_max <- bm$v[[1]]
  v <- DBI::dbGetQuery(con, "
    SELECT v.region_id, v.record_id, v.revision, v.op, v.first_treat_month,
           v.batch_id, v.received_at, v.payload, 'version' AS origin
      FROM registration_version v
      JOIN commit_boundary b ON b.batch_id = v.batch_id
     WHERE b.commit_seq <= ?
    UNION ALL
    SELECT region_id, record_id, revision, op, first_treat_month,
           batch_id, received_at, payload, 'baseline' AS origin
      FROM baseline_registration
     WHERE record_id <= ?",
    list(as.integer(commit_seq), as.integer(base_max)))
  if (nrow(v) == 0L)
    return(data.table(region_id = character(), record_id = integer(),
                      revision = integer(), op = character(),
                      first_treat_month = character(),
                      batch_id = character(), received_at = character(),
                      payload = character(), origin = character()))
  setDT(v)
  v[, rnk := frank(-revision, ties.method = "min"), by = region_id]
  top <- v[rnk == 1L]
  top <- top[order(origin == "baseline")]   # 并列时 version 行在前
  top <- top[!duplicated(top, by = "region_id")]
  top[, rnk := NULL]
  setorder(top, region_id)
  top
}

# ---- 在固定快照内按边界拼面板（沿用 compute_export_tables 的面板规则）--------
# 直接复用现行规则的判定逻辑，只把“数据源”从 *_current 换成时间旅行视图，
# 从而保证重建面板与“当时做普通导出”得到的结果逐格一致。
compute_rebuild_tables <- function(con, commit_seq) {
  m <- point_in_time_monthly(con, commit_seq)
  r <- point_in_time_registration(con, commit_seq)
  if (nrow(m) == 0L) return(empty_export_tables())

  m <- m[op == "upsert"]   # delete 墓碑不进面板（与现行 WHERE m.op='upsert' 一致）
  if (nrow(m) == 0L) return(empty_export_tables())

  r_eff <- r[, .(region_id, reg_record_id = record_id,
                 reg_revision = revision, reg_op = op,
                 reg_first_treat_month = first_treat_month,
                 reg_batch_id = batch_id, reg_received_at = received_at)]
  raw <- merge(m, r_eff, by = "region_id", all.x = TRUE, sort = FALSE)
  for (col in c("reg_record_id", "reg_revision"))
    set(raw, j = col, value = as.integer(raw[[col]]))
  for (col in c("y", "x1", "x2"))
    set(raw, j = col, value = as.numeric(raw[[col]]))
  for (col in c("region_id", "month", "batch_id", "received_at", "payload",
                "reg_op", "reg_first_treat_month", "reg_batch_id",
                "reg_received_at"))
    set(raw, j = col, value = as.character(raw[[col]]))

  effective <- !is.na(raw$reg_record_id) & !is.na(raw$reg_op) &
    raw$reg_op == "upsert" & !is.na(raw$reg_first_treat_month) &
    nzchar(raw$reg_first_treat_month)
  joined <- raw[effective]
  missing <- raw[!effective]

  panel <- data.table(
    region_id = joined$region_id, month = joined$month,
    y = joined$y, x1 = joined$x1, x2 = joined$x2,
    first_treat_month = joined$reg_first_treat_month)
  panel[, treat := data.table::fifelse(
    panel$first_treat_month == "never", 0L,
    as.integer(panel$month >= panel$first_treat_month))]

  prov <- joined[, .(region_id, month,
                     monthly_record_id = record_id,
                     monthly_revision = revision,
                     monthly_batch_id = batch_id,
                     monthly_received_at = received_at,
                     registration_record_id = reg_record_id,
                     registration_revision = reg_revision,
                     registration_batch_id = reg_batch_id,
                     registration_received_at = reg_received_at)]

  if (nrow(missing) == 0L) {
    miss <- data.table(region_id = character(), month = character(),
                       monthly_record_id = integer(),
                       monthly_revision = integer(),
                       monthly_batch_id = character(), reason = character())
    miss_summary <- empty_export_tables()$missing_summary
  } else {
    miss <- missing[, .(region_id, month, monthly_record_id = record_id,
                        monthly_revision = revision,
                        monthly_batch_id = batch_id,
                        registration_record_id = reg_record_id,
                        registration_op = reg_op)]
    miss[, reason := data.table::fcase(
      is.na(registration_record_id), "no_registration",
      registration_op == "delete", "registration_withdrawn",
      default = "registration_incomplete")]
    miss_summary <- miss[, .(
      n_months = .N, first_month = min(month), last_month = max(month),
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

# 结果目录名：确定性命名（同一账本同一边界 -> 同名），重复重建命中“目录已
# 存在”保护而不是静默覆盖；需要保留多次结果时由调用者换 --out 根目录。
rebuild_dir_name <- function(batch_id, commit_seq) {
  safe <- gsub("[^A-Za-z0-9._:-]", "_", batch_id)
  paste0("rebuild-seq", sprintf("%010d", commit_seq), "-", safe)
}

# 收集边界身份信息（写入 manifest，供复核者核对账本/来源与文件校验值）
rebuild_identity <- function(con, target, tables, dv) {
  source_id <- ledger_source_id(con)
  bm <- DBI::dbGetQuery(con,
    "SELECT * FROM baseline_meta WHERE baseline_id = ?", list(BASELINE_ID))
  boundaries <- query_boundaries(con)
  n_boundary_batches <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM commit_boundary")$n
  # 显式逐行 list，避免 jsonlite 把单行/单元素 data.table 展平成原子向量
  boundary_order <- lapply(seq_len(nrow(boundaries)), function(i) {
    row <- boundaries[i]
    list(commit_seq = as.integer(row$commit_seq),
         batch_id = row$batch_id,
         kind = row$kind,
         source_id = row$source_id,
         origin = if (is.na(row$origin)) NULL else row$origin,
         created_at = if (is.na(row$created_at)) NULL else row$created_at,
         committed_at = if (is.na(row$committed_at)) NULL
                        else row$committed_at,
         file_md5 = if (is.na(row$file_md5)) NULL else row$file_md5)
  })
  identity <- list(
    report_type = "panel_rebuild",
    report_version = REBUILD_REPORT_VERSION,
    source_id = source_id,
    db_path = ledger_path_of(con),
    db_identity = list(
      # 库文件内容指纹：每次连接可能不同（WAL），因此只记录路径、大小与
      # 账本自报身份，真正可核对的是下面的边界顺序与回执 md5。
      sqlite_data_version = dv,
      schema_version = LEDGER_SCHEMA_VERSION),
    target = list(
      kind = target$kind,
      batch_id = target$batch_id,
      commit_seq = target$commit_seq),
    baseline = list(
      baseline_id = bm$baseline_id[[1]],
      origin = bm$origin[[1]],
      created_at = bm$created_at[[1]],
      n_monthly_current = bm$n_monthly_current[[1]],
      n_registration_current = bm$n_registration_current[[1]],
      snapshot_note = bm$snapshot_note[[1]]),
    boundary_order = boundary_order,
    n_rebuildable_batch_boundaries = n_boundary_batches)
  if (target$kind == BOUNDARY_KIND_BATCH) {
    identity$target$receipt <- list(
      batch_id = target$receipt$batch_id,
      source_id = target$receipt$source_id,
      status = target$receipt$status,
      file_md5 = target$receipt$file_md5,
      received_at = target$receipt$received_at)
    identity$target$boundary_row <- target$boundary
  }
  identity
}

# ---- 主入口 ------------------------------------------------------------------
# opts: list(out_dir, batch_id)
rebuild_panel <- function(con, opts) {
  t0 <- Sys.time()
  out_dir <- opts$out_dir
  if (is.null(out_dir) || !nzchar(out_dir))
    stop_ledger("missing_output_dir", "必须提供 --out 重建结果根目录")
  target <- resolve_rebuild_target(con, opts$batch_id)
  # 先校验输出根目录可创建/可规范化，避免在非法路径上留下半成品
  tryCatch({
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_dir <- normalizePath(out_dir, mustWork = TRUE)
  }, error = function(e)
    stop_ledger("invalid_output_dir",
                paste0("重建输出根目录不可用: ", opts$out_dir,
                       " | ", conditionMessage(e))))

  final_dir <- file.path(out_dir,
                         rebuild_dir_name(target$batch_id, target$commit_seq))
  # 目录已存在：绝不覆盖调用者已有的重建结果
  if (dir.exists(final_dir) || file.exists(final_dir))
    stop_ledger("rebuild_output_exists",
      paste0("重建结果目录已存在，为避免覆盖既有结果请更换 --out 或先移除: ",
             final_dir),
      details = list(existing_dir = final_dir,
                     batch_id = target$batch_id,
                     commit_seq = target$commit_seq))

  dv <- NULL
  tables <- with_read_snapshot(con, function() {
    # 校验与读取必须在同一个固定快照里：校验后、读取前别的进程即使提交了
    # 新批次，本快照也看不到，保证选择的是同一账本状态。
    again <- resolve_rebuild_target(con, opts$batch_id)
    if (!identical(again$commit_seq, target$commit_seq))
      stop_ledger("boundary_changed_during_rebuild",
                  "重建期间目标边界发生变化，请重试")
    dv <<- DBI::dbGetQuery(con, "PRAGMA data_version")[[1]]
    compute_rebuild_tables(con, target$commit_seq)
  })

  staging <- file.path(out_dir,
    paste0(".staging-rebuild-", target$commit_seq, "-", Sys.getpid()))
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
    stop_ledger("rebuild_write_failed",
                paste0("写重建临时目录失败: ", conditionMessage(e)))
  })

  created_at <- now_iso()
  file_meta <- lapply(files, function(f) {
    p <- file.path(staging, f)
    list(name = f, md5 = unname(tools::md5sum(p)),
         n_rows = length(readLines(p, warn = FALSE)) - 1L)
  })
  identity <- rebuild_identity(con, target, tables, dv)
  manifest <- c(identity, list(
    created_at = created_at,
    panel_columns = PANEL_COLS,
    n_panel = nrow(tables$panel),
    n_missing_registration = nrow(tables$missing),
    n_missing_registration_regions = nrow(tables$missing_summary),
    n_provenance = nrow(tables$provenance),
    files = file_meta,
    side_effects = "read_only",
    side_effects_note = paste0(
      "重建为纯只读操作：未改写当前指针、版本历史、回执或提交边界，",
      "也未登记 export_release；历史输出不会成为后续普通导出的当前状态。")))
  tryCatch(
    jsonlite::write_json(manifest, file.path(staging, "manifest.json"),
                         auto_unbox = TRUE, pretty = TRUE, null = "null",
                         digits = NA),
    error = function(e)
      stop_ledger("rebuild_write_failed",
                  paste0("写重建 manifest 失败: ", conditionMessage(e))))

  published <- file.rename(staging, final_dir)
  if (!isTRUE(published))
    stop_ledger("rebuild_publish_failed",
                paste0("发布重建结果失败（重命名）: ", staging, " -> ",
                       final_dir))
  ok <- TRUE

  list(rebuild_dir = final_dir,
       batch_id = target$batch_id,
       boundary_kind = target$kind,
       commit_seq = target$commit_seq,
       source_id = identity$source_id,
       baseline_origin = identity$baseline$origin,
       created_at = created_at,
       sqlite_data_version = dv,
       n_panel = nrow(tables$panel),
       n_missing_registration = nrow(tables$missing),
       n_missing_registration_regions = nrow(tables$missing_summary),
       n_provenance = nrow(tables$provenance),
       files = lapply(file_meta, function(fm) file.path(final_dir, fm$name)),
       file_md5 = lapply(file_meta, function(fm) fm$md5),
       elapsed_ms = as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000)
}
