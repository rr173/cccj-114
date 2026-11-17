# =============================================================================
# boundary.R —— 提交边界与历史起点基线（schema v2）
#
# 两个核心概念：
#
# 1) 基线（baseline）：账本可精确重建的最早时点。
#    * 新建账本（fresh_init）：基线就是“空账本”状态，等价且可复核；
#    * v1 老账本升级（upgrade_v1）：升级事务内把升级瞬间的**当前有效**
#      月报/登记冻结到 baseline_monthly / baseline_registration，连同元数据
#      写入 baseline_meta。升级前的历史批次顺序在 v1 账本里没有被记录，
#      无法恢复，因此基线以前的旧 batch_id 一律按“不可重建的历史请求”拒绝，
#      绝不假称它们是可精确重建的边界。
#    基线固定为 commit_seq = 0，batch_id 用保留值 "__baseline__"。
#
# 2) 提交边界（commit_boundary）：基线之后，每个成功提交的导入批次在持有
#    BEGIN IMMEDIATE 写锁的同一事务内分配一个严格递增、无空洞的 commit_seq。
#    边界顺序只来自账本提交顺序——不使用文件名、调用时间（received_at）或
#    revision 大小推断。失败事务不写回执、不占序号；同 batch_id 同内容重传
#    返回原回执（replay），不新增边界；并发成功批次按各自拿到写锁的先后
#    得到唯一且可核对的顺序。
#
# 本模块所有函数都不自行开关事务：写函数的调用者必须已持有写事务
# （见 with_immediate_tx / ingest_tx_core），查询函数可在只读快照内调用。
# =============================================================================

BASELINE_ID <- "baseline-0"

# ---- 基线是否已存在（幂等判断，供升级重试使用）------------------------------
has_baseline <- function(con) {
  if (!has_table(con, "baseline_meta")) return(FALSE)
  DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM baseline_meta")$n > 0L
}

# 冻结当前有效状态到基线表。调用前提：
#   * v2 表已建好；
#   * 调用者持有写事务（升级事务或建库事务）；
#   * baseline_meta 为空（本函数不负责幂等，由调用者先用 has_baseline 判断）。
# origin: "fresh_init"（空起点）或 "upgrade_v1"（升级冻结点）。
capture_baseline_tx <- function(con, origin) {
  if (!origin %in% c("fresh_init", "upgrade_v1"))
    stop_ledger("internal_error", paste0("未知基线来源: ", origin))
  source_id <- ledger_source_id(con)
  now <- now_iso()

  m_cur <- DBI::dbGetQuery(con, "
    SELECT v.region_id, v.month, v.record_id, v.revision, v.op,
           v.y, v.x1, v.x2, v.batch_id, v.received_at, v.payload
      FROM monthly_current c
      JOIN monthly_version v ON v.record_id = c.record_id
     ORDER BY v.region_id, v.month")
  r_cur <- DBI::dbGetQuery(con, "
    SELECT v.region_id, v.record_id, v.revision, v.op,
           v.first_treat_month, v.batch_id, v.received_at, v.payload
      FROM registration_current c
      JOIN registration_version v ON v.record_id = c.record_id
     ORDER BY v.region_id")

  if (nrow(m_cur) > 0L) {
    DBI::dbAppendTable(con, "baseline_monthly", data.frame(
      region_id = as.character(m_cur$region_id),
      month = as.character(m_cur$month),
      record_id = as.integer(m_cur$record_id),
      revision = as.integer(m_cur$revision),
      op = as.character(m_cur$op),
      y = as.numeric(m_cur$y),
      x1 = as.numeric(m_cur$x1),
      x2 = as.numeric(m_cur$x2),
      batch_id = as.character(m_cur$batch_id),
      received_at = as.character(m_cur$received_at),
      payload = as.character(m_cur$payload),
      stringsAsFactors = FALSE))
  }
  if (nrow(r_cur) > 0L) {
    DBI::dbAppendTable(con, "baseline_registration", data.frame(
      region_id = as.character(r_cur$region_id),
      record_id = as.integer(r_cur$record_id),
      revision = as.integer(r_cur$revision),
      op = as.character(r_cur$op),
      first_treat_month = as.character(r_cur$first_treat_month),
      batch_id = as.character(r_cur$batch_id),
      received_at = as.character(r_cur$received_at),
      payload = as.character(r_cur$payload),
      stringsAsFactors = FALSE))
  }

  max_m <- DBI::dbGetQuery(con,
    "SELECT COALESCE(MAX(record_id),0) AS v FROM monthly_version")$v
  max_r <- DBI::dbGetQuery(con,
    "SELECT COALESCE(MAX(record_id),0) AS v FROM registration_version")$v

  note <- if (origin == "upgrade_v1") {
    paste0("v1->v2 升级时冻结的当前有效账本状态；升级前各批次的精确提交顺序",
           "在 v1 账本中没有记录、不可恢复，本基线是可精确重建的最早时点；",
           "基线以前的旧 batch_id 不属于可重建边界。")
  } else {
    paste0("新建账本的空起点基线；其后第一个成功提交的导入批次即成为",
           " commit_seq=1 的第一个可重建批次边界。")
  }
  DBI::dbExecute(con, "
    INSERT INTO baseline_meta(baseline_id, source_id, schema_version_at_capture,
      created_at, origin, n_monthly_current, n_registration_current,
      max_monthly_record_id, max_registration_record_id, snapshot_note)
    VALUES (?,?,?,?,?,?,?,?,?,?)",
    list(BASELINE_ID, source_id, LEDGER_SCHEMA_VERSION, now, origin,
         nrow(m_cur), nrow(r_cur), as.integer(max_m), as.integer(max_r), note))
  invisible(list(baseline_id = BASELINE_ID, origin = origin,
                 n_monthly = nrow(m_cur), n_registration = nrow(r_cur)))
}

# ---- 在成功导入事务内登记提交边界 ---------------------------------------------
# 调用前提：receipt 行已在同一事务写入（外键），且调用者持有写锁。
# 返回分配到的 commit_seq。严格递增、无空洞：写锁保证没有并发事务能同时
# 读到同一个 MAX(commit_seq)。
record_commit_boundary_tx <- function(con, batch_id, source_id, content_md5,
                                      committed_at) {
  seq <- DBI::dbGetQuery(con,
    "SELECT COALESCE(MAX(commit_seq),0) + 1 AS next_seq
       FROM commit_boundary")$next_seq
  DBI::dbExecute(con, "
    INSERT INTO commit_boundary(commit_seq, batch_id, source_id, file_md5,
      receipt_status, committed_at)
    VALUES (?,?,?,?,'committed',?)",
    list(as.integer(seq), batch_id, source_id, content_md5, committed_at))
  invisible(as.integer(seq))
}

# ---- 边界查询 -----------------------------------------------------------------
get_boundary_row <- function(con, batch_id) {
  r <- DBI::dbGetQuery(con, "
    SELECT commit_seq, batch_id, source_id, file_md5, receipt_status,
           committed_at
      FROM commit_boundary WHERE batch_id = ?", list(batch_id))
  if (nrow(r) == 0L) NULL else as.list(r[1, ])
}

# 全部可重建边界：基线（seq=0）+ 成功批次，按提交顺序。
query_boundaries <- function(con) {
  empty <- data.table(commit_seq = integer(), batch_id = character(),
                      kind = character(), source_id = character(),
                      origin = character(), created_at = character(),
                      file_md5 = character(), committed_at = character())
  base <- empty[0L]
  if (has_table(con, "baseline_meta")) {
    bm <- DBI::dbGetQuery(con,
      "SELECT source_id, origin, created_at
         FROM baseline_meta WHERE baseline_id = ?",
      list(BASELINE_ID))
    if (nrow(bm) > 0L)
      base <- data.table(commit_seq = 0L,
                         batch_id = REBUILD_BASELINE_BATCH_ID,
                         kind = BOUNDARY_KIND_BASELINE,
                         source_id = bm$source_id[[1]],
                         origin = bm$origin[[1]],
                         created_at = bm$created_at[[1]],
                         file_md5 = NA_character_,
                         committed_at = NA_character_)
  }
  batches <- empty[0L]
  if (has_table(con, "commit_boundary")) {
    cb <- as.data.table(DBI::dbGetQuery(con, "
      SELECT commit_seq, batch_id, source_id, file_md5, committed_at
        FROM commit_boundary ORDER BY commit_seq"))
    if (nrow(cb) > 0L)
      batches <- cb[, .(commit_seq = as.integer(commit_seq), batch_id,
                        kind = BOUNDARY_KIND_BATCH, source_id,
                        origin = NA_character_, created_at = NA_character_,
                        file_md5, committed_at)]
  }
  out <- rbind(base, batches, fill = TRUE)
  if (nrow(out) > 0L) setorder(out, commit_seq)
  out
}

# ---- 重建请求的边界校验 --------------------------------------------------------
# 决定一个 batch_id 能否作为重建目标，返回 list(
#   ok=TRUE, kind, commit_seq, boundary_row|baseline_meta) 或抛 ledger_error：
#     unknown_batch             账本回执中不存在该 batch_id
#     batch_not_committed       只存在失败/非 committed 记录（当前 schema 下
#                               失败事务不写回执，此分支主要面向未来状态）
#     batch_before_baseline     升级冻结点以前的旧 batch_id：顺序不可恢复
#     boundary_not_available    账本尚未完成 v2 基线（理论上不会发生）
resolve_rebuild_target <- function(con, batch_id) {
  if (is.null(batch_id) || length(batch_id) != 1L || is.na(batch_id) ||
      !nzchar(trimws(batch_id)))
    stop_ledger("missing_batch_id", "必须提供非空 --batch-id 指定重建边界")
  batch_id <- trimws(batch_id)
  if (!has_baseline(con))
    stop_ledger("boundary_not_available",
      "账本缺少 v2 历史起点基线；请先运行 R/ledger/init_ledger.R 完成升级")

  # 保留的基线标识：允许显式重建基线时点
  if (identical(batch_id, REBUILD_BASELINE_BATCH_ID) ||
      identical(batch_id, BASELINE_ID)) {
    bm <- DBI::dbGetQuery(con,
      "SELECT * FROM baseline_meta WHERE baseline_id = ?",
      list(BASELINE_ID))
    return(list(ok = TRUE, kind = BOUNDARY_KIND_BASELINE, commit_seq = 0L,
                batch_id = REBUILD_BASELINE_BATCH_ID, baseline = as.list(bm[1, ])))
  }

  receipt <- get_receipt(con, batch_id)
  if (is.null(receipt))
    stop_ledger("unknown_batch",
      paste0("batch_id 不属于当前账本（无回执）: ", batch_id),
      details = list(batch_id = batch_id,
                     hint = "可用 query_ledger.R --what boundaries 查看可重建边界"))
  if (!identical(receipt$status, "committed"))
    stop_ledger("batch_not_committed",
      paste0("batch_id 对应一次未成功提交的回执，不是重建边界: ", batch_id),
      details = list(batch_id = batch_id, status = receipt$status))

  brow <- get_boundary_row(con, batch_id)
  if (is.null(brow)) {
    # 回执存在且成功，但没有提交边界：只可能是升级冻结点之前的旧批次
    stop_ledger("batch_before_baseline",
      paste0("batch_id ", batch_id, " 是升级基线之前的历史批次；其精确提交",
             "顺序在 v1 账本中没有记录，不能假称为可重建时点。",
             "可重建范围从基线（", REBUILD_BASELINE_BATCH_ID,
             "）开始，之后的成功批次见 --what boundaries。"),
      details = list(batch_id = batch_id,
                     baseline_id = BASELINE_ID))
  }
  list(ok = TRUE, kind = BOUNDARY_KIND_BATCH,
       commit_seq = as.integer(brow$commit_seq), batch_id = batch_id,
       boundary = brow, receipt = receipt)
}
