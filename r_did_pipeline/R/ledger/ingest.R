# =============================================================================
# ingest.R —— 批次导入：回执、修订号判定、当前有效记录维护
#
# 一次导入 = 一个显式 BEGIN IMMEDIATE 事务，事务内依次：
#   1. 校验 source_id 与批次编号；同 (source_id,batch_id) 已有成功回执时：
#        - 文件内容（两类文件的 md5 组合）相同 -> 直接返回原回执，不重复生效；
#        - 内容不同 -> 拒绝；
#   2. 逐类处理：按业务键取库内最高 revision，按 revision 升序逐条比较：
#        - 更高 revision：写入版本行；最后一条推进当前指针；
#        - 相等且内容一致：记为 duplicate，不写版本行；
#        - 相等且内容冲突：整批失败（回滚，且不占用 batch_id）；
#        - 更低 revision：记为 stale，保留在 arrival_log，不改变当前记录；
#   3. 回执（receipt）、版本行、逐条判定、过时/重复来件同一事务提交。
#
# 事务是“全部成功或完全不可见”的单元；失败批次不写 receipt，
# 因此修正文件后用同一 batch_id 可以重新导入。
# =============================================================================

# ---- 空批次占位表：保证后续 nrow() 等标量判断不会得到 NULL -----------------
empty_incoming <- function(kind) {
  if (kind == LEDGER_KIND_MONTHLY)
    data.table(region_id = character(), month = character(),
               revision = integer(), op = character(),
               y = numeric(), x1 = numeric(), x2 = numeric(),
               payload = character(), csv_row = integer())
  else
    data.table(region_id = character(), revision = integer(),
               op = character(), first_treat_month = character(),
               payload = character(), csv_row = integer())
}

# ---- 批次文件内容身份 --------------------------------------------------------
batch_content_md5 <- function(monthly_info, registration_info) {
  parts <- c(
    if (is.null(monthly_info)) "monthly=-" else
      paste0("monthly=", monthly_info$md5),
    if (is.null(registration_info)) "registration=-" else
      paste0("registration=", registration_info$md5)
  )
  text_hash(paste(parts, collapse = "\n"))
}

# ---- 回执读取 / 重传检查 ------------------------------------------------------
get_receipt <- function(con, batch_id) {
  r <- DBI::dbGetQuery(con,
    "SELECT batch_id, source_id, file_md5, status, summary_json, received_at
       FROM receipt WHERE batch_id = ?", list(batch_id))
  if (nrow(r) == 0L) return(NULL)
  list(batch_id = r$batch_id[[1]], source_id = r$source_id[[1]],
       file_md5 = r$file_md5[[1]], status = r$status[[1]],
       summary = jsonlite::fromJSON(r$summary_json[[1]],
                                    simplifyVector = FALSE),
       received_at = r$received_at[[1]])
}

check_replay <- function(con, source_id, batch_id, content_md5) {
  rec <- get_receipt(con, batch_id)
  if (is.null(rec)) return(NULL)
  if (!identical(rec$status, "committed"))
    stop_ledger("batch_id_reused_after_failure",
                paste0("batch_id 已被一次失败回执占用: ", batch_id))
  if (!identical(rec$source_id, source_id))
    stop_ledger("batch_id_other_source",
                paste0("batch_id ", batch_id, " 已属于 source_id=",
                       rec$source_id))
  if (!identical(rec$file_md5, content_md5))
    stop_ledger("batch_replay_conflict",
                paste0("batch_id ", batch_id,
                       " 已成功导入过不同内容的文件；拒绝覆盖。",
                       "请使用新的 batch_id 提交修正文件"),
                details = list(expected_md5 = rec$file_md5,
                               received_md5 = content_md5))
  rec$replay <- TRUE
  rec
}

# ---- 库内当前状态 ------------------------------------------------------------
load_monthly_state <- function(con) {
  cur <- DBI::dbGetQuery(con, "
    SELECT v.region_id, v.month, v.revision, v.op, v.payload
      FROM monthly_current c JOIN monthly_version v ON v.record_id = c.record_id")
  mx <- DBI::dbGetQuery(con, "
    SELECT region_id, month, MAX(revision) AS max_revision
      FROM monthly_version GROUP BY region_id, month")
  setDT(cur); setDT(mx)
  list(current = cur, max_rev = mx)
}

load_registration_state <- function(con) {
  cur <- DBI::dbGetQuery(con, "
    SELECT v.region_id, v.revision, v.op, v.payload
      FROM registration_current c JOIN registration_version v
        ON v.record_id = c.record_id")
  mx <- DBI::dbGetQuery(con, "
    SELECT region_id, MAX(revision) AS max_revision
      FROM registration_version GROUP BY region_id")
  setDT(cur); setDT(mx)
  list(current = cur, max_rev = mx)
}

# ---- 单类判定 ----------------------------------------------------------------
# 语义：同一批内的行等价于“按 revision 升序逐条到达”，与文件行序无关：
#   * 升序链上每条高于库内最高版本的记录都进入版本表（历史可查）；
#   * 链上低于某条更高版本的行不会被跳过，而是作为该键的中间版本保存；
#   * 同键同 revision 出现两种内容：无论本批是否还有更高版本，整批失败
#     （parse_batch_file 已拦截批内冲突，这里再校验与库内当前版本的冲突）。
# 返回 list(incoming=待写版本行, outcomes=逐行判定, n_* 统计)
# outcome 取 insert / replace / restore / delete / duplicate / stale。
classify_rows <- function(rows, state, kind, key_cols) {
  empty_cur <- if (kind == LEDGER_KIND_MONTHLY)
    data.table(region_id = character(), month = character(),
               cur_revision = integer(), cur_payload = character(),
               cur_op = character())
  else
    data.table(region_id = character(), cur_revision = integer(),
               cur_payload = character(), cur_op = character())
  cur <- state$current
  cur_key <- if (nrow(cur) > 0L) {
    if (kind == LEDGER_KIND_MONTHLY)
      cur[, .(region_id, month, cur_revision = revision,
              cur_payload = payload, cur_op = op)]
    else
      cur[, .(region_id, cur_revision = revision,
              cur_payload = payload, cur_op = op)]
  } else empty_cur
  mx <- state$max_rev
  if (nrow(mx) == 0L) {
    mx <- if (kind == LEDGER_KIND_MONTHLY)
      data.table(region_id = character(), month = character(),
                 max_revision = integer()) else
      data.table(region_id = character(), max_revision = integer())
  }

  j <- merge(rows, cur_key, by = key_cols, all.x = TRUE, sort = FALSE)
  j <- merge(j, mx, by = key_cols, all.x = TRUE, sort = FALSE)
  j[is.na(max_revision), max_revision := 0L]

  # 与库内当前版本同 revision 不同内容 -> 整批失败（先于任何写入判断）
  eq <- j[revision == max_revision]
  eq_conflict <- eq[is.na(cur_payload) | payload != cur_payload]
  # 批内同版本不同内容（parse_batch_file 已拦截，这里兜底）
  dup_conflict <- j[, .(n = .N, n_p = uniqueN(payload)),
                    by = c(key_cols, "revision")][n_p > 1L]
  if (nrow(dup_conflict) > 0L)
    stop_ledger("same_revision_conflict",
                "同一批次内同一业务键存在相同 revision 的不同内容；整批不导入")
  if (nrow(eq_conflict) > 0L) {
    details <- lapply(seq_len(min(nrow(eq_conflict), 20L)), function(i) {
      ri <- eq_conflict[i]
      list(region_id = ri$region_id,
           month = if ("month" %in% key_cols) ri$month else NULL,
           revision = ri$revision,
           batch_csv_row = ri$csv_row,
           stored_payload = ri$cur_payload,
           incoming_payload = ri$payload)
    })
    txt <- paste(vapply(seq_len(nrow(eq_conflict)), function(i) {
      ri <- eq_conflict[i]
      paste0(ri$region_id,
             if ("month" %in% key_cols) paste0("/", ri$month) else "",
             "@r", ri$revision)
    }, character(1)), collapse = ", ")
    stop_ledger("revision_content_conflict",
                paste0("revision 与库内当前版本相同但内容不同: ", txt,
                       "；整批回滚"),
                details = details)
  }

  # 按业务键 + revision 升序遍历，模拟逐条到达；同 revision 的行已被折叠
  j[, seq_id := seq_len(.N)]
  if ("month" %in% key_cols)
    data.table::setorder(j, region_id, month, revision, seq_id)
  else
    data.table::setorder(j, region_id, revision, seq_id)
  key_str <- do.call(paste, c(lapply(key_cols, function(k) j[[k]]),
                              list(sep = "\r")))
  keys_unique <- unique(key_str)
  k_idx <- match(key_str, keys_unique)
  # 每个键的初始状态：库内当前版本（无则 revision=0），按键索引共享
  first_occurrence <- match(keys_unique, key_str)
  st_rev <- integer(length(keys_unique))
  st_payload <- character(length(keys_unique))
  st_op <- character(length(keys_unique))
  st_rev[] <- ifelse(is.na(j$cur_revision[first_occurrence]), 0L,
                     as.integer(j$cur_revision[first_occurrence]))
  st_payload[] <- ifelse(is.na(j$cur_payload[first_occurrence]), "",
                         j$cur_payload[first_occurrence])
  st_op[] <- ifelse(is.na(j$cur_op[first_occurrence]), "",
                    j$cur_op[first_occurrence])

  n <- nrow(j)
  outcome <- character(n)
  for (i in seq_len(n)) {
    k <- k_idx[i]
    rev_i <- as.integer(j$revision[i])
    if (rev_i < st_rev[k]) {
      outcome[i] <- "stale"
      next
    }
    if (rev_i == st_rev[k]) {
      outcome[i] <- if (identical(j$payload[i], st_payload[k]))
        "duplicate" else "conflict"   # 理论上前置检查已拦截
      next
    }
    outcome[i] <- if (j$op[i] == "delete") "delete"
      else if (st_rev[k] == 0L) "insert"
      else if (st_op[k] == "delete") "restore"
      else "replace"
    st_rev[k] <- rev_i
    st_payload[k] <- j$payload[i]
    st_op[k] <- j$op[i]
  }
  if (any(outcome == "conflict"))
    stop_ledger("revision_content_conflict",
                "存在与库内版本相同 revision 但内容不同的记录；整批回滚")
  j[, outcome := outcome]

  incoming <- j[outcome %in% c("insert", "replace", "restore", "delete")]
  if (kind == LEDGER_KIND_MONTHLY) {
    incoming <- incoming[, .(region_id, month, revision, op, y, x1, x2,
                             payload, csv_row)]
    outcomes <- j[, .(region_id, month, revision, op, y, x1, x2,
                      payload, csv_row, outcome)]
  } else {
    incoming <- incoming[, .(region_id, revision, op, first_treat_month,
                             payload, csv_row)]
    outcomes <- j[, .(region_id, revision, op, first_treat_month,
                      payload, csv_row, outcome)]
  }
  data.table::setorder(outcomes, csv_row)
  # 写入顺序必须按业务键内 revision 升序：当前指针只由最后一次写入决定，
  # 因此每个键最后写入的必须就是批内最高版本。
  if (kind == LEDGER_KIND_MONTHLY)
    data.table::setorder(incoming, region_id, month, revision, csv_row)
  else
    data.table::setorder(incoming, region_id, revision, csv_row)
  list(incoming = incoming, outcomes = outcomes,
       n_duplicate = outcomes[outcome == "duplicate", .N],
       n_stale = outcomes[outcome == "stale", .N],
       n_applied = nrow(incoming))
}

# ---- 写版本行与当前指针 ------------------------------------------------------
# 批量插入：单进程已持有 BEGIN IMMEDIATE 写锁，AUTOINCREMENT 在该事务内连续，
# 因此新 record_id 必定是 (插入前 MAX, 插入前 MAX + n]。使用 dbAppendTable
# 一次绑定，避免逐行执行 + last_insert_rowid() 往返。
max_record_id <- function(con, table) {
  DBI::dbGetQuery(con, paste0("SELECT COALESCE(MAX(record_id), 0) AS v FROM ",
                              table))$v
}

write_version_rows <- function(con, incoming, kind, batch_id, received_at) {
  if (nrow(incoming) == 0L) return(invisible(integer(0)))
  table <- if (kind == LEDGER_KIND_MONTHLY) "monthly_version"
           else "registration_version"
  before <- max_record_id(con, table)
  if (kind == LEDGER_KIND_MONTHLY) {
    df <- data.frame(
      region_id = as.character(incoming$region_id),
      month = as.character(incoming$month),
      revision = as.integer(incoming$revision),
      op = as.character(incoming$op),
      y = as.numeric(incoming$y),
      x1 = as.numeric(incoming$x1),
      x2 = as.numeric(incoming$x2),
      batch_id = batch_id,
      received_at = received_at,
      payload = as.character(incoming$payload),
      stringsAsFactors = FALSE)
  } else {
    df <- data.frame(
      region_id = as.character(incoming$region_id),
      revision = as.integer(incoming$revision),
      op = as.character(incoming$op),
      first_treat_month = as.character(incoming$first_treat_month),
      batch_id = batch_id,
      received_at = received_at,
      payload = as.character(incoming$payload),
      stringsAsFactors = FALSE)
  }
  DBI::dbAppendTable(con, table, df)
  ids <- seq.int(before + 1L, before + nrow(df))
  # 写锁内不会有其他写入者；仍然核对一次，避免依赖 AUTOINCREMENT 假设时静默错位
  after <- max_record_id(con, table)
  if (after != before + nrow(df) || after != max(ids))
    stop_ledger("internal_id_mismatch",
                paste0("版本行 record_id 推进异常: before=", before,
                       " after=", after, " n=", nrow(df)))
  # 当前指针：先写入事务内临时表，再一条 upsert 语句合并
  cur_table <- if (kind == LEDGER_KIND_MONTHLY) "monthly_current"
               else "registration_current"
  tmp <- paste0("_stage_", cur_table)
  if (kind == LEDGER_KIND_MONTHLY) {
    stage <- data.frame(region_id = df$region_id, month = df$month,
                        record_id = ids, stringsAsFactors = FALSE)
  } else {
    stage <- data.frame(region_id = df$region_id, record_id = ids,
                        stringsAsFactors = FALSE)
  }
  DBI::dbWriteTable(con, tmp, stage, temporary = TRUE, overwrite = TRUE)
  # 同一批可能给同一键写入多个版本（中间版本进入历史）；SQLite 的
  # INSERT ... ON CONFLICT 对同一语句内的重复键只保留任意一行，因此
  # 当前指针必须按“每个键的最后一条（批内最高 version）”显式取。
  if (kind == LEDGER_KIND_MONTHLY) {
    DBI::dbExecute(con, paste0("
      INSERT INTO ", cur_table, "(region_id, month, record_id)
      SELECT s.region_id, s.month, s.record_id
        FROM ", tmp, " s
        JOIN (SELECT region_id, month, MAX(record_id) AS record_id
                FROM ", tmp, " GROUP BY region_id, month) t
          ON t.region_id = s.region_id AND t.month = s.month
         AND t.record_id = s.record_id
       WHERE 1
      ON CONFLICT(region_id, month) DO UPDATE SET record_id = excluded.record_id"))
  } else {
    DBI::dbExecute(con, paste0("
      INSERT INTO ", cur_table, "(region_id, record_id)
      SELECT s.region_id, s.record_id
        FROM ", tmp, " s
        JOIN (SELECT region_id, MAX(record_id) AS record_id
                FROM ", tmp, " GROUP BY region_id) t
          ON t.region_id = s.region_id AND t.record_id = s.record_id
       WHERE 1
      ON CONFLICT(region_id) DO UPDATE SET record_id = excluded.record_id"))
  }
  invisible(ids)
}

# 逐条判定写入（账本可查每批每一行的结果）
write_outcome_rows <- function(con, outcomes, kind, batch_id) {
  if (nrow(outcomes) == 0L) return(invisible(NULL))
  df <- data.frame(
    batch_id = batch_id, kind = kind,
    region_id = as.character(outcomes$region_id),
    month = if (kind == LEDGER_KIND_MONTHLY)
      as.character(outcomes$month) else NA_character_,
    revision = as.integer(outcomes$revision),
    outcome = as.character(outcomes$outcome),
    csv_row = as.integer(outcomes$csv_row),
    stringsAsFactors = FALSE)
  DBI::dbAppendTable(con, "ingest_outcome", df)
  invisible(NULL)
}

# 过时/重复来件保留完整内容，便于“旧版本和过时来件仍可在账本查询”
write_arrival_rows <- function(con, outcomes, kind, batch_id, received_at) {
  keep <- outcomes[outcome %in% c("stale", "duplicate")]
  if (nrow(keep) == 0L) return(invisible(NULL))
  if (kind == LEDGER_KIND_MONTHLY) {
    df <- data.frame(
      batch_id = batch_id, kind = kind,
      region_id = as.character(keep$region_id),
      month = as.character(keep$month),
      revision = as.integer(keep$revision),
      op = as.character(keep$op),
      y = as.numeric(keep$y), x1 = as.numeric(keep$x1), x2 = as.numeric(keep$x2),
      first_treat_month = NA_character_,
      payload = as.character(keep$payload),
      outcome = as.character(keep$outcome),
      csv_row = as.integer(keep$csv_row),
      received_at = received_at, stringsAsFactors = FALSE)
  } else {
    df <- data.frame(
      batch_id = batch_id, kind = kind,
      region_id = as.character(keep$region_id),
      month = NA_character_,
      revision = as.integer(keep$revision),
      op = as.character(keep$op),
      y = NA_real_, x1 = NA_real_, x2 = NA_real_,
      first_treat_month = as.character(keep$first_treat_month),
      payload = as.character(keep$payload),
      outcome = as.character(keep$outcome),
      csv_row = as.integer(keep$csv_row),
      received_at = received_at, stringsAsFactors = FALSE)
  }
  DBI::dbAppendTable(con, "arrival_log", df)
  invisible(NULL)
}

# ---- 主入口 ------------------------------------------------------------------
# files: list(monthly = path|NULL, registration = path|NULL)
# opts:  list(batch_id, source_id = NULL, interrupt_at = NULL,
#             pause_before_commit = NULL, pause_marker = NULL)
ingest_batch <- function(con, files, opts) {
  t0 <- Sys.time()
  started_at <- now_iso()
  source_id <- ledger_source_id(con)
  if (!is.null(opts$source_id) && !identical(opts$source_id, source_id))
    stop_ledger("source_id_conflict",
                paste0("--source-id 与库绑定值不一致: 库=", source_id,
                       " 请求=", opts$source_id))
  monthly <- if (!is.null(files$monthly))
    parse_batch_file(files$monthly, LEDGER_KIND_MONTHLY) else NULL
  registration <- if (!is.null(files$registration))
    parse_batch_file(files$registration, LEDGER_KIND_REGISTRATION) else NULL
  contract <- validate_batch_contract(opts$batch_id, source_id,
                                      monthly, registration)
  batch_id <- contract$batch_id
  content_md5 <- batch_content_md5(monthly, registration)

  # 原文件重传：不写任何东西，直接返回原回执（形状与首次提交一致）
  replay <- check_replay(con, source_id, batch_id, content_md5)
  if (!is.null(replay)) {
    out <- replay$summary
    out$batch_id <- replay$batch_id
    out$source_id <- replay$source_id
    out$status <- replay$status
    out$received_at <- replay$received_at
    out$replay <- TRUE
    out$elapsed_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
    return(out)
  }

  received_at <- now_iso()
  applied <- list(monthly = integer(0), registration = integer(0))
  result <- with_immediate_tx(con, function() {
    # 写锁内二次检查：两个进程同时重传同一批次时，后者在这里看到已提交回执
    again <- check_replay(con, source_id, batch_id, content_md5)
    if (!is.null(again)) {
      out <- again$summary
      out$replay <- TRUE
      return(out)
    }

    m_state <- load_monthly_state(con)
    r_state <- load_registration_state(con)
    m <- if (!is.null(monthly))
      classify_rows(monthly$rows, m_state, LEDGER_KIND_MONTHLY,
                    c("region_id", "month")) else
      list(incoming = empty_incoming(LEDGER_KIND_MONTHLY),
           outcomes = classify_outcomes_empty(LEDGER_KIND_MONTHLY),
           n_duplicate = 0L, n_stale = 0L, n_applied = 0L)
    r <- if (!is.null(registration))
      classify_rows(registration$rows, r_state, LEDGER_KIND_REGISTRATION,
                    "region_id") else
      list(incoming = empty_incoming(LEDGER_KIND_REGISTRATION),
           outcomes = classify_outcomes_empty(LEDGER_KIND_REGISTRATION),
           n_duplicate = 0L, n_stale = 0L, n_applied = 0L)

    m_ids <- write_version_rows(con, m$incoming, LEDGER_KIND_MONTHLY,
                                batch_id, received_at)
    r_ids <- write_version_rows(con, r$incoming, LEDGER_KIND_REGISTRATION,
                                batch_id, received_at)
    applied$monthly <<- m_ids
    applied$registration <<- r_ids

    summary <- list(
      batch_id = batch_id,
      source_id = source_id,
      status = "committed",
      received_at = received_at,
      file_md5 = content_md5,
      monthly = list(
        n_rows_in_file = if (is.null(monthly)) 0L else monthly$n_rows_raw,
        n_duplicate_in_file =
          if (is.null(monthly)) 0L else monthly$n_duplicate_in_file,
        n_applied = m$n_applied, n_stale = m$n_stale,
        n_duplicate = m$n_duplicate,
        n_delete = if (nrow(m$incoming) > 0L)
          as.integer(sum(m$incoming$op == "delete")) else 0L,
        file_md5 = if (is.null(monthly)) NULL else monthly$md5),
      registration = list(
        n_rows_in_file = if (is.null(registration)) 0L
                         else registration$n_rows_raw,
        n_duplicate_in_file =
          if (is.null(registration)) 0L
          else registration$n_duplicate_in_file,
        n_applied = r$n_applied, n_stale = r$n_stale,
        n_duplicate = r$n_duplicate,
        n_delete = if (nrow(r$incoming) > 0L)
          as.integer(sum(r$incoming$op == "delete")) else 0L,
        file_md5 = if (is.null(registration)) NULL else registration$md5),
      outcomes = list())
    # 逐条判定里只把「非平凡」结果写进回执（insert/replace/restore/delete/stale），
    # 完整的逐行表由 ingest_outcome 与 arrival_log 提供。
    nontrivial <- list()
    for (o in list(list(kind = "monthly", dt = m$outcomes),
                   list(kind = "registration", dt = r$outcomes))) {
      dt <- o$dt
      if (nrow(dt) == 0L) next
      dt <- dt[outcome != "insert"]  # insert 是常态，避免回执过大
      for (i in seq_len(nrow(dt))) {
        ri <- dt[i]
        nontrivial[[length(nontrivial) + 1L]] <- list(
          kind = o$kind,
          region_id = ri$region_id,
          month = if (o$kind == "monthly") ri$month else NULL,
          revision = ri$revision,
          outcome = ri$outcome)
      }
    }
    summary$outcomes <- nontrivial
    summary$n_inserted <- nrow(m$outcomes[outcome == "insert"]) +
      nrow(r$outcomes[outcome == "insert"])

    # ---- 测试/演练用：事务内可控中断点 -----------------------------------
    #  interrupt_at = "before_receipt"：抛错 -> 自动回滚（模拟业务校验失败）
    #  pause_before_commit > 0：写标记文件后休眠，外部可 SIGKILL 进程，
    #  用于验证“进程被杀 -> 本批完全不可见，重启重传不重复生效”。
    if (!is.null(opts$interrupt_at) &&
        opts$interrupt_at %in% c("before_receipt", "before_commit")) {
      ledger_log("触发测试中断点: ", opts$interrupt_at, "（事务将回滚）")
      stop_ledger("interrupted_at", paste0("测试中断点: ", opts$interrupt_at))
    }
    if (!is.null(opts$pause_before_commit) &&
        is.finite(as.numeric(opts$pause_before_commit)) &&
        as.numeric(opts$pause_before_commit) > 0) {
      if (!is.null(opts$pause_marker) && nzchar(opts$pause_marker))
        writeLines(as.character(Sys.getpid()), opts$pause_marker)
      ledger_log("事务内暂停 ", opts$pause_before_commit,
                 " 秒（已持有写锁，等待外部终止）")
      Sys.sleep(as.numeric(opts$pause_before_commit))
    }

    DBI::dbExecute(con, "
      INSERT INTO receipt(batch_id, source_id, file_md5, status, summary_json, received_at)
      VALUES (?,?,?,'committed',?,?)",
      list(batch_id, source_id, content_md5,
           jsonlite::toJSON(summary, auto_unbox = TRUE, null = "null",
                            digits = NA),
           received_at))
    # 依赖 receipt 的外键行必须在回执之后写
    write_outcome_rows(con, m$outcomes, LEDGER_KIND_MONTHLY, batch_id)
    write_outcome_rows(con, r$outcomes, LEDGER_KIND_REGISTRATION, batch_id)
    write_arrival_rows(con, m$outcomes, LEDGER_KIND_MONTHLY, batch_id,
                       received_at)
    write_arrival_rows(con, r$outcomes, LEDGER_KIND_REGISTRATION, batch_id,
                       received_at)
    summary
  })

  # 事务内发现回执（并发重传的后者）时，结果本身就是“重放”，不能覆盖标记
  if (is.null(result$replay)) result$replay <- FALSE
  if (is.null(result$status)) result$status <- "committed"
  result$started_at <- started_at
  result$finished_at <- now_iso()
  result$elapsed_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
  if (is.null(result$applied_ids)) {
    # 回执 JSON 不塞入成千上万个 id；完整映射在 monthly_version/registration_version
    # 的 record_id 与 receipt.batch_id 中可查。
    result$applied_ids <- lapply(applied, function(ids) {
      list(n = length(ids),
           first = if (length(ids) > 0L) head(ids, 20L) else integer(0))
    })
  }
  result
}

classify_outcomes_empty <- function(kind) {
  if (kind == LEDGER_KIND_MONTHLY)
    data.table(region_id = character(), month = character(),
               revision = integer(), op = character(), y = numeric(),
               x1 = numeric(), x2 = numeric(), payload = character(),
               csv_row = integer(), outcome = character())
  else
    data.table(region_id = character(), revision = integer(),
               op = character(), first_treat_month = character(),
               payload = character(), csv_row = integer(),
               outcome = character())
}
