# =============================================================================
# preview.R —— 批次预演：与正式导入完全相同的校验、批内冲突判断、修订比较与
#               重放规则，生成可供复核的影响报告，但不提交任何数据。
#
# 实现方式：
#  * 文件解析、批次契约、内容指纹、重放检查、逐行裁决、版本行写入全部调用
#    与正式导入相同的函数（parse_batch_file / validate_batch_contract /
#    batch_content_md5 / check_replay / ingest_tx_core）；
#  * 模拟在单个 BEGIN IMMEDIATE 事务内进行：写锁保证预演期间没有其他批次
#    提交，报告的“预演前 / 预演后”两侧与状态边界来自同一个账本状态，
#    并发导入不会混入；事务结束时**无条件回滚**，版本行、当前指针、回执、
#    逐行结果、过时/重复来件、导出发布记录都不会留下，sqlite_sequence
#    也随之还原，同一 batch_id 之后仍可正式导入；
#  * “预演后”的分析面板与待补登记清单用与正式导出相同的
#    compute_export_tables() 在模拟事务内重算；
#  * 该 batch_id 已有相同文件的成功回执时，报告明确标记为重放
#    （verdict = "replay"），不模拟再次生效；已有编号对应不同内容时按正式
#    导入规则拒绝（batch_replay_conflict，退出码 2，不写报告）。
#
# 报告结论（verdict）：
#   would_commit  批次可导入；报告给出逐行结果、面板/待补差异与预测面板
#   replay        同 batch_id 同文件已有成功回执；不模拟再次生效
#   would_reject  批次会被整批拒绝；报告给出拒绝原因与冲突/错误行
# 只有预演本身无法执行（库不存在、参数非法、文件读不到、batch_id 对应不同
# 内容等）时才以 ledger_error 抛出（CLI 退出码 2），此时不写报告、不触碰
# 调用者已有的报告文件。
# =============================================================================

# 这些错误代表“批次数据本身会被正式导入整批拒绝”，预演照常生成报告
PREVIEW_REPORTABLE_CODES <- c("file_validation_error",
                              "same_revision_conflict",
                              "revision_content_conflict")

PREVIEW_REPORT_VERSION <- 1L

# ---- 状态边界 -----------------------------------------------------------------
# 报告记录本次预演所依据的账本状态，供复核者核对；这只是预演时的状态，
# 之后其他批次导入会使账本前进，不影响本报告当时的有效性。
capture_state_boundary <- function(con) {
  list(
    max_monthly_record_id = DBI::dbGetQuery(con,
      "SELECT COALESCE(MAX(record_id),0) AS v FROM monthly_version")$v,
    max_registration_record_id = DBI::dbGetQuery(con,
      "SELECT COALESCE(MAX(record_id),0) AS v FROM registration_version")$v,
    n_monthly_versions = DBI::dbGetQuery(con,
      "SELECT COUNT(*) AS v FROM monthly_version")$v,
    n_registration_versions = DBI::dbGetQuery(con,
      "SELECT COUNT(*) AS v FROM registration_version")$v,
    sqlite_data_version = DBI::dbGetQuery(con, "PRAGMA data_version")[[1]])
}

boundary_with_note <- function(b) {
  c(b, list(note = paste0(
    "以上边界只是本次预演所依据的账本状态；报告的预演前/预演后两侧均在",
    "同一个 BEGIN IMMEDIATE 事务内读取（随后已回滚），并发导入不会混入。",
    "预演之后其他批次导入会使账本前进，不影响本报告当时的结论。")))
}

# ---- 面板 / 待补清单差异 -------------------------------------------------------
# NA 感知的不相等：两边都缺失视为相等
preview_neq <- function(x, y) {
  (is.na(x) != is.na(y)) | (!is.na(x) & !is.na(y) & x != y)
}

PANEL_DIFF_VALUE_COLS <- c("y", "x1", "x2", "first_treat_month", "treat")

# 逐 (region_id, month) 比较两版导出面板；每行保留月报与登记的修订来源
diff_panel_rows <- function(before, after) {
  b <- merge(before$panel, before$provenance,
             by = c("region_id", "month"), all.x = TRUE, sort = FALSE)
  a <- merge(after$panel, after$provenance,
             by = c("region_id", "month"), all.x = TRUE, sort = FALSE)
  b[, .in_b := TRUE]
  a[, .in_a := TRUE]
  j <- merge(b, a, by = c("region_id", "month"), all = TRUE, sort = TRUE,
             suffixes = c(".b", ".a"))
  if (nrow(j) == 0L)
    return(list(counts = list(added = 0L, changed = 0L, removed = 0L,
                              unchanged = 0L),
                rows = list()))
  j[is.na(.in_b), .in_b := FALSE]
  j[is.na(.in_a), .in_a := FALSE]

  col_b <- function(nm) j[[paste0(nm, ".b")]]
  col_a <- function(nm) j[[paste0(nm, ".a")]]
  # 逐列的变化标记（list of logical(nrow(j))）；vapply 在单行时会退化成向量，
  # 因此用 lapply + Reduce 组合
  changed_list <- lapply(PANEL_DIFF_VALUE_COLS, function(nm)
    preview_neq(col_b(nm), col_a(nm)))
  is_changed <- j$.in_b & j$.in_a & Reduce(`|`, changed_list)
  status <- ifelse(!j$.in_b, "added",
            ifelse(!j$.in_a, "removed",
            ifelse(is_changed, "changed", "unchanged")))

  block <- function(i, suf) {
    g <- function(nm) j[[paste0(nm, suf)]][i]
    list(y = g("y"), x1 = g("x1"), x2 = g("x2"),
         first_treat_month = g("first_treat_month"),
         treat = as.integer(g("treat")),
         monthly_revision = as.integer(g("monthly_revision")),
         monthly_batch_id = g("monthly_batch_id"),
         registration_revision = as.integer(g("registration_revision")),
         registration_batch_id = g("registration_batch_id"))
  }
  rows <- vector("list", nrow(j))
  for (i in seq_len(nrow(j))) {
    st <- status[i]
    fields <- if (st == "changed")
      PANEL_DIFF_VALUE_COLS[vapply(changed_list, `[`, logical(1), i)] else NULL
    rows[[i]] <- list(
      region_id = j$region_id[i],
      month = j$month[i],
      status = st,
      changed_fields = fields,
      before = if (j$.in_b[i]) block(i, ".b") else NULL,
      after = if (j$.in_a[i]) block(i, ".a") else NULL)
  }
  cnt <- table(factor(status, levels = c("added", "changed", "removed",
                                         "unchanged")))
  list(counts = as.list(stats::setNames(as.integer(cnt), names(cnt))),
       rows = rows)
}

# 逐 (region_id, month) 比较两版待补登记清单
diff_missing_rows <- function(before, after) {
  bm <- before$missing
  am <- after$missing
  b <- bm[, .(region_id, month, reason_b = reason,
              monthly_revision_b = monthly_revision,
              monthly_batch_id_b = monthly_batch_id, .in_b = TRUE)]
  a <- am[, .(region_id, month, reason_a = reason,
              monthly_revision_a = monthly_revision,
              monthly_batch_id_a = monthly_batch_id, .in_a = TRUE)]
  j <- merge(b, a, by = c("region_id", "month"), all = TRUE, sort = TRUE)
  if (nrow(j) == 0L)
    return(list(counts = list(entered = 0L, left = 0L, changed = 0L,
                              unchanged = 0L),
                rows = list()))
  j[is.na(.in_b), .in_b := FALSE]
  j[is.na(.in_a), .in_a := FALSE]
  reason_changed <- j$.in_b & j$.in_a &
    preview_neq(j$reason_b, j$reason_a)
  status <- ifelse(!j$.in_b, "entered",
            ifelse(!j$.in_a, "left",
            ifelse(reason_changed, "changed", "unchanged")))
  rows <- vector("list", nrow(j))
  for (i in seq_len(nrow(j))) {
    from_after <- j$.in_a[i]
    rows[[i]] <- list(
      region_id = j$region_id[i],
      month = j$month[i],
      status = status[i],
      before_reason = if (j$.in_b[i]) j$reason_b[i] else NULL,
      after_reason = if (j$.in_a[i]) j$reason_a[i] else NULL,
      monthly_revision = as.integer(
        if (from_after) j$monthly_revision_a[i] else j$monthly_revision_b[i]),
      monthly_batch_id =
        if (from_after) j$monthly_batch_id_a[i] else j$monthly_batch_id_b[i])
  }
  cnt <- table(factor(status, levels = c("entered", "left", "changed",
                                         "unchanged")))
  list(counts = as.list(stats::setNames(as.integer(cnt), names(cnt))),
       rows = rows)
}

diff_export_tables <- function(before, after) {
  list(panel = diff_panel_rows(before, after),
       missing = diff_missing_rows(before, after))
}

# ---- 逐行结果 ------------------------------------------------------------------
# 与 ingest_outcome 同形的逐行判定表（monthly 行在前、registration 行在后，
# 各自按 csv_row 排序）；outcome ∈
# insert/replace/restore/delete/stale/duplicate
preview_outcome_table <- function(m_outcomes, r_outcomes) {
  mk <- function(dt, kind) {
    if (nrow(dt) == 0L)
      return(data.frame(kind = character(), csv_row = integer(),
                        region_id = character(), month = character(),
                        revision = integer(), op = character(),
                        outcome = character(), stringsAsFactors = FALSE))
    data.frame(kind = kind,
               csv_row = as.integer(dt$csv_row),
               region_id = as.character(dt$region_id),
               month = if (kind == LEDGER_KIND_MONTHLY)
                 as.character(dt$month) else NA_character_,
               revision = as.integer(dt$revision),
               op = as.character(dt$op),
               outcome = as.character(dt$outcome),
               stringsAsFactors = FALSE)
  }
  rbind(mk(m_outcomes, LEDGER_KIND_MONTHLY),
        mk(r_outcomes, LEDGER_KIND_REGISTRATION))
}

preview_outcome_counts <- function(outcomes) {
  lv <- c("insert", "replace", "restore", "delete", "stale", "duplicate")
  cnt <- table(factor(outcomes$outcome, levels = lv))
  as.list(stats::setNames(as.integer(cnt), names(cnt)))
}

# ---- 报告骨架 ------------------------------------------------------------------
preview_files_section <- function(files, monthly, registration, content_md5) {
  one <- function(path, info) {
    if (is.null(path)) return(NULL)
    list(path = normalizePath(path, mustWork = FALSE),
         md5 = file_hash(path),
         n_rows = if (is.null(info)) NULL else info$n_rows_raw,
         n_duplicate_in_file = if (is.null(info)) NULL
           else info$n_duplicate_in_file)
  }
  list(monthly = one(files$monthly, monthly),
       registration = one(files$registration, registration),
       content_md5 = content_md5)
}

new_preview_report <- function(con, batch_id, verdict, files_section,
                               boundary) {
  list(
    report_type = "batch_preview",
    report_version = PREVIEW_REPORT_VERSION,
    generated_at = now_iso(),
    db_path = ledger_path_of(con),
    source_id = ledger_source_id(con),
    batch_id = batch_id,
    verdict = verdict,
    files = files_section,
    state_boundary = boundary_with_note(boundary),
    rejection = NULL,
    replay = NULL,
    row_outcomes = NULL,
    outcome_counts = NULL,
    panel_diff = NULL,
    missing_diff = NULL,
    predicted_panel = NULL,
    predicted_missing_registration = NULL,
    side_effects = "none",
    side_effects_note = paste0(
      "预演在回滚事务内模拟：未留下版本行、当前指针、回执、逐行结果、",
      "过时/重复来件记录或导出发布记录；同一 batch_id 之后仍可正式导入，",
      "修正文件时仍按原有 batch_id 冲突规则处理。"))
}

# ---- 主入口 --------------------------------------------------------------------
# files: list(monthly = path|NULL, registration = path|NULL)
# opts:  list(batch_id, source_id = NULL,
#             pause_inside_tx = NULL, pause_marker = NULL)   # 后两项仅测试用
# 返回报告对象（list）；写盘与发布见 write_preview_report()。
preview_batch <- function(con, files, opts = list()) {
  t0 <- Sys.time()
  source_id <- ledger_source_id(con)
  if (!is.null(opts$source_id) && !identical(opts$source_id, source_id))
    stop_ledger("source_id_conflict",
                paste0("--source-id 与库绑定值不一致: 库=", source_id,
                       " 请求=", opts$source_id))

  # ---- 1) 文件解析：与正式导入完全相同的校验。单元格级错误属于“批次会被
  #         整批拒绝”，照常出报告；结构性问题（读不到/表头错/空文件）与
  #         正式导入一样直接拒绝，不写报告。 -------------------------------
  parse_error <- NULL
  monthly <- NULL
  registration <- NULL
  if (!is.null(files$monthly)) {
    monthly <- tryCatch(
      parse_batch_file(files$monthly, LEDGER_KIND_MONTHLY),
      ledger_error = function(e) { parse_error <<- e; NULL })
  }
  if (is.null(parse_error) && !is.null(files$registration)) {
    registration <- tryCatch(
      parse_batch_file(files$registration, LEDGER_KIND_REGISTRATION),
      ledger_error = function(e) { parse_error <<- e; NULL })
  }
  if (!is.null(parse_error) &&
      !(parse_error$code %in% PREVIEW_REPORTABLE_CODES))
    stop(parse_error)

  # 内容指纹：与正式导入同一公式；解析失败的文件也能给出文件级 md5
  m_md5 <- if (!is.null(files$monthly) && file.exists(files$monthly))
    file_hash(files$monthly) else NULL
  r_md5 <- if (!is.null(files$registration) && file.exists(files$registration))
    file_hash(files$registration) else NULL
  content_md5 <- batch_content_md5(
    if (is.null(m_md5)) NULL else list(md5 = m_md5),
    if (is.null(r_md5)) NULL else list(md5 = r_md5))
  files_section <- preview_files_section(files, monthly, registration,
                                         content_md5)

  finish <- function(report) {
    report$elapsed_ms <- as.numeric(difftime(Sys.time(), t0,
                                             units = "secs")) * 1000
    report
  }

  # ---- 1b) 文件校验失败：整批拒绝报告（边界取自只读快照） ------------------
  if (!is.null(parse_error)) {
    boundary <- with_read_snapshot(con, function() capture_state_boundary(con))
    report <- new_preview_report(con, opts$batch_id, "would_reject",
                                 files_section, boundary)
    report$rejection <- list(
      error_code = parse_error$code,
      message = conditionMessage(parse_error),
      offending_rows = parse_error$details)
    return(finish(report))
  }

  # ---- 2) 批次契约：与正式导入相同（非法 batch_id / 空批次直接拒绝） -------
  contract <- validate_batch_contract(opts$batch_id, source_id,
                                      monthly, registration)
  batch_id <- contract$batch_id

  # ---- 3) 重放检查：与正式导入相同。已有编号对应不同内容 ->
  #         batch_replay_conflict 直接拒绝（退出码 2，不写报告）-------------
  pre <- with_read_snapshot(con, function() {
    list(replay = check_replay(con, source_id, batch_id, content_md5),
         boundary = capture_state_boundary(con))
  })
  if (!is.null(pre$replay)) {
    report <- new_preview_report(con, batch_id, "replay",
                                 files_section, pre$boundary)
    report$replay <- list(
      received_at = pre$replay$received_at,
      original_summary = pre$replay$summary,
      note = paste0("该 batch_id 已有相同文件的成功回执；按正式导入规则这",
                    "是重放，不模拟再次生效，分析面板与待补登记清单不变。"))
    return(finish(report))
  }

  # ---- 4) 模拟事务：写锁内取边界与前视图 -> 模拟导入 -> 重算后视图 -> 回滚 --
  received_at <- now_iso()
  sim <- with_immediate_tx_rollback(con, function() {
    boundary <- capture_state_boundary(con)
    before <- compute_export_tables(con)
    # 测试钩子：事务内暂停（此时已持有写锁），用于并发演练
    if (!is.null(opts$pause_inside_tx) &&
        is.finite(as.numeric(opts$pause_inside_tx)) &&
        as.numeric(opts$pause_inside_tx) > 0) {
      if (!is.null(opts$pause_marker) && nzchar(opts$pause_marker))
        writeLines(as.character(Sys.getpid()), opts$pause_marker)
      ledger_log("预演事务内暂停 ", opts$pause_inside_tx,
                 " 秒（已持有写锁）")
      Sys.sleep(as.numeric(opts$pause_inside_tx))
    }
    core <- tryCatch(
      ingest_tx_core(con, monthly, registration, batch_id, content_md5,
                     source_id, received_at, opts = list(), finalize = FALSE),
      ledger_error = function(e) e)
    if (is_ledger_error(core)) {
      if (core$code %in% PREVIEW_REPORTABLE_CODES)
        return(list(kind = "reject", error = core, boundary = boundary))
      stop(core)   # 其余错误（如并发后的 batch_replay_conflict）照常抛出
    }
    if (isTRUE(core$replay))
      return(list(kind = "replay", replay = core$summary,
                  boundary = boundary))
    after <- compute_export_tables(con)
    list(kind = "commit", boundary = boundary, before = before,
         after = after, m = core$m, r = core$r)
  })

  # ---- 5) 组装报告 -----------------------------------------------------------
  if (identical(sim$kind, "replay")) {
    # 并发下另一进程刚提交了同内容批次：按重放报告（边界含该批次）
    report <- new_preview_report(con, batch_id, "replay",
                                 files_section, sim$boundary)
    report$replay <- list(
      received_at = sim$replay$received_at,
      original_summary = sim$replay,
      note = paste0("该 batch_id 已有相同文件的成功回执；按正式导入规则这",
                    "是重放，不模拟再次生效，分析面板与待补登记清单不变。"))
    return(finish(report))
  }
  if (identical(sim$kind, "reject")) {
    report <- new_preview_report(con, batch_id, "would_reject",
                                 files_section, sim$boundary)
    report$rejection <- list(
      error_code = sim$error$code,
      message = conditionMessage(sim$error),
      offending_rows = sim$error$details)
    return(finish(report))
  }

  # would_commit：逐行结果 + 面板/待补差异 + 预测面板
  diff <- diff_export_tables(sim$before, sim$after)
  outcomes <- preview_outcome_table(sim$m$outcomes, sim$r$outcomes)
  report <- new_preview_report(con, batch_id, "would_commit",
                               files_section, sim$boundary)
  report$row_outcomes <- outcomes
  report$outcome_counts <- preview_outcome_counts(outcomes)
  report$panel_diff <- diff$panel
  report$missing_diff <- diff$missing
  report$predicted_panel <- as.data.frame(sim$after$panel)
  report$predicted_missing_registration <- as.data.frame(sim$after$missing)
  finish(report)
}

# ---- 报告发布 ------------------------------------------------------------------
# 全部内容生成成功后，先写同目录临时文件再原子重命名发布；
# 任何失败都会删掉临时文件，调用者已有的报告保持不变。
write_preview_report <- function(report, path) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    stop_ledger("missing_report_path", "必须提供 --report 报告输出路径")
  dir <- dirname(path)
  if (!dir.exists(dir))
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dir))
    stop_ledger("report_dir_failed",
                paste0("无法创建报告目录: ", dir))
  tmp <- file.path(dir, sprintf(".%s.tmp-%d", basename(path), Sys.getpid()))
  if (file.exists(tmp)) unlink(tmp, force = TRUE)
  ok <- FALSE
  on.exit(if (!ok && file.exists(tmp)) unlink(tmp, force = TRUE), add = TRUE)
  tryCatch(
    jsonlite::write_json(report, tmp, auto_unbox = TRUE, null = "null",
                         digits = NA, pretty = TRUE),
    error = function(e)
      stop_ledger("report_write_failed",
                  paste0("写预演报告临时文件失败: ", conditionMessage(e))))
  if (!file.rename(tmp, path))
    stop_ledger("report_publish_failed",
                paste0("发布预演报告失败（重命名）: ", tmp, " -> ", path))
  ok <- TRUE
  invisible(normalizePath(path, mustWork = FALSE))
}

# ---- CLI 摘要行 -----------------------------------------------------------------
preview_report_summary <- function(report, report_path) {
  out <- list(
    report = report_path,
    batch_id = report$batch_id,
    verdict = report$verdict,
    replay = identical(report$verdict, "replay"),
    state_boundary = report$state_boundary,
    elapsed_ms = report$elapsed_ms)
  if (!is.null(report$rejection))
    out$error_code <- report$rejection$error_code
  if (!is.null(report$outcome_counts))
    out$outcome_counts <- report$outcome_counts
  if (!is.null(report$panel_diff))
    out$panel_diff_counts <- report$panel_diff$counts
  if (!is.null(report$missing_diff))
    out$missing_diff_counts <- report$missing_diff$counts
  if (!is.null(report$predicted_panel))
    out$n_predicted_panel <- nrow(report$predicted_panel)
  if (!is.null(report$predicted_missing_registration))
    out$n_predicted_missing <- nrow(report$predicted_missing_registration)
  out
}
