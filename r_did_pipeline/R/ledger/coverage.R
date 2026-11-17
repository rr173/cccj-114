# =============================================================================
# coverage.R —— 覆盖核验：把运营人员提供的覆盖约定与账本当前状态对照，
#               输出逐地区逐月的缺口与例外，不改动账本或正式导出。
#
# 背景：账本只能列出“已经收到”的月报与“因缺少有效上线登记而不能拼入面板”的
# 记录；对于完全没有收到的地区-月份，账本不知道该月份本来是否应该上报。
# 覆盖核验由运营人员提供本次核验适用的覆盖约定（哪些地区在哪些月份应报、
# 哪些月份豁免），与账本当前数据对照，找出可定位的缺口与例外。
#
# 关键规则：
#  * 覆盖约定文件按地区给出闭区间 [start_month, end_month]（YYYY-MM）与状态
#    expected（应报）/ exempt（豁免，必须带原因）；一个地区的区间不能重叠；
#    文件格式错误、月份无效、区间倒置或相互矛盾时，核验整体失败并指出问题行；
#  * 命令接收本次核验的起止月份，只处理与该范围相交的约定（区间按范围裁切）；
#    未被任何约定覆盖的月份不默认为豁免，作为“约定缺失”（uncovered）单独报告；
#  * 每个应报的地区-月份按账本最高修订状态区分：
#      missing_report        从未收到月报（账本中没有任何版本行）
#      report_withdrawn      月报已撤回（最高修订为 delete 墓碑）
#      missing_registration  月报存在但缺少有效上线登记
#      ready_for_panel       已具备进入面板的条件（有效月报 + 有效登记）
#    豁免月份保留豁免原因；该月仍收到（现存有效）月报时列为
#    exempt_with_delivery（豁免期有来件），不静默计作按期交付；
#  * 落在本次月份范围内、却不属于任何应报约定的现存月报逐条列入
#    unscheduled_reports，避免排期文件漏掉地区后多余数据消失在汇总里；
#  * 全部读取在单个只读事务（固定快照）内完成：并发导入不会让部分地区按
#    导入前、部分按导入后的数据判定；报告带可复核的账本边界；
#  * 核验不新建账本记录、回执或导出发布记录；报告全部生成后才原子发布到
#    调用者指定位置，任何失败都不覆盖已有报告。
# =============================================================================

COVERAGE_FILE_COLS <- c("region_id", "start_month", "end_month",
                        "status", "reason")
COVERAGE_STATUS_EXPECTED <- "expected"   # 应报
COVERAGE_STATUS_EXEMPT   <- "exempt"     # 豁免（必须带原因）

COVERAGE_REPORT_VERSION <- 1L

# 逐地区逐月明细的固定列序（JSON 数组逐行输出）
COVERAGE_ITEM_COLS <- c(
  "region_id", "month", "coverage", "status", "actionable",
  "report_state",
  "monthly_record_id", "monthly_revision", "monthly_batch_id",
  "monthly_received_at",
  "registration_state",
  "registration_record_id", "registration_revision",
  "registration_batch_id", "registration_received_at",
  "first_treat_month", "exemption_reason", "source_row")

COVERAGE_UNSCHEDULED_COLS <- c(
  "region_id", "month", "monthly_record_id", "monthly_revision",
  "monthly_batch_id", "monthly_received_at", "covered_by")

# 需要运营处理的明细状态（决定 verdict 与退出码）
COVERAGE_ACTIONABLE_STATUS <- c("missing_report", "report_withdrawn",
                                "missing_registration", "uncovered",
                                "exempt_with_delivery")

# ---- 月份工具 ------------------------------------------------------------------
# from/to 必须是合法 YYYY-MM（调用前已校验）；返回闭区间全部月份。
month_seq <- function(from, to) {
  fy <- as.integer(substr(from, 1L, 4L)); fm <- as.integer(substr(from, 6L, 7L))
  ty <- as.integer(substr(to, 1L, 4L));   tm <- as.integer(substr(to, 6L, 7L))
  n <- (ty - fy) * 12L + (tm - fm) + 1L
  if (n <= 0L) return(character(0))
  total <- (fy * 12L + (fm - 1L)) + (seq_len(n) - 1L)
  sprintf("%04d-%02d", total %/% 12L, total %% 12L + 1L)
}

# 向量化严格 YYYY-MM 校验（与 validate.R 的 is_valid_month 同一套规则）
valid_month_vec <- function(x) {
  ok <- grepl(MONTH_RE, x)
  d <- suppressWarnings(as.Date(paste0(x, "-01"), format = "%Y-%m-%d"))
  ok & !is.na(d) & (format(d, "%Y-%m") == x)
}

# ---- 覆盖约定文件：解析与校验（纯函数，不接触数据库） ---------------------------
# 文件格式（CSV，UTF-8，首行表头，列顺序不限，不允许多余/缺少列）：
#   region_id,start_month,end_month,status,reason
#   * status 为 expected（应报）或 exempt（豁免）；
#   * exempt 必须给出非空 reason；expected 不允许填写 reason；
#   * 闭区间 [start_month, end_month]，不允许倒置；
#   * 同一地区的区间（即使状态相同）不能重叠；重叠即矛盾，整体失败。
# 所有问题按数据行号（表头为第 1 行）聚合后一次性报告。
parse_coverage_file <- function(path) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    stop_ledger("missing_coverage_path",
                "必须提供覆盖约定文件路径（--coverage）")
  if (!file.exists(path))
    stop_ledger("coverage_file_not_found",
                paste0("覆盖约定文件不存在: ", path))
  dt <- tryCatch(
    data.table::fread(path, colClasses = "character", na.strings = NULL,
                      strip.white = FALSE, blank.lines.skip = TRUE,
                      check.names = FALSE, encoding = "UTF-8",
                      showProgress = FALSE),
    error = function(e) stop_ledger("coverage_format_error",
      paste0("无法解析覆盖约定 CSV: ", path, " | ", conditionMessage(e))))
  if (!is.data.frame(dt) || nrow(dt) == 0L)
    stop_ledger("coverage_format_error",
                paste0("覆盖约定文件没有数据行: ", path,
                       "（至少要有一条约定）"))
  nm <- sub("^﻿", "", names(dt))
  setnames(dt, nm)
  if (anyDuplicated(names(dt)))
    stop_ledger("coverage_format_error",
                paste0("表头存在重复列名: ", path))
  miss <- setdiff(COVERAGE_FILE_COLS, names(dt))
  extra <- setdiff(names(dt), COVERAGE_FILE_COLS)
  if (length(miss) > 0L || length(extra) > 0L) {
    stop_ledger("coverage_format_error", paste0(
      path, " 表头必须恰好为 [", paste(COVERAGE_FILE_COLS, collapse = ","), "]",
      if (length(miss) > 0L)
        paste0("；缺少 [", paste(miss, collapse = ","), "]") else "",
      if (length(extra) > 0L)
        paste0("；多余 [", paste(extra, collapse = ","), "]") else ""))
  }
  setcolorder(dt, COVERAGE_FILE_COLS)
  dt <- as.data.table(lapply(dt, function(z) trimws(as.character(z))))
  n <- nrow(dt)

  errs <- list()
  add_errs <- function(sel, field, why, values = NULL) {
    idx <- which(sel)
    if (is.null(values)) values <- dt[[field]]
    for (i in idx)
      errs[[length(errs) + 1L]] <<- ledger_error_cell(
        field, i + 1L, values[i], why)
  }

  rid <- dt$region_id
  bad_rid_empty <- !nzchar(rid)
  bad_rid_ctrl <- !bad_rid_empty &
    grepl("[\x01-\x1f\x7f]", rid, useBytes = TRUE)
  bad_rid_len <- !bad_rid_empty &
    nchar(rid, type = "bytes") > 512L
  add_errs(bad_rid_empty, "region_id", "region_id 不能为空")
  add_errs(bad_rid_ctrl, "region_id", "region_id 不能包含控制字符")
  add_errs(bad_rid_len, "region_id", "region_id 过长（>512 字节）")

  sm_ok <- valid_month_vec(dt$start_month)
  em_ok <- valid_month_vec(dt$end_month)
  add_errs(!sm_ok, "start_month", "月份必须是严格 YYYY-MM 且真实存在")
  add_errs(!em_ok, "end_month", "月份必须是严格 YYYY-MM 且真实存在")

  st <- dt$status
  bad_status <- !st %in% c(COVERAGE_STATUS_EXPECTED, COVERAGE_STATUS_EXEMPT)
  add_errs(bad_status, "status",
           paste0("status 必须是 ", COVERAGE_STATUS_EXPECTED, "（应报）或 ",
                  COVERAGE_STATUS_EXEMPT, "（豁免）"))

  reason <- dt$reason
  add_errs(!bad_status & st == COVERAGE_STATUS_EXEMPT & !nzchar(reason),
           "reason", "豁免区间必须给出原因", values = reason)
  add_errs(!bad_status & st == COVERAGE_STATUS_EXPECTED & nzchar(reason),
           "reason", "应报区间不填写原因（只有豁免区间需要原因）",
           values = reason)

  inverted <- sm_ok & em_ok & dt$start_month > dt$end_month
  add_errs(inverted, "interval",
           "区间倒置：start_month 晚于 end_month",
           values = paste0(dt$start_month, "..", dt$end_month))

  # ---- 同地区区间重叠（含相互矛盾）：在月份合法且未倒置的行之间检查 ---------
  # 先收集重叠对，再为每个涉事行各生成一条错误（双方都指出，便于定位）。
  cand <- which(sm_ok & em_ok & !inverted)
  overlap_pairs <- list()
  if (length(cand) > 1L) {
    for (reg in unique(rid[cand])) {
      idx <- cand[rid[cand] == reg]
      ord <- idx[order(dt$start_month[idx], dt$end_month[idx], idx)]
      best <- ord[[1L]]
      for (k in ord[-1L]) {
        if (dt$start_month[[k]] <= dt$end_month[[best]]) {
          overlap_pairs[[length(overlap_pairs) + 1L]] <- c(best, k)
          if (dt$end_month[[k]] > dt$end_month[[best]]) best <- k
        } else {
          best <- k
        }
      }
    }
  }
  if (length(overlap_pairs) > 0L) {
    involved <- sort(unique(unlist(overlap_pairs)))
    for (i in involved) {
      partners <- unique(unlist(lapply(overlap_pairs, function(p)
        if (i %in% p) setdiff(p, i) else NULL)))
      txt <- paste(vapply(partners, function(j)
        sprintf("数据行 %d（%s..%s，%s）", j + 1L, dt$start_month[[j]],
                dt$end_month[[j]], st[[j]]), character(1)), collapse = "、")
      errs[[length(errs) + 1L]] <- ledger_error_cell(
        "interval", i + 1L,
        paste0(dt$start_month[[i]], "..", dt$end_month[[i]]),
        paste0("与 ", txt, " 重叠；一个地区的区间不能重叠"))
    }
  }

  if (length(errs) > 0L) {
    ord <- order(vapply(errs, function(e) e$row, 0L))
    errs <- errs[ord]
    stop_ledger("coverage_validation_error",
                format_cell_errors(errs, path), details = errs)
  }

  rows <- data.table(region_id = rid, start_month = dt$start_month,
                     end_month = dt$end_month, status = st, reason = reason,
                     csv_row = seq_len(n) + 1L)
  list(rows = rows, n_rows = n, md5 = file_hash(path))
}

# ---- 范围校验与裁切 --------------------------------------------------------------
validate_verify_range <- function(from, to) {
  if (is.null(from) || length(from) != 1L || isTRUE(from) || !nzchar(from) ||
      is.null(to) || length(to) != 1L || isTRUE(to) || !nzchar(to))
    stop_ledger("missing_range",
                "必须提供本次核验的起止月份（--from 与 --to，YYYY-MM）")
  if (!is_valid_month(from) || !is_valid_month(to))
    stop_ledger("invalid_range",
                paste0("--from/--to 必须是严格 YYYY-MM 且真实存在: ",
                       from, " .. ", to))
  if (from > to)
    stop_ledger("invalid_range",
                paste0("核验范围倒置: --from ", from, " 晚于 --to ", to))
  invisible(TRUE)
}

# 只保留与 [from, to] 相交的约定，并把区间裁切到范围内（跨月边界裁切）
clip_coverage <- function(rows, from, to) {
  keep <- rows$start_month <= to & rows$end_month >= from
  r <- rows[keep]
  if (nrow(r) == 0L)
    return(data.table(region_id = character(), start_month = character(),
                      end_month = character(), status = character(),
                      reason = character(), csv_row = integer(),
                      clip_start = character(), clip_end = character()))
  r[, `:=`(clip_start = pmax(start_month, from),
           clip_end = pmin(end_month, to))]
  r
}

# 把裁切后的区间展开成逐月覆盖表（同一地区-月份至多一行：区间不重叠已校验）
expand_coverage <- function(clipped) {
  if (nrow(clipped) == 0L)
    return(data.table(region_id = character(), month = character(),
                      coverage = character(), exemption_reason = character(),
                      source_row = integer()))
  clipped[, .(month = month_seq(clip_start, clip_end)),
          by = .(region_id, coverage = status, exemption_reason = reason,
                 source_row = csv_row)]
}

# ---- 账本读取（只读快照内） -------------------------------------------------------
# 与预演/导出同一组边界字段，供复核者核对本报告对应哪个账本状态。
capture_coverage_boundary <- function(con) {
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

coverage_boundary_with_note <- function(b) {
  c(b, list(note = paste0(
    "以上边界是本次核验读取的账本状态；核验在单个只读事务（固定快照）内完成，",
    "并发导入不会混入，结论只适用于该状态。核验之后其他批次导入会使账本前进，",
    "不影响本报告当时的判定。")))
}

# 当前有效月报（范围内全部地区）与当前有效登记（全表，供范围外月份判定归属）
read_coverage_ledger <- function(con, from, to) {
  monthly <- as.data.table(DBI::dbGetQuery(con, "
    SELECT v.region_id, v.month, v.revision, v.op,
           v.batch_id, v.record_id, v.received_at
      FROM monthly_current c JOIN monthly_version v ON v.record_id = c.record_id
     WHERE v.month >= ? AND v.month <= ?", list(from, to)))
  registration <- as.data.table(DBI::dbGetQuery(con, "
    SELECT v.region_id, v.revision, v.op, v.first_treat_month,
           v.batch_id, v.record_id, v.received_at
      FROM registration_current c JOIN registration_version v
        ON v.record_id = c.record_id"))
  list(monthly = monthly, registration = registration)
}

# ---- 逐地区逐月判定 ---------------------------------------------------------------
# covered: expand_coverage() 的输出；months: month_seq(from, to)；
# monthly/registration: read_coverage_ledger() 的输出。
# 返回 data.table，列为 COVERAGE_ITEM_COLS。
build_coverage_items <- function(covered, months, monthly, registration) {
  regions <- sort(unique(covered$region_id))
  grid <- data.table::CJ(region_id = regions, month = months)
  grid <- covered[grid, on = .(region_id, month)]
  grid[is.na(coverage), coverage := "none"]

  m <- monthly[, .(region_id, month, monthly_op = op,
                   monthly_revision = revision, monthly_batch_id = batch_id,
                   monthly_record_id = record_id,
                   monthly_received_at = received_at)]
  grid <- m[grid, on = .(region_id, month)]
  r <- registration[, .(region_id, registration_op = op,
                        registration_revision = revision,
                        registration_batch_id = batch_id,
                        registration_record_id = record_id,
                        registration_received_at = received_at,
                        first_treat_month)]
  grid <- r[grid, on = .(region_id)]

  # 月报本身的最高修订状态：已有有效月报 / 从未收到 / 已撤回
  grid[, report_state := data.table::fcase(
    is.na(monthly_record_id), "never_received",
    monthly_op == "delete", "withdrawn",
    default = "valid")]
  # 登记状态（与导出同一“有效登记”定义：当前 upsert 且 first_treat_month 合法）
  grid[, registration_state := data.table::fcase(
    is.na(registration_record_id), "none",
    registration_op == "delete", "withdrawn",
    registration_op == "upsert" & !is.na(first_treat_month) &
      nzchar(first_treat_month), "effective",
    default = "incomplete")]
  grid[, status := data.table::fcase(
    coverage == "none", "uncovered",
    coverage == "exempt" & report_state == "valid", "exempt_with_delivery",
    coverage == "exempt", "exempt",
    report_state == "never_received", "missing_report",
    report_state == "withdrawn", "report_withdrawn",
    registration_state != "effective", "missing_registration",
    default = "ready_for_panel")]
  grid[, actionable := status %in% COVERAGE_ACTIONABLE_STATUS]
  grid[coverage != "exempt", exemption_reason := NA_character_]

  # 类型归一（0 行表 join 后可能出现 logical 列）
  for (cc in c("monthly_record_id", "monthly_revision",
               "registration_record_id", "registration_revision",
               "source_row"))
    set(grid, j = cc, value = as.integer(grid[[cc]]))
  for (cc in c("monthly_batch_id", "monthly_received_at",
               "registration_batch_id", "registration_received_at",
               "first_treat_month", "exemption_reason"))
    set(grid, j = cc, value = as.character(grid[[cc]]))
  grid[, c("monthly_op", "registration_op") := NULL]
  data.table::setcolorder(grid, COVERAGE_ITEM_COLS)
  data.table::setorder(grid, region_id, month)
  grid
}

# 范围内现存有效月报中，不属于任何应报约定的部分（逐条列出，不进入汇总黑洞）
build_unscheduled_reports <- function(covered, monthly) {
  ups <- monthly[op == "upsert"]
  if (nrow(ups) == 0L)
    return(data.table(region_id = character(), month = character(),
                      monthly_record_id = integer(),
                      monthly_revision = integer(),
                      monthly_batch_id = character(),
                      monthly_received_at = character(),
                      covered_by = character()))
  key <- paste(ups$region_id, ups$month, sep = "\r")
  exp_key <- paste(covered[coverage == "expected"]$region_id,
                   covered[coverage == "expected"]$month, sep = "\r")
  exe_key <- paste(covered[coverage == "exempt"]$region_id,
                   covered[coverage == "exempt"]$month, sep = "\r")
  keep <- !key %in% exp_key
  out <- ups[keep, .(region_id, month,
                     monthly_record_id = record_id,
                     monthly_revision = revision,
                     monthly_batch_id = batch_id,
                     monthly_received_at = received_at)]
  out[, covered_by := ifelse(paste(region_id, month, sep = "\r") %in% exe_key,
                             "exempt", "none")]
  for (cc in c("monthly_record_id", "monthly_revision"))
    set(out, j = cc, value = as.integer(out[[cc]]))
  for (cc in c("monthly_batch_id", "monthly_received_at", "covered_by"))
    set(out, j = cc, value = as.character(out[[cc]]))
  data.table::setcolorder(out, COVERAGE_UNSCHEDULED_COLS)
  data.table::setorder(out, region_id, month)
  out
}

coverage_summary <- function(items, unscheduled) {
  n <- function(pred) sum(pred)
  n_unsched_none <- if (nrow(unscheduled) > 0L)
    sum(unscheduled$covered_by == "none") else 0L
  counts <- list(
    n_items = nrow(items),
    n_expected = n(items$coverage == "expected"),
    n_exempt = n(items$coverage == "exempt"),
    n_uncovered = n(items$status == "uncovered"),
    n_ready_for_panel = n(items$status == "ready_for_panel"),
    n_missing_report = n(items$status == "missing_report"),
    n_report_withdrawn = n(items$status == "report_withdrawn"),
    n_missing_registration = n(items$status == "missing_registration"),
    n_exempt_with_delivery = n(items$status == "exempt_with_delivery"),
    n_unscheduled_reports = nrow(unscheduled),
    n_unscheduled_no_convention = n_unsched_none)
  counts$n_actionable <- counts$n_missing_report +
    counts$n_report_withdrawn + counts$n_missing_registration +
    counts$n_uncovered + counts$n_exempt_with_delivery +
    counts$n_unscheduled_no_convention
  counts
}

# ---- 主入口 ----------------------------------------------------------------------
# opts: list(pause_inside_snapshot = NULL, pause_marker = NULL)  # 仅并发测试用
# 返回报告对象（list）；写盘与发布见 write_coverage_report()。
verify_coverage <- function(con, coverage_path, from, to, opts = list()) {
  t0 <- Sys.time()
  source_id <- ledger_source_id(con)
  cov <- parse_coverage_file(coverage_path)     # 文件问题：整体失败，指出问题行
  validate_verify_range(from, to)
  months <- month_seq(from, to)
  clipped <- clip_coverage(cov$rows, from, to)
  covered <- expand_coverage(clipped)

  # 全部账本读取在同一个只读事务（固定快照）内：并发导入不会混入本次判定
  snap <- with_read_snapshot(con, function() {
    boundary <- capture_coverage_boundary(con)
    # 测试钩子：快照内暂停（此时读快照已固定），用于并发演练
    if (!is.null(opts$pause_inside_snapshot) &&
        is.finite(as.numeric(opts$pause_inside_snapshot)) &&
        as.numeric(opts$pause_inside_snapshot) > 0) {
      if (!is.null(opts$pause_marker) && nzchar(opts$pause_marker))
        writeLines(as.character(Sys.getpid()), opts$pause_marker)
      ledger_log("核验快照内暂停 ", opts$pause_inside_snapshot,
                 " 秒（只读事务已固定快照）")
      Sys.sleep(as.numeric(opts$pause_inside_snapshot))
    }
    data <- read_coverage_ledger(con, from, to)
    list(boundary = boundary, data = data)
  })

  items <- build_coverage_items(covered, months, snap$data$monthly,
                                snap$data$registration)
  unscheduled <- build_unscheduled_reports(covered, snap$data$monthly)
  summary <- coverage_summary(items, unscheduled)
  verdict <- if (summary$n_actionable > 0L) "findings" else "clean"

  notes <- character(0)
  if (nrow(clipped) == 0L)
    notes <- c(notes, paste0(
      "约定文件中没有与本次范围相交的约定；items 为空。",
      "请确认核验范围或约定文件是否正确。"))

  list(
    report_type = "coverage_verification",
    report_version = COVERAGE_REPORT_VERSION,
    generated_at = now_iso(),
    db_path = ledger_path_of(con),
    source_id = source_id,
    range = list(from = from, to = to),
    coverage_file = list(path = normalizePath(coverage_path, mustWork = FALSE),
                         md5 = cov$md5, n_rows = cov$n_rows),
    verdict = verdict,
    state_boundary = coverage_boundary_with_note(snap$boundary),
    items = as.data.frame(items),
    unscheduled_reports = as.data.frame(unscheduled),
    summary = summary,
    notes = notes,
    side_effects = "none",
    side_effects_note = paste0(
      "核验只读取账本：未新建版本行、当前指针、回执、逐行结果、",
      "过时/重复来件记录或导出发布记录；报告只写调用者指定的文件。"),
    elapsed_ms = as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000)
}

# ---- 报告发布 ----------------------------------------------------------------------
# 全部内容生成成功后，先写同目录临时文件再原子重命名发布；
# 任何失败都会删掉临时文件，调用者已有的报告保持不变。
write_coverage_report <- function(report, path) {
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
                         na = "null", digits = NA, pretty = TRUE),
    error = function(e)
      stop_ledger("report_write_failed",
                  paste0("写覆盖核验报告临时文件失败: ", conditionMessage(e))))
  if (!file.rename(tmp, path))
    stop_ledger("report_publish_failed",
                paste0("发布覆盖核验报告失败（重命名）: ", tmp, " -> ", path))
  ok <- TRUE
  invisible(normalizePath(path, mustWork = FALSE))
}

# ---- CLI 摘要行 ---------------------------------------------------------------------
coverage_report_summary <- function(report, report_path) {
  list(report = report_path,
       verdict = report$verdict,
       range = report$range,
       state_boundary = report$state_boundary,
       summary = report$summary,
       notes = report$notes,
       elapsed_ms = report$elapsed_ms)
}
