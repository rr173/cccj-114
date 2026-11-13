# =============================================================================
# validate.R —— 批次文件格式与逐行校验（纯函数，不接触数据库）
#
# 文件格式（CSV，UTF-8，首行表头，列顺序不限，不允许多余列）：
#
#   月报 monthly.csv:
#     region_id,month,revision,op,y,x1,x2
#   上线登记 registration.csv:
#     region_id,revision,op,first_treat_month
#
# 规则：
#  * region_id 非空；month 严格 YYYY-MM（01..12，真实存在的月份）；
#  * revision 严格十进制正整数（无前导零，<= 2147483647）；
#  * op 为 upsert 或 delete（区分大小写）；
#  * delete 行不得携带数值或 first_treat_month；
#  * 数值字段允许空单元格或字面量 NA 表示显式缺失；其余必须是有限数字，
#    拒绝 Inf/NaN、非数值字符串；
#  * first_treat_month 为严格 YYYY-MM 或字面量 never；
#  * 同一批内同一业务键同一 revision 若内容一致则折叠为重复，内容不同则整批失败；
#  * 该判断对更高版本的存在不敏感：先找冲突，再决定当前版本。
# =============================================================================

LEDGER_MONTHLY_COLS <- c("region_id", "month", "revision", "op",
                         "y", "x1", "x2")
LEDGER_REGISTRATION_COLS <- c("region_id", "revision", "op",
                              "first_treat_month")

LEDGER_KIND_MONTHLY <- "monthly"
LEDGER_KIND_REGISTRATION <- "registration"

# 共享校验正则：标量 helper 与 parse_batch_file 的向量化路径使用同一套规则
MONTH_RE    <- "^\\d{4}-(0[1-9]|1[0-2])$"
REVISION_RE <- "^[1-9][0-9]{0,9}$"
NUMERIC_RE  <- "^[+-]?(\\d+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$"

# ---- 单元格工具 --------------------------------------------------------------
trim_cell <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x, which = "both")
}

ledger_error_cell <- function(field, row, value, why) {
  list(field = field, row = row, value = value, why = why)
}

format_cell_errors <- function(errs, path) {
  lines <- vapply(errs, function(e)
    sprintf("数据行 %d 字段 %s='%s'：%s", e$row, e$field, e$value, e$why),
    character(1))
  paste0(path, " 格式错误：\n  ", paste(lines, collapse = "\n  "))
}

# 严格 YYYY-MM，并验证日历真实性（拒绝 2024-13、2024-02-30 等）
is_valid_month <- function(x) {
  x <- trim_cell(x)
  if (!grepl(MONTH_RE, x)) return(FALSE)
  d <- suppressWarnings(as.Date(paste0(x, "-01"), format = "%Y-%m-%d"))
  if (is.na(d)) return(FALSE)
  identical(format(d, "%Y-%m"), x)
}

# 严格正整数 revision
parse_revision <- function(x, row, field = "revision") {
  x <- trim_cell(x)
  if (!grepl(REVISION_RE, x))
    return(list(value = NA_integer_,
                error = ledger_error_cell(field, row, x,
                  "revision 必须是十进制正整数（无前导零、无符号、无小数）")))
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v) || v > LEDGER_MAX_REVISION)
    return(list(value = NA_integer_,
                error = ledger_error_cell(field, row, x,
                  paste0("revision 超出范围 1..", LEDGER_MAX_REVISION))))
  list(value = as.integer(v), error = NULL)
}

# 数值：空 / 字面量 NA => 显式缺失；否则必须有限。
parse_numeric_cell <- function(x, row, field) {
  x <- trim_cell(x)
  if (!nzchar(x) || identical(x, "NA"))
    return(list(value = NA_real_, error = NULL))
  if (!grepl(NUMERIC_RE, x))
    return(list(value = NA_real_,
                error = ledger_error_cell(field, row, x,
                  "必须是有限数值，或空/NA 表示显式缺失")))
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v) || !is.finite(v))
    return(list(value = NA_real_,
                error = ledger_error_cell(field, row, x,
                  "数值溢出或非有限（Inf/NaN 不接受）")))
  list(value = v, error = NULL)
}

parse_first_treat_cell <- function(x, row) {
  x <- trim_cell(x)
  if (!nzchar(x))
    return(list(value = NA_character_,
                error = ledger_error_cell("first_treat_month", row, x,
                  "upsert 登记必须给出 YYYY-MM 或 never")))
  if (identical(x, "never"))
    return(list(value = "never", error = NULL))
  if (!is_valid_month(x))
    return(list(value = NA_character_,
                error = ledger_error_cell("first_treat_month", row, x,
                  "必须是严格 YYYY-MM 或字面量 never")))
  list(value = x, error = NULL)
}

# ---- 文件读取 ----------------------------------------------------------------
read_csv_strict <- function(path, kind) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    stop_ledger("missing_file", paste0(kind, " 文件路径为空"))
  if (!file.exists(path))
    stop_ledger("file_not_found", paste0(kind, " 文件不存在: ", path))
  required <- if (kind == LEDGER_KIND_MONTHLY) LEDGER_MONTHLY_COLS
              else LEDGER_REGISTRATION_COLS
  dt <- tryCatch(
    data.table::fread(path, colClasses = "character", na.strings = NULL,
                      strip.white = FALSE, blank.lines.skip = TRUE,
                      check.names = FALSE, encoding = "UTF-8",
                      showProgress = FALSE),
    error = function(e) stop_ledger("file_format_error",
      paste0("无法解析 CSV: ", path, " | ", conditionMessage(e))))
  if (!is.data.frame(dt) || nrow(dt) == 0L)
    stop_ledger("empty_batch_file",
                paste0("文件没有数据行: ", path,
                       "（批次至少要有一条记录）"))
  nm <- names(dt)
  nm <- sub("^﻿", "", nm)
  setnames(dt, nm)
  if (anyDuplicated(names(dt)))
    stop_ledger("file_format_error",
                paste0("表头存在重复列名: ", path))
  miss <- setdiff(required, names(dt))
  extra <- setdiff(names(dt), required)
  if (length(miss) > 0L || length(extra) > 0L) {
    stop_ledger("file_format_error", paste0(
      path, " 表头必须恰好为 [", paste(required, collapse = ","), "]",
      if (length(miss) > 0L) paste0("；缺少 [", paste(miss, collapse = ","), "]") else "",
      if (length(extra) > 0L) paste0("；多余 [", paste(extra, collapse = ","), "]") else ""))
  }
  setcolorder(dt, required)
  dt
}

# 业务内容指纹：只含 op 与业务字段（不含键和 revision），数值用 17 位有效数字。
# 该字符串即内容身份：同版本比较时逐字比较，比哈希更直接且无碰撞风险。
payload_string <- function(kind, op, vals) {
  num <- function(v) ifelse(is.na(v), "<NA>", sprintf("%.17g", v))
  if (kind == LEDGER_KIND_MONTHLY) {
    paste0("op=", op, "|y=", num(vals$y), "|x1=", num(vals$x1),
           "|x2=", num(vals$x2))
  } else {
    paste0("op=", op, "|first_treat_month=",
           ifelse(is.na(vals$first_treat_month), "<NA>",
                  vals$first_treat_month))
  }
}

# 仅用于“批次文件组合”“导出内容”等单值场景（每次导入/导出最多调用一次）
file_hash <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  unname(tools::md5sum(path))
}
text_hash <- function(s) {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  writeBin(charToRaw(enc2utf8(s)), tf)
  unname(tools::md5sum(tf))
}

# ---- 解析并校验单个批次文件 ------------------------------------------------
# 逐列向量化校验（批次可能有数千行）；错误按行号聚合后一次性报告。
# 同键同版本内容冲突在此直接终止，不进入任何写事务。
parse_batch_file <- function(path, kind) {
  raw <- read_csv_strict(path, kind)
  raw <- as.data.table(lapply(raw, function(z) trimws(as.character(z))))
  n <- nrow(raw)
  errs <- list()
  add_errs <- function(sel, field, why, values = NULL) {
    idx <- which(sel)
    if (is.null(values)) values <- raw[[field]]
    for (i in idx)
      errs[[length(errs) + 1L]] <<- ledger_error_cell(
        field, i + 1L, values[i], why)
  }

  rid <- raw$region_id
  bad_rid_empty <- !nzchar(rid)
  bad_rid_ctrl <- !bad_rid_empty &
    grepl("[\x01-\x1f\x7f]", rid, useBytes = TRUE)
  bad_rid_len <- !bad_rid_empty &
    nchar(rid, type = "bytes") > 512L
  add_errs(bad_rid_empty, "region_id", "region_id 不能为空")
  add_errs(bad_rid_ctrl, "region_id", "region_id 不能包含控制字符")
  add_errs(bad_rid_len, "region_id", "region_id 过长（>512 字节）")

  bad_rev_fmt <- !grepl(REVISION_RE, raw$revision)
  rev_val <- suppressWarnings(as.numeric(raw$revision))
  bad_rev_range <- !bad_rev_fmt &
    (is.na(rev_val) | rev_val > LEDGER_MAX_REVISION)
  add_errs(bad_rev_fmt, "revision",
           "revision 必须是十进制正整数（无前导零、无符号、无小数）")
  add_errs(bad_rev_range, "revision",
           paste0("revision 超出范围 1..", LEDGER_MAX_REVISION))

  bad_op <- !raw$op %in% c("upsert", "delete")
  add_errs(bad_op, "op", "op 必须是 upsert 或 delete")

  out <- data.table(
    region_id = ifelse(bad_rid_empty | bad_rid_ctrl | bad_rid_len,
                       NA_character_, rid),
    revision = ifelse(bad_rev_fmt | bad_rev_range, NA_integer_, rev_val),
    op = ifelse(bad_op, NA_character_, raw$op))

  if (kind == LEDGER_KIND_MONTHLY) {
    mth_ok <- grepl(MONTH_RE, raw$month)
    out[, month := ifelse(mth_ok, raw$month, NA_character_)]
    add_errs(!mth_ok, "month", "月份必须是严格 YYYY-MM 且 01..12")

    for (f in c("y", "x1", "x2")) {
      v <- raw[[f]]
      missing <- !nzchar(v) | v == "NA"
      numeric_ok <- grepl(NUMERIC_RE, v)
      num <- suppressWarnings(as.numeric(v))
      bad_fmt <- !missing & !numeric_ok
      bad_inf <- !missing & numeric_ok & (is.na(num) | !is.finite(num))
      add_errs(bad_fmt, f, "必须是有限数值，或空/NA 表示显式缺失")
      add_errs(bad_inf, f, "数值溢出或非有限（Inf/NaN 不接受）")
      out[[f]] <- ifelse(missing | bad_fmt | bad_inf, NA_real_, num)
    }
    del <- !is.na(out$op) & out$op == "delete"
    has_payload <- !is.na(out$y) | !is.na(out$x1) | !is.na(out$x2)
    add_errs(del & has_payload, "delete_payload",
             "delete 行只能带键和 revision，数值字段必须为空/NA",
             values = ifelse(is.na(out$y), "", as.character(out$y)))
    key_cols <- c("region_id", "month")
  } else {
    ft <- raw$first_treat_month
    del <- !is.na(out$op) & out$op == "delete"
    ft_missing <- !nzchar(ft) | ft == "NA"
    ft_never <- ft == "never"
    ft_month <- grepl(MONTH_RE, ft)
    add_errs(del & !ft_missing, "delete_payload",
             "delete 行只能带键和 revision，first_treat_month 必须为空/NA",
             values = ft)
    add_errs(!del & !ft_missing & !ft_never & !ft_month,
             "first_treat_month", "必须是严格 YYYY-MM 或字面量 never")
    out[, first_treat_month := ifelse(
      ft_never, "never",
      ifelse(ft_month & !del, ft, NA_character_))]
    if (any(del & ft_missing)) out[del & ft_missing, first_treat_month := NA_character_]
    key_cols <- "region_id"
  }

  if (length(errs) > 0L) {
    ord <- order(vapply(errs, function(e) e$row, 0L))
    errs <- errs[ord]
    stop_ledger("file_validation_error",
                format_cell_errors(errs, path), details = errs)
  }
  out$csv_row <- seq_len(n) + 1L

  # ---- 批内同键同版本：内容一致=重复，内容不同=冲突 -----------------------
  if (kind == LEDGER_KIND_MONTHLY) {
    payload <- payload_string(kind, out$op,
      list(y = out$y, x1 = out$x1, x2 = out$x2))
  } else {
    payload <- payload_string(kind, out$op,
      list(first_treat_month = out$first_treat_month))
  }
  set(out, j = "payload", value = payload)

  dup_groups <- out[, .(n = .N, n_payload = uniqueN(payload)),
                    by = c(key_cols, "revision")]
  conflicts <- dup_groups[n_payload > 1L]
  if (nrow(conflicts) > 0L) {
    details <- lapply(seq_len(min(nrow(conflicts), 20L)), function(i) {
      cc <- conflicts[i]
      list(region_id = cc$region_id,
           month = if ("month" %in% key_cols) cc$month else NULL,
           revision = cc$revision,
           n_rows = cc$n)
    })
    txt <- paste(vapply(seq_len(nrow(conflicts)), function(i) {
      cc <- conflicts[i]
      paste0(cc$region_id,
             if ("month" %in% key_cols) paste0("/", cc$month) else "",
             "@r", cc$revision)
    }, character(1)), collapse = ", ")
    stop_ledger("same_revision_conflict",
                paste0("同一批次内同一业务键存在相同 revision 的不同内容: ", txt,
                       "；即使本批还包含更高版本，整批也不导入"),
                details = details)
  }

  # 折叠完全相同的重复行（保留最早出现的一行）
  dup_in_file <- n - uniqueN(out, by = c(key_cols, "revision"))
  keep <- !duplicated(out, by = c(key_cols, "revision"))
  out <- out[keep]
  if (kind == LEDGER_KIND_MONTHLY)
    out <- out[, .(region_id, month, revision, op, y, x1, x2,
                   payload, csv_row)]
  else
    out <- out[, .(region_id, revision, op, first_treat_month,
                   payload, csv_row)]
  list(rows = out, n_rows_raw = n, n_duplicate_in_file = dup_in_file,
       md5 = file_hash(path))
}

# 批次整体契约：至少一类文件、batch_id/source_id 合法
validate_batch_contract <- function(batch_id, source_id, monthly, registration) {
  if (is.null(batch_id) || length(batch_id) != 1L || is.na(batch_id) ||
      !nzchar(trimws(batch_id)))
    stop_ledger("missing_batch_id", "必须提供非空 batch_id")
  batch_id <- trimws(batch_id)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._:-]{0,127}$", batch_id))
    stop_ledger("invalid_batch_id",
                paste0("batch_id 非法（允许 ASCII 字母数字及 . _ : -，以字母数字开头，<=128）: ",
                       batch_id))
  if (is.null(monthly) && is.null(registration))
    stop_ledger("empty_batch", "批次必须至少包含 monthly 或 registration 文件之一")
  if (length(source_id) != 1L || is.na(source_id) || !nzchar(source_id))
    stop_ledger("source_id_missing", "账本缺少 source_id，拒绝导入")
  list(batch_id = batch_id)
}
