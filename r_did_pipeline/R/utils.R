# =============================================================================
# utils.R —— 通用工具：依赖加载、日志、配置读取、哈希与结果身份
# 被管线所有脚本 source 使用。无包外副作用。
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

# ---- 日志（带时间戳，同时写入控制台与运行日志文件）---------------------------
.log_con <- NULL
init_log <- function(log_path) {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  .log_con <<- file(log_path, open = "wt")
  invisible(NULL)
}
log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", ...)
  message(msg)
  if (!is.null(.log_con)) {
    writeLines(msg, .log_con)
    flush(.log_con)
  }
}
close_log <- function() if (!is.null(.log_con)) close(.log_con)

# ---- 配置读取与校验 -----------------------------------------------------------
DEFAULT_CONFIG <- list(
  # 输入/输出
  data_path        = NULL,
  output_dir       = "output",
  correction_path  = NULL,          # 可选：处理时间更正 CSV
  # 列名映射
  col_unit         = "region_id",
  col_time         = "month",       # 可解析为 Date 的 YYYY-MM-01 字段
  col_outcome      = "y",
  col_treat        = "treat",
  col_first_treat  = "first_treat_month",  # Date 或 YYYY-MM-01；Inf 用 NA
  never_treat_code = NULL,          # 永不处理组的 first_treat_month 编码，如 "never"
  covariates       = list("x1", "x2"),
  cluster_var      = NULL,          # NULL => 按地区层级聚类
  # 样本窗口（含端点，YYYY-MM-01；NULL 表示用数据全窗口）
  window_start     = NULL,
  window_end       = NULL,
  # 对照策略：never-treated / not-yet-treated
  control_group    = "not-yet-treated",
  # 预处理检验窗口（事件时间，负数；窗口之外的事件期不进入 pretest）
  pretest_window   = list(-3L, -1L),
  # 事件研究聚合权重："group"（did 默认，组份额）或 "simple"
  es_weight        = "group",
  # 聚类层与随机种子
  cluster_level    = "unit",        # unit(地区) / time(月份) / 自定义列名
  seed             = 20240101L,
  biters           = 1000L,         # MPC 自举迭代次数
  min_group_size   = 5L,            # 与 did 内部阈值一致，用于预飞行可识别性
  # 协变量全缺地区的处理：exclude（排除并列明，唯一支持的预设规则）
  fully_missing_cov_rule = "exclude"
)

load_config <- function(path) {
  if (!file.exists(path)) stop("配置文件不存在: ", path)
  user <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  # jsonlite 会把 JSON null 解析成空 list，统一规范为 NULL
  user <- lapply(user, function(z)
    if (is.list(z) && length(z) == 0L) NULL else z)
  cfg <- DEFAULT_CONFIG
  for (nm in names(user)) cfg[[nm]] <- user[[nm]]
  validate_config(cfg)
  cfg
}

validate_config <- function(cfg) {
  stopifnot(is.character(cfg$control_group),
            cfg$control_group %in% c("never-treated", "not-yet-treated"))
  stopifnot(is.character(cfg$es_weight),
            cfg$es_weight %in% c("group", "simple"))
  stopifnot(cfg$fully_missing_cov_rule == "exclude")
  if (!is.null(cfg$pretest_window)) {
    pw <- unlist(cfg$pretest_window)
    stopifnot(length(pw) == 2L, pw[1] < pw[2], pw[2] < 0)
  }
  if (!is.null(cfg$window_start) && !is.null(cfg$window_end))
    stopifnot(cfg$window_start <= cfg$window_end)
  invisible(TRUE)
}

# ---- 月份字段解析为 Date（每月首日）------------------------------------------
parse_month <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- as.character(x)
  # 允许 "YYYY-MM-01" / "YYYY-MM"
  x2 <- ifelse(grepl("^\\d{4}-\\d{2}$", x), paste0(x, "-01"), x)
  d <- suppressWarnings(as.Date(x2, format = "%Y-%m-%d"))
  non_na <- x[!is.na(x)]
  if (length(non_na) > 0L && all(is.na(d[!is.na(x)])))
    stop("时间字段无法解析为 YYYY-MM-DD: ", paste(unique(non_na), collapse = ","))
  d
}

# ---- 文件/配置哈希 -----------------------------------------------------------
file_md5 <- function(path) {
  if (is.null(path) || length(path) != 1L || is.na(path) ||
      !is.character(path) || !nzchar(path) || !file.exists(path))
    return(NA_character_)
  unname(tools::md5sum(path))
}
text_md5 <- function(x) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeLines(paste(x, collapse = ""), tmp, useBytes = TRUE)
  unname(tools::md5sum(tmp))
}
canonical_json <- function(obj) {
  jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", digits = NA,
                   pretty = FALSE)
}

# ---- 结果身份：快照哈希 + 配置哈希 + 包版本 + 窗口/对照/种子等关键参数 --------
# 样本窗口变化、处理时间更正或对照策略变化都会改变哈希，从而生成新版本。
build_identity <- function(cfg, snapshot_path, panel_meta, n_rows_analytic) {
  pkg_v <- as.character(utils::packageVersion("did"))
  r_v   <- paste(R.version$major, R.version$minor, sep = ".")
  key <- list(
    snapshot_md5      = file_md5(snapshot_path),
    correction_md5    = file_md5(cfg$correction_path),
    config_canonical  = canonical_json(cfg),
    window_start      = panel_meta$window_start,
    window_end        = panel_meta$window_end,
    control_group     = cfg$control_group,
    es_weight         = cfg$es_weight,
    pretest_window    = unlist(cfg$pretest_window),
    cluster_level     = cfg$cluster_level,
    seed              = cfg$seed,
    biters            = cfg$biters,
    n_rows_analytic   = n_rows_analytic,
    R_version         = r_v,
    did_version       = pkg_v,
    data_table_version = as.character(utils::packageVersion("data.table"))
  )
  hash <- substr(text_md5(canonical_json(key)), 1, 12)
  list(version_id = paste0("v_", format(Sys.Date(), "%Y%m%d"), "_", hash),
       hash_inputs = key, did_version = pkg_v, r_version = r_v)
}

ensure_dir <- function(...) {
  p <- file.path(...)
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

write_csv <- function(dt, path) {
  data.table::fwrite(dt, path)
  log_msg("写出: ", path, " (", nrow(dt), " 行)")
}
