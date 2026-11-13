# =============================================================================
# core.R —— 接入账本的连接、表结构、初始化与升级（schema v1）
#
# 设计约束：
#  * 每个 SQLite 文件只绑定一个 source_id（存于 meta，不提供改绑命令）；
#  * 所有写入在一个显式的 BEGIN IMMEDIATE 事务中完成：先拿库级写锁，再做
#    修订号比较与写入，多个进程之间不存在“检查—写入”窗口；
#  * revision 在业务键（月报 = region_id+month；登记 = region_id）内比较，
#    与文件到达顺序无关；
#  * 账本保留每个业务键的全部版本与接收批次。当前有效记录由“最高 revision”
#    派生：同 revision 不允许两种内容，因此 delete 一旦拿到最高修订号，
#    低版本 upsert 即使后到也只能进入历史，不能复活数据；
#  * 版本行 append-only；当前指针表可更新，但每次都在同一事务内与版本行一致。
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(data.table)
})

LEDGER_SCHEMA_VERSION <- 1L
LEDGER_MAX_REVISION <- 2147483647L

# ---- 错误对象：命令层把错误转为 JSON（退出码 2），测试可直接断言 class ----
ledger_error <- function(code, message, details = NULL) {
  structure(
    class = c("ledger_error", "error", "condition"),
    list(message = message, call = NULL,
         code = code, details = details)
  )
}

stop_ledger <- function(code, message, details = NULL) {
  stop(ledger_error(code, message, details))
}

is_ledger_error <- function(e) inherits(e, "ledger_error")

# ---- 日志（命令层 stdout 只放 JSON 结果，日志写 stderr）---------------------
ledger_log <- function(...) {
  message(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), " | ", ...)
}

now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

# ---- 路径与连接 --------------------------------------------------------------
default_ledger_db <- function() {
  Sys.getenv("LEDGER_DB", unset = NA_character_)
}

# 每条连接都必须单独设置 busy_timeout；WAL 是库级持久设置，只在非 WAL 时切换，
# 避免并发连接在别人的写事务中执行 journal_mode 切换。
open_ledger <- function(db_path, create = FALSE, quiet = FALSE) {
  if (is.null(db_path) || length(db_path) != 1L || is.na(db_path) ||
      !nzchar(db_path)) {
    stop_ledger("missing_db_path",
                "未指定账本库路径；请用 --db 或环境变量 LEDGER_DB")
  }
  exists_before <- file.exists(db_path)
  if (!exists_before && !isTRUE(create)) {
    stop_ledger("db_not_found",
                paste0("账本库不存在: ", db_path,
                       "；请先运行 R/ledger/init_ledger.R"))
  }
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 30000")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(con, "PRAGMA synchronous = FULL")
  jm <- DBI::dbGetQuery(con, "PRAGMA journal_mode")[[1]]
  if (!identical(tolower(jm), "wal"))
    DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  if (!isTRUE(quiet)) {
    ledger_log("连接账本: ", normalizePath(db_path, mustWork = FALSE))
  }
  attr(con, "ledger_path") <- normalizePath(db_path, mustWork = FALSE)
  # 任何入口都不允许操作“比代码更新”的库；较旧的库必须经 init_ledger.R 升级，
  # 避免读写代码在未迁移的表结构上运行。
  if (has_table(con, "meta")) {
    ver <- suppressWarnings(as.integer(
      DBI::dbGetQuery(con,
        "SELECT value FROM meta WHERE key='schema_version'")$value))
    if (length(ver) == 1L && !is.na(ver) && ver > LEDGER_SCHEMA_VERSION) {
      close_ledger(con)
      stop_ledger("schema_newer_than_code",
                  paste0("账本 schema v", ver, " 比当前代码支持的 v",
                         LEDGER_SCHEMA_VERSION, " 新，请升级代码"))
    }
    if (length(ver) == 1L && !is.na(ver) && ver < LEDGER_SCHEMA_VERSION) {
      close_ledger(con)
      stop_ledger("schema_upgrade_required",
                  paste0("账本 schema v", ver, " 需要升级到 v",
                         LEDGER_SCHEMA_VERSION,
                         "；请先运行 R/ledger/init_ledger.R"))
    }
  }
  con
}

ledger_path_of <- function(con) {
  p <- attr(con, "ledger_path")
  if (is.null(p)) NA_character_ else unname(p)
}

close_ledger <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  invisible(NULL)
}

has_table <- function(con, name) {
  name %in% DBI::dbListTables(con)
}

# ---- 显式事务：BEGIN IMMEDIATE 先取写锁 ---------------------------------------
begin_immediate <- function(con) {
  DBI::dbExecute(con, "BEGIN IMMEDIATE")
  invisible(TRUE)
}

commit_tx <- function(con) {
  DBI::dbExecute(con, "COMMIT")
  invisible(TRUE)
}

rollback_tx <- function(con) {
  try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
  invisible(TRUE)
}

with_immediate_tx <- function(con, code) {
  begin_immediate(con)
  ok <- FALSE
  on.exit({
    if (!ok) rollback_tx(con)
  }, add = TRUE)
  res <- code()
  commit_tx(con)
  ok <- TRUE
  res
}

# ---- 库身份：source_id 绑定 ----------------------------------------------------
ledger_meta <- function(con) {
  if (!has_table(con, "meta"))
    stop_ledger("schema_missing", "账本库尚未初始化（缺少 meta 表）")
  m <- DBI::dbGetQuery(con, "SELECT key, value FROM meta")
  setNames(as.list(m$value), m$key)
}

ledger_source_id <- function(con) {
  meta <- ledger_meta(con)
  sid <- meta[["source_id"]]
  if (is.null(sid) || !nzchar(sid))
    stop_ledger("source_id_missing", "账本库 meta 缺少 source_id")
  sid
}

# ---- schema DDL -------------------------------------------------------------
schema_ddl <- function() {
  list(
    meta = paste(
      "CREATE TABLE IF NOT EXISTS meta (",
      "  key TEXT PRIMARY KEY,",
      "  value TEXT NOT NULL",
      ")"),
    receipt = paste(
      "CREATE TABLE IF NOT EXISTS receipt (",
      "  batch_id TEXT PRIMARY KEY,",
      "  source_id TEXT NOT NULL,",
      "  file_md5 TEXT NOT NULL,",
      "  status TEXT NOT NULL CHECK (status IN ('committed','rejected')),",
      "  summary_json TEXT NOT NULL,",
      "  received_at TEXT NOT NULL",
      ")"),
    monthly_version = paste(
      "CREATE TABLE IF NOT EXISTS monthly_version (",
      "  record_id INTEGER PRIMARY KEY AUTOINCREMENT,",
      "  region_id TEXT NOT NULL,",
      "  month TEXT NOT NULL,",
      "  revision INTEGER NOT NULL CHECK (revision > 0 AND revision <= 2147483647),",
      "  op TEXT NOT NULL CHECK (op IN ('upsert','delete')),",
      "  y REAL, x1 REAL, x2 REAL,",
      "  batch_id TEXT NOT NULL,",
      "  received_at TEXT NOT NULL,",
      "  payload TEXT NOT NULL,",
      "  UNIQUE (region_id, month, revision)",
      ")"),
    ingest_outcome = paste(
      "CREATE TABLE IF NOT EXISTS ingest_outcome (",
      "  outcome_id INTEGER PRIMARY KEY AUTOINCREMENT,",
      "  batch_id TEXT NOT NULL,",
      "  kind TEXT NOT NULL CHECK (kind IN ('monthly','registration')),",
      "  region_id TEXT NOT NULL,",
      "  month TEXT,",
      "  revision INTEGER NOT NULL,",
      "  outcome TEXT NOT NULL,",
      "  csv_row INTEGER,",
      "  FOREIGN KEY (batch_id) REFERENCES receipt(batch_id)",
      ")"),
    # 过时/重复来件的完整内容：不进入版本表，但账本可查。
    arrival_log = paste(
      "CREATE TABLE IF NOT EXISTS arrival_log (",
      "  arrival_id INTEGER PRIMARY KEY AUTOINCREMENT,",
      "  batch_id TEXT NOT NULL,",
      "  kind TEXT NOT NULL CHECK (kind IN ('monthly','registration')),",
      "  region_id TEXT NOT NULL,",
      "  month TEXT,",
      "  revision INTEGER NOT NULL,",
      "  op TEXT NOT NULL,",
      "  y REAL, x1 REAL, x2 REAL,",
      "  first_treat_month TEXT,",
      "  payload TEXT NOT NULL,",
      "  outcome TEXT NOT NULL CHECK (outcome IN ('stale','duplicate')),",
      "  csv_row INTEGER,",
      "  received_at TEXT NOT NULL,",
      "  FOREIGN KEY (batch_id) REFERENCES receipt(batch_id)",
      ")"),
    monthly_current = paste(
      "CREATE TABLE IF NOT EXISTS monthly_current (",
      "  region_id TEXT NOT NULL,",
      "  month TEXT NOT NULL,",
      "  record_id INTEGER NOT NULL UNIQUE,",
      "  PRIMARY KEY (region_id, month),",
      "  FOREIGN KEY (record_id) REFERENCES monthly_version(record_id)",
      ")"),
    registration_version = paste(
      "CREATE TABLE IF NOT EXISTS registration_version (",
      "  record_id INTEGER PRIMARY KEY AUTOINCREMENT,",
      "  region_id TEXT NOT NULL,",
      "  revision INTEGER NOT NULL CHECK (revision > 0 AND revision <= 2147483647),",
      "  op TEXT NOT NULL CHECK (op IN ('upsert','delete')),",
      "  first_treat_month TEXT,",
      "  batch_id TEXT NOT NULL,",
      "  received_at TEXT NOT NULL,",
      "  payload TEXT NOT NULL,",
      "  UNIQUE (region_id, revision)",
      ")"),
    registration_current = paste(
      "CREATE TABLE IF NOT EXISTS registration_current (",
      "  region_id TEXT PRIMARY KEY,",
      "  record_id INTEGER NOT NULL UNIQUE,",
      "  FOREIGN KEY (record_id) REFERENCES registration_version(record_id)",
      ")"),
    export_release = paste(
      "CREATE TABLE IF NOT EXISTS export_release (",
      "  release_id TEXT PRIMARY KEY,",
      "  source_id TEXT NOT NULL,",
      "  created_at TEXT NOT NULL,",
      "  monthly_n INTEGER NOT NULL,",
      "  registration_n INTEGER NOT NULL,",
      "  manifest_json TEXT NOT NULL",
      ")"),
    export_file = paste(
      "CREATE TABLE IF NOT EXISTS export_file (",
      "  release_id TEXT NOT NULL,",
      "  file_name TEXT NOT NULL,",
      "  md5 TEXT NOT NULL,",
      "  n_rows INTEGER NOT NULL,",
      "  PRIMARY KEY (release_id, file_name),",
      "  FOREIGN KEY (release_id) REFERENCES export_release(release_id)",
      ")")
  )
}

create_schema <- function(con, source_id) {
  ddl <- schema_ddl()
  for (sql in ddl) DBI::dbExecute(con, sql)
  now <- now_iso()
  for (kv in list(c("source_id", source_id),
                  c("schema_version", as.character(LEDGER_SCHEMA_VERSION)),
                  c("created_at", now))) {
    DBI::dbExecute(con, "INSERT INTO meta(key, value) VALUES (?, ?)",
                   list(kv[[1]], kv[[2]]))
  }
  invisible(TRUE)
}

# ---- 升级策略 ----------------------------------------------------------------
# v1 是首个版本。未来升级时在此登记 (from, to, function(con))；
# 升级必须在 BEGIN IMMEDIATE 内，失败则回滚，库保持旧版本。
SCHEMA_MIGRATIONS <- list()

upgrade_schema <- function(con, quiet = FALSE) {
  meta <- ledger_meta(con)
  ver <- suppressWarnings(as.integer(meta[["schema_version"]]))
  if (is.na(ver))
    stop_ledger("schema_invalid", "meta.schema_version 不是整数")
  if (ver > LEDGER_SCHEMA_VERSION)
    stop_ledger("schema_newer_than_code",
                paste0("账本 schema v", ver, " 比当前代码支持的 v",
                       LEDGER_SCHEMA_VERSION, " 新，请升级代码"))
  while (ver < LEDGER_SCHEMA_VERSION) {
    mig <- SCHEMA_MIGRATIONS[[as.character(ver)]]
    if (is.null(mig))
      stop_ledger("schema_no_migration",
                  paste0("缺少 v", ver, " -> v", ver + 1L, " 的迁移"))
    with_immediate_tx(con, function() {
      mig(con)
      DBI::dbExecute(con,
        "UPDATE meta SET value = ? WHERE key = 'schema_version'",
        list(as.character(ver + 1L)))
    })
    ver <- ver + 1L
    if (!isTRUE(quiet))
      ledger_log("账本 schema 升级到 v", ver)
  }
  invisible(ver)
}

init_ledger <- function(db_path, source_id) {
  if (is.null(source_id) || length(source_id) != 1L || is.na(source_id) ||
      !nzchar(source_id))
    stop_ledger("missing_source_id", "必须提供非空 source_id")
  if (grepl("[[:space:]]", source_id))
    stop_ledger("invalid_source_id", "source_id 不能包含空白")
  if (file.exists(db_path)) {
    # 已存在的库可能比代码旧（需要升级），因此这里绕开 open_ledger 的版本门禁
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(close_ledger(con), add = TRUE)
    DBI::dbExecute(con, "PRAGMA busy_timeout = 30000")
    DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
    attr(con, "ledger_path") <- normalizePath(db_path, mustWork = FALSE)
    if (!has_table(con, "meta"))
      stop_ledger("db_not_empty_no_ledger",
                  paste0("文件已存在但不是账本库: ", db_path))
    existing <- ledger_source_id(con)
    if (!identical(existing, source_id))
      stop_ledger("source_id_conflict",
                  paste0("该库已绑定 source_id=", existing,
                         "；不能改为 ", source_id))
    upgrade_schema(con, quiet = TRUE)
    ledger_log("账本已存在且 source_id 一致，无需初始化: ", db_path)
    return(invisible(list(status = "exists", db_path = db_path,
                          source_id = source_id)))
  }
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- open_ledger(db_path, create = TRUE, quiet = TRUE)
  on.exit(close_ledger(con), add = TRUE)
  with_immediate_tx(con, function() create_schema(con, source_id))
  ledger_log("初始化账本: ", db_path, " | source_id=", source_id,
             " | schema v", LEDGER_SCHEMA_VERSION)
  invisible(list(status = "created", db_path = db_path,
                 source_id = source_id))
}
