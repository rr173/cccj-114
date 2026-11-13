#!/usr/bin/env Rscript
# =============================================================================
# init_ledger.R —— 初始化本地 SQLite 接入账本（或校验已有库的绑定）
#
# 用法:
#   Rscript R/ledger/init_ledger.R --db data/ledger.sqlite --source-id SRC-EAST
#
# 幂等：库已存在且 source_id 一致时只做 schema 升级检查并返回 exists；
# source_id 不一致直接拒绝（一个库只绑定一个来源）。
# =============================================================================

here <- dirname(normalizePath(sub("--file=", "",
          grep("--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = FALSE))
source(file.path(here, "cli.R"), local = FALSE)
load_ledger_lib()

opts <- parse_cli(commandArgs(trailingOnly = TRUE))

run_cli(opts, function(o) {
  db <- require_db(o)
  sid <- cli_opt(o, "source_id")
  if (is.null(sid) || isTRUE(sid) || !nzchar(sid))
    stop_ledger("missing_source_id", "必须提供 --source-id")
  if (file.exists(db)) {
    con <- open_ledger(db, quiet = FALSE)
    on.exit(close_ledger(con), add = TRUE)
    res <- init_ledger(db, sid)
  } else {
    res <- init_ledger(db, sid)
    con <- open_ledger(db, quiet = TRUE)
    on.exit(close_ledger(con), add = TRUE)
  }
  snap <- ledger_snapshot(con)
  list(action = res$status,
       db_path = normalizePath(db, mustWork = TRUE),
       source_id = snap$source_id,
       schema_version = snap$schema_version,
       journal_mode = snap$journal_mode,
       tables = sort(DBI::dbListTables(con)))
})
