#!/usr/bin/env Rscript
# =============================================================================
# query_ledger.R —— 账本查询（当前数据 / 历史版本 / 回执 / 批次判定 / 发布）
#
# 用法:
#   Rscript R/ledger/query_ledger.R --db data/ledger.sqlite --what snapshot
#   ... --what current-monthly [--region-id R1] [--month 2024-01]
#   ... --what history-monthly [--region-id R1] [--month 2024-01]
#   ... --what current-registration
#   ... --what history-registration [--region-id R1]
#   ... --what receipts [--batch-id B001]
#   ... --what outcomes [--batch-id B001]
#   ... --what arrivals [--batch-id B001]
#   ... --what releases
#   ... --what check
#
# 输出为单个 JSON 对象（data.table 行以数组返回）。
# =============================================================================

here <- dirname(normalizePath(sub("--file=", "",
          grep("--file=", commandArgs(FALSE), value = TRUE)[1]), mustWork = FALSE))
source(file.path(here, "cli.R"), local = FALSE)
load_ledger_lib()

opts <- parse_cli(commandArgs(trailingOnly = TRUE))

run_cli(opts, function(o) {
  db <- require_db(o)
  con <- open_ledger(db, quiet = TRUE)
  on.exit(close_ledger(con), add = TRUE)
  what <- cli_opt(o, "what", "snapshot")
  region_id <- cli_opt(o, "region_id")
  month <- cli_opt(o, "month")
  batch_id <- cli_opt(o, "batch_id")
  rows_json <- function(dt) {
    if (is.null(dt) || nrow(dt) == 0L) return(list())
    # dataframe="rows" 保证 NA 输出为 null（as.list(dt[i]) 会把 NA 变字符串 "NA"）
    jsonlite::fromJSON(jsonlite::toJSON(
      as.data.frame(dt), auto_unbox = TRUE, null = "null", digits = NA,
      dataframe = "rows"), simplifyVector = FALSE)
  }
  body <- switch(what,
    snapshot = ledger_snapshot(con),
    `current-monthly` = query_current_monthly(con, region_id, month),
    `history-monthly` = query_monthly_history(con, region_id, month),
    `current-registration` = query_current_registration(con, region_id),
    `history-registration` = query_registration_history(con, region_id),
    receipts = query_receipts(con, batch_id),
    outcomes = query_outcomes(con, batch_id),
    arrivals = query_arrivals(con, batch_id),
    releases = query_releases(con),
    check = check_ledger_invariants(con),
    stop_ledger("unknown_what", paste0("未知查询类型: ", what))
  )
  if (is.data.frame(body)) {
    list(what = what, n = nrow(body), rows = rows_json(body))
  } else {
    c(list(what = what), body)
  }
})
