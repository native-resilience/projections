# ledger.R — fingerprints of finished work.
#
# One row per (stage, land_id, item) with the key it was built from. A stage
# redoes an item only when the key it would build from differs from the
# recorded one, so reruns compute only what is new or changed. The ledger is
# a small parquet mirrored at s3://<bucket>/<prefix>/_state/; workers never
# write it — they return rows and the main process records them.

DIR_STATE     <- Sys.getenv("NR_STATE_DIR", unset = "_state")
LEDGER_FILE   <- file.path(DIR_STATE, "ledger.parquet")
LANDSETS_FILE <- file.path(DIR_STATE, "landsets.parquet")

ledger_empty <- function() {
  tibble::tibble(stage = character(), land_id = character(),
                 item = character(), key = character(),
                 written_at = as.POSIXct(character(), tz = "UTC"))
}

nr_state_pull <- function() {
  if (nr_publish())
    s3_pull(s3_bucket_name, paste0(s3_prefix, "/_state"), DIR_STATE)
  invisible(TRUE)
}

nr_state_push <- function() {
  if (nr_publish() && dir.exists(DIR_STATE))
    s3_push(s3_bucket_name, paste0(s3_prefix, "/_state"), DIR_STATE,
            delete = FALSE)
  invisible(TRUE)
}

ledger_read <- function(file = LEDGER_FILE) {
  if (!file.exists(file)) return(ledger_empty())
  arrow::read_parquet(file)
}

ledger_write <- function(ledger, file = LEDGER_FILE) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  ledger %>%
    dplyr::arrange(stage, land_id, item) %>%
    arrow::write_parquet(file, compression = "zstd")
  invisible(ledger)
}

# Insert or replace rows by (stage, land_id, item), then persist.
ledger_record <- function(rows, file = LEDGER_FILE) {
  if (nrow(rows) == 0) return(invisible(ledger_read(file)))
  rows$written_at <- as.POSIXct(Sys.time(), tz = "UTC")
  ledger_read(file) %>%
    dplyr::anti_join(rows, by = c("stage", "land_id", "item")) %>%
    dplyr::bind_rows(rows) %>%
    ledger_write(file)
}

# Rows of `want` (stage, land_id, item, key) whose key is not already recorded.
ledger_pending <- function(want, stage_name, ledger = ledger_read()) {
  if (nr_force(stage_name)) return(want)
  want %>%
    dplyr::anti_join(dplyr::filter(ledger, stage == stage_name),
                     by = c("stage", "land_id", "item", "key"))
}

nr_hash <- function(...) {
  digest::digest(list(...), algo = "xxhash64")
}
