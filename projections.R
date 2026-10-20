# projections.R — annual metrics, ensemble smooths, workbooks, graphs and
# dashboard parquet for every modeled land (reservations, statistical areas,
# Navajo agencies, Hawai‘i climate divisions).
#
# Input: data/daily/land_id=…/ from projections-extract.R (pulled from S3).
# Stages (NR_STAGES, default all):
#   annual     data/annual/land_id=…/part-0.parquet   (long; one row per
#              model × scenario × Year × metric)
#   smooth     data/smoothed/land_id=…/part-0.parquet
#   outputs    output/<name>/ xlsx + pdf + png, upload/<name>.zip, upload/pdf/
#   dashboard  data/{annual,smoothed}-by-variable/variable=…/scenario=…/ and
#              the lookup tables data/variables.parquet, data/lands.parquet
#
# Only work whose fingerprint changed is redone (see R/ledger.R): adding a
# metric to R/metrics.R computes just that metric everywhere.
#
#   Rscript projections.R
#   NR_LANDS=0305R,0010R PUBLISH=0 Rscript projections.R

source("R/setup.R")

if (nr_publish()) {
  s3_preflight()
  nr_state_pull()
}

lands <-
  sf::read_sf("native-land.parquet") %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(modeled)
if (length(nr_env_list("NR_LANDS")))
  lands <- dplyr::filter(lands, land_id %in% nr_env_list("NR_LANDS"))

ledger <- ledger_read()
mt <- metrics_table()

daily_keys <-
  ledger %>%
  dplyr::filter(stage == "daily", land_id %in% lands$land_id) %>%
  dplyr::select(land_id, daily_key = key)

missing_daily <- setdiff(lands$land_id, daily_keys$land_id)
if (length(missing_daily))
  message(length(missing_daily), " modeled lands have no daily data yet: ",
          paste(head(missing_daily, 10), collapse = ", "),
          if (length(missing_daily) > 10) ", …")

# ---- What each land needs ----
want_annual <-
  tidyr::crossing(daily_keys, dplyr::select(mt, metric_id, metric_version)) %>%
  dplyr::mutate(stage = "annual", item = metric_id,
                key = purrr::map2_chr(metric_version, daily_key, nr_hash))

want_smooth <-
  want_annual %>%
  dplyr::left_join(dplyr::select(mt, metric_id, family), by = "metric_id") %>%
  dplyr::mutate(stage = "smooth",
                key = purrr::pmap_chr(list(key, family, SMOOTH_SPEC_VERSION),
                                      nr_hash))

want_outputs <-
  want_smooth %>%
  dplyr::summarise(key = nr_hash(sort(key)), .by = land_id) %>%
  dplyr::left_join(dplyr::select(lands, land_id, name, census_year),
                   by = "land_id") %>%
  dplyr::mutate(stage = "outputs", item = "outputs",
                key = purrr::pmap_chr(list(key, name, census_year,
                                           OUTPUT_SPEC_VERSION), nr_hash))

pending_annual  <- if (nr_stage("annual"))  ledger_pending(dplyr::select(want_annual, stage, land_id, item, key), "annual", ledger) else want_annual[0, c("stage", "land_id", "item", "key")]
pending_smooth  <- if (nr_stage("smooth"))  ledger_pending(dplyr::select(want_smooth, stage, land_id, item, key), "smooth", ledger) else want_smooth[0, c("stage", "land_id", "item", "key")]
pending_outputs <- if (nr_stage("outputs")) ledger_pending(dplyr::select(want_outputs, stage, land_id, item, key), "outputs", ledger) else want_outputs[0, c("stage", "land_id", "item", "key")]

todo <-
  dplyr::bind_rows(pending_annual, pending_smooth, pending_outputs) %>%
  dplyr::summarise(rows = list(dplyr::pick(stage, item, key)), .by = land_id) %>%
  dplyr::left_join(lands, by = "land_id")

message("annual: ", nrow(pending_annual), " land × metric; smooth: ",
        nrow(pending_smooth), "; outputs: ", nrow(pending_outputs), " lands")

# ---- Run, in batches so progress is recorded and pushed as we go ----
if (nrow(todo) > 0) {
  batches <- split(seq_len(nrow(todo)), ceiling(seq_len(nrow(todo)) / 48))
  for (b in batches) {
    res <- nr_map(b, \(i) process_land(todo[i, ], publish),
                  todo = todo, publish = nr_publish())
    ledger_record(dplyr::bind_rows(purrr::map(res, "rows")))
    status <- dplyr::bind_rows(purrr::map(res, "status"))
    if (nrow(status) > 0) {
      status <- dplyr::summarise(status, n = sum(n), .by = status)
      message("smooth fit status (land-years): ",
              paste0(status$status, "=", status$n, collapse = ", "))
    }
    if (nr_publish()) {
      s3_push(s3_bucket_name, file.path(s3_prefix, DIR_ANNUAL), DIR_ANNUAL, delete = FALSE)
      s3_push(s3_bucket_name, file.path(s3_prefix, DIR_SMOOTH), DIR_SMOOTH, delete = FALSE)
      nr_state_push()
    }
  }
}

# ---- Dashboard: by-variable layouts and lookup tables ----
# Rewrites only the variable= partitions whose inputs changed: a variable's
# key is the set of smooth keys for that metric across all lands.
if (nr_stage("dashboard")) {
  # Fingerprint over every land's recorded smooth, not just this run's
  # NR_LANDS subset, so a partial run rebuilds only the variables it touched.
  want_dash <-
    ledger_read() %>%
    dplyr::filter(stage == "smooth", item %in% names(METRICS)) %>%
    dplyr::summarise(key = nr_hash(sort(paste(land_id, key))), .by = item) %>%
    dplyr::transmute(stage = "dashboard", land_id = "*", item, key)
  pending_dash <- ledger_pending(want_dash, "dashboard")
  message("dashboard: ", nrow(pending_dash), " variables to (re)build")

  if (nrow(pending_dash) > 0) {
    if (nr_publish()) {
      s3_pull(s3_bucket_name, file.path(s3_prefix, DIR_ANNUAL), DIR_ANNUAL)
      s3_pull(s3_bucket_name, file.path(s3_prefix, DIR_SMOOTH), DIR_SMOOTH)
    }
    con <- duckdb::dbConnect(duckdb::duckdb())
    DBI::dbExecute(con, paste0("SET threads = ", nr_workers()))
    duckdb::duckdb_register(con, "pending",
                            tibble::tibble(metric_id = pending_dash$item))
    for (src in c("annual", "smoothed")) {
      out <- file.path(DIR_DATA, paste0(src, "-by-variable"))
      staging <- tempfile(src)
      DBI::dbExecute(con, sprintf("
        COPY (
          SELECT metric_id AS variable, * EXCLUDE (metric_id)
          FROM read_parquet('%s/*/*.parquet', hive_partitioning = true)
          WHERE metric_id IN (SELECT metric_id FROM pending)
          ORDER BY variable, scenario, land_id, Year
        ) TO '%s' (FORMAT parquet, COMPRESSION zstd,
                   PARTITION_BY (variable, scenario), FILENAME_PATTERN 'part-{i}')",
        file.path(DIR_DATA, src), staging))
      for (v in pending_dash$item) {
        dest <- file.path(out, paste0("variable=", v))
        unlink(dest, recursive = TRUE)
        dir.create(out, recursive = TRUE, showWarnings = FALSE)
        file.rename(file.path(staging, paste0("variable=", v)), dest)
      }
    }
    duckdb::dbDisconnect(con, shutdown = TRUE)
  }

  # Metrics removed from the registry lose their partitions (and are deleted
  # from S3 by the delete-mirror push below).
  for (d in file.path(DIR_DATA, c("annual-by-variable", "smoothed-by-variable"))) {
    stale <- setdiff(list.files(d), paste0("variable=", names(METRICS)))
    unlink(file.path(d, stale), recursive = TRUE)
  }

  metrics_table() %>%
    dplyr::rename(variable = metric_id) %>%
    arrow::write_parquet(file.path(DIR_DATA, "variables.parquet"))
  sf::read_sf("native-land.parquet") %>%
    sf::st_drop_geometry() %>%
    arrow::write_parquet(file.path(DIR_DATA, "lands.parquet"))

  if (nr_publish()) {
    for (d in c("annual-by-variable", "smoothed-by-variable"))
      s3_push(s3_bucket_name, file.path(s3_prefix, DIR_DATA, d),
              file.path(DIR_DATA, d), delete = TRUE)
    for (f in c("variables.parquet", "lands.parquet"))
      s3_put(s3_bucket_name, file.path(s3_prefix, DIR_DATA, f),
             file.path(DIR_DATA, f),
             content_type = "application/vnd.apache.parquet",
             cache_control = "max-age=3600")
  }
  ledger_record(pending_dash)
  nr_state_push()
}

# Publish upload/ and the data/ prefix with: Rscript projections-publish.R
