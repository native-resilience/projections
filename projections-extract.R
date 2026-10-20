# projections-extract.R — extract NASA NEX-GDDP-CMIP6 daily data for every
# unit in native-land.parquet and assemble per-land daily parquet.
#
# Meant to run on EC2 in us-west-2, next to s3://nex-gddp-cmip6 (no egress
# cost), with an instance role that can write s3://native-resilience:
#
#   NR_WORKERS=24 Rscript projections-extract.R
#
# Stages (NR_STAGES):
#   extract  one task per model × scenario × year: read the 9 variables, take
#            area-weighted means over each land, write
#            _cache/nex-gddp-cmip6/<version>/model=…/scenario=…/<year>-<landset>.parquet
#   daily    combine the cache into data/daily/land_id=…/part-0.parquet
#
# Incremental: a task is keyed by the md5s of its source files, and each land
# by its geometry hash. A rerun extracts only lands that are new or whose
# geometry changed (into an extra <landset> part) and re-extracts a task only
# if NASA republishes its files. NR_NEX_ACCESS=stream reads over /vsis3/
# instead of copying each file to local disk first (slower; for testing).

source("R/setup.R")

if (nr_publish()) {
  s3_preflight()
  nr_state_pull()
}

lands <- nr_read_lands()
if (length(nr_env_list("NR_LANDS")))
  lands <- dplyr::filter(lands, land_id %in% nr_env_list("NR_LANDS"))

# ---- Task list from NASA's md5 index ----
nex_files <-
  readr::read_table(NEX_INDEX, col_names = c("md5", "key"),
                    col_types = "cc", progress = FALSE) %>%
  dplyr::filter(grepl(paste0("_", NEX_VERSION, "\\.nc$"), key)) %>%
  tidyr::separate_wider_delim(key, delim = "/", cols_remove = FALSE,
                              names = c("root", "model", "scenario", "run",
                                        "variable", "file")) %>%
  dplyr::mutate(year = as.integer(stringr::str_extract(
    file, "(?<=_)\\d{4}(?=_v)"))) %>%
  dplyr::filter(model %in% NEX_MODELS,
                scenario %in% names(SCENARIOS),
                variable %in% NEX_VARIABLES) %>%
  dplyr::select(model, scenario, run, year, variable, key, md5)

# Some models publish more than one ensemble member; keep the first per model.
nex_files <-
  nex_files %>%
  dplyr::filter(run == sort(unique(run))[[1]], .by = model)

tasks <-
  nex_files %>%
  dplyr::arrange(variable) %>%
  dplyr::summarise(files = list(dplyr::pick(variable, key)),
                   task_key = nr_hash(variable, md5),
                   .by = c(model, scenario, run, year)) %>%
  dplyr::mutate(task_id = paste(model, scenario, year, sep = "/")) %>%
  dplyr::arrange(model, scenario, year)

stopifnot("expected all 8 models" = setequal(unique(tasks$model), NEX_MODELS))

# NR_TASKS: regex on task_id ("model/scenario/year"), for smoke tests.
if (nzchar(Sys.getenv("NR_TASKS")))
  tasks <- dplyr::filter(tasks, grepl(Sys.getenv("NR_TASKS"), task_id))
message(nrow(tasks), " model × scenario × year tasks over ",
        nrow(nex_files), " NEX ", NEX_VERSION, " files")

# ---- Which lands does each task still need? ----
read_landsets <- function() {
  if (!file.exists(LANDSETS_FILE))
    return(tibble::tibble(landset = character(), land_id = character(),
                          geom_hash = character()))
  arrow::read_parquet(LANDSETS_FILE)
}

# Recorded extraction rows: land_id holds the landset hash, item the task_id.
extract_done <- function(ledger = ledger_read()) {
  ledger %>%
    dplyr::filter(stage == "extract") %>%
    dplyr::select(task_id = item, task_key = key, landset = land_id) %>%
    dplyr::inner_join(read_landsets(), by = "landset",
                      relationship = "many-to-many")
}

current_lands <- sf::st_drop_geometry(lands)[, c("land_id", "geom_hash")]

# ---- Stage: extract ----
# A task that still fails after its retries is logged and left pending; the
# stage makes up to three passes, so transient S3 errors heal themselves.
# Anything still missing makes the script exit non-zero at the end.
extract_failed <- 0L
if (nr_stage("extract")) {
  for (pass in 1:3) {
    done <- if (nr_force("extract") && pass == 1) extract_done()[0, ] else extract_done()
    need <-
      tasks %>%
      dplyr::select(task_id, task_key) %>%
      tidyr::crossing(current_lands) %>%
      dplyr::anti_join(done, by = c("task_id", "task_key", "land_id", "geom_hash"))

    if (nrow(need) == 0) {
      message("extract: nothing to do")
      extract_failed <- 0L
      break
    }

    # Group tasks that are missing the same lands so they share a landset.
    plan <-
      need %>%
      dplyr::summarise(lands = list(sort(paste(land_id, geom_hash))),
                       .by = c(task_id, task_key)) %>%
      dplyr::mutate(landset = purrr::map_chr(lands, \(l) substr(nr_hash(l), 1, 12)))

    landsets_new <-
      plan %>%
      dplyr::distinct(landset, lands) %>%
      tidyr::unnest(lands) %>%
      tidyr::separate_wider_delim(lands, " ", names = c("land_id", "geom_hash"))
    read_landsets() %>%
      dplyr::bind_rows(landsets_new) %>%
      dplyr::distinct() %>%
      arrow::write_parquet(LANDSETS_FILE)

    message("extract (pass ", pass, "): ", nrow(plan), " tasks for ",
            dplyr::n_distinct(need$land_id), " lands")

    access <- Sys.getenv("NR_NEX_ACCESS", unset = "download")
    landset_ids <- split(landsets_new$land_id, landsets_new$landset)
    lands_file <- normalizePath(".nr-lands-rotated.rds", mustWork = FALSE)
    saveRDS(st_rotate(lands), lands_file)
    extract_failed <- 0L
    batches <- split(seq_len(nrow(plan)), ceiling(seq_len(nrow(plan)) / 200))
    for (b in batches) {
      # Each element carries only its own task; the boundaries go through
      # lands_file, read once per daemon.
      items <- purrr::map(b, \(i) {
        p <- plan[i, ]
        task <- as.list(tasks[tasks$task_id == p$task_id, ])
        task$files <- task$files[[1]]
        list(task = task, landset = p$landset,
             land_ids = landset_ids[[p$landset]])
      })
      rows <- nr_map(
        items,
        \(item) tryCatch(
          extract_task(item, lands_file, access, DIR_CACHE, publish),
          # Daemon output is not captured, so failures travel back as rows.
          error = function(e)
            tibble::tibble(stage = "failed", land_id = item$landset,
                           item = item$task$task_id, key = nr_root_message(e))),
        lands_file = lands_file, access = access, publish = nr_publish()
      )
      rows <- dplyr::bind_rows(rows)
      failed <- dplyr::filter(rows, stage == "failed")
      extract_failed <- extract_failed + nrow(failed)
      for (i in seq_len(nrow(failed)))
        message("extract failed (stays pending): ", failed$item[i], ": ",
                failed$key[i])
      ledger_record(dplyr::filter(rows, stage != "failed"))
      nr_state_push()
    }
    if (extract_failed == 0L) break
  }
}

# ---- Stage: daily ----
# A land's daily partition is rebuilt when the set of tasks covering its
# current geometry changes. Lands not yet covered by every task are skipped.
if (nr_stage("daily")) {
  if (nr_publish())
    s3_pull(s3_bucket_name, file.path(s3_prefix, DIR_CACHE), DIR_CACHE)

  cover <-
    extract_done() %>%
    dplyr::semi_join(dplyr::select(tasks, task_id, task_key),
                     by = c("task_id", "task_key")) %>%
    dplyr::semi_join(current_lands, by = c("land_id", "geom_hash"))

  want <-
    cover %>%
    dplyr::summarise(n_tasks = dplyr::n_distinct(task_id),
                     key = nr_hash(geom_hash[[1]], sort(unique(task_key))),
                     .by = land_id) %>%
    dplyr::filter(n_tasks == nrow(tasks)) %>%
    dplyr::transmute(stage = "daily", land_id, item = "daily", key)

  incomplete <- setdiff(current_lands$land_id, want$land_id)
  if (length(incomplete))
    message("daily: ", length(incomplete),
            " lands are not fully extracted yet and were skipped")

  pending <- ledger_pending(want, "daily")
  message("daily: ", nrow(pending), " lands to (re)build")

  if (nrow(pending) > 0) {
    con <- duckdb::dbConnect(duckdb::duckdb())
    on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbExecute(con, paste0("SET threads = ", nr_workers()))
    duckdb::duckdb_register(con, "valid_lands",
                            dplyr::filter(current_lands, land_id %in% pending$land_id))
    duckdb::duckdb_register(con, "valid_tasks",
                            dplyr::select(tasks, task_key))

    staging <- tempfile("daily-")
    for (chunk in split(pending$land_id, ceiling(seq_along(pending$land_id) / 50))) {
      duckdb::duckdb_register(con, "chunk", tibble::tibble(land_id = chunk))
      DBI::dbExecute(con, glue::glue("
        COPY (
          SELECT c.land_id, c.model, c.scenario, c.run, c.date,
                 {paste0('c.', NEX_VARIABLES, collapse = ', ')}
          FROM read_parquet('{DIR_CACHE}/**/*.parquet', hive_partitioning = true,
                            filename = true) c
          JOIN valid_lands v USING (land_id, geom_hash)
          JOIN valid_tasks t USING (task_key)
          JOIN chunk k USING (land_id)
          QUALIFY row_number() OVER (PARTITION BY c.land_id, c.model, c.scenario, c.date
                                     ORDER BY c.filename DESC) = 1
          ORDER BY c.land_id, c.scenario, c.model, c.date
        ) TO '{staging}' (FORMAT parquet, COMPRESSION zstd,
                          PARTITION_BY (land_id), FILENAME_PATTERN 'part-{{i}}',
                          OVERWRITE_OR_IGNORE)"))
      duckdb::duckdb_unregister(con, "chunk")
    }

    for (id in pending$land_id) {
      dest <- file.path(DIR_DAILY, paste0("land_id=", id))
      unlink(dest, recursive = TRUE)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.rename(file.path(staging, paste0("land_id=", id)), dest)
    }

    if (nr_publish()) {
      s3_push(s3_bucket_name, file.path(s3_prefix, DIR_DAILY), DIR_DAILY,
              delete = FALSE)
      s3_verify(s3_bucket_name, file.path(s3_prefix, DIR_DAILY), DIR_DAILY,
                allow_extra = character(0), expect_exact = FALSE)
    }
    ledger_record(pending)
    nr_state_push()
  }
}

if (extract_failed > 0L)
  stop(extract_failed, " extraction task(s) still failing after 3 passes; ",
       "rerun to retry them")
