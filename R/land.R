# land.R — per-land worker for projections.R: annual metrics, smooths and
# outputs for one land. Lives here so mirai daemons have it after sourcing
# R/setup.R.

land_part <- function(dir, id) file.path(dir, paste0("land_id=", id), "part-0.parquet")

land_pull <- function(dir, id, publish) {
  f <- land_part(dir, id)
  if (publish && !file.exists(f))
    s3_pull(s3_bucket_name, file.path(s3_prefix, dirname(f)), dirname(f))
  if (file.exists(f)) arrow::read_parquet(f) else NULL
}

land_write <- function(x, dir, id) {
  f <- land_part(dir, id)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(x, f, compression = "zstd")
  f
}

# Replace the rows of `ids` in an existing long table and drop metrics that
# are no longer in the registry.
merge_metrics <- function(old, new, ids) {
  if (!is.null(old))
    old <- dplyr::filter(old, !metric_id %in% ids, metric_id %in% names(METRICS))
  dplyr::bind_rows(old, new)
}

process_land <- function(land, publish) {
  id <- land$land_id
  rows <- land$rows[[1]]
  need_annual <- rows$item[rows$stage == "annual"]
  need_smooth <- rows$item[rows$stage == "smooth"]
  need_outputs <- any(rows$stage == "outputs")

  daily <- NULL
  prepared <- NULL
  get_prepared <- function() {
    if (is.null(prepared)) {
      daily <<- land_pull(DIR_DAILY, id, publish)
      stopifnot(!is.null(daily))
      prepared <<- nr_prepare_daily(daily)
    }
    prepared
  }

  annual <- land_pull(DIR_ANNUAL, id, publish)
  if (length(need_annual)) {
    new <- nr_compute_annual(get_prepared(), METRICS[need_annual])
    annual <- merge_metrics(annual, new, need_annual) %>%
      dplyr::arrange(metric_id, model, scenario, Year)
    land_write(annual, DIR_ANNUAL, id)
  }

  smoothed <- land_pull(DIR_SMOOTH, id, publish)
  if (length(need_smooth)) {
    new <- nr_smooth_land(annual, METRICS[need_smooth], land_id = id)
    smoothed <- merge_metrics(smoothed, new, need_smooth) %>%
      dplyr::arrange(metric_id, scenario, Year)
    land_write(smoothed, DIR_SMOOTH, id)
  }

  if (need_outputs) {
    stem <- paste0(land$name, "_CMIP6-climate-projections")
    dir <- file.path(DIR_OUTPUT, land$name)
    unlink(dir, recursive = TRUE)
    xlsx <- nr_write_workbook(file.path(dir, paste0(stem, ".xlsx")),
                              land$name, land$census_year, get_prepared(),
                              annual, smoothed)
    pdf <- nr_save_graphs(nr_plot_land(land$name, annual, smoothed),
                          file.path(dir, paste0(stem, ".pdf")),
                          file.path(dir, "png"))
    nr_compress_pdf(pdf)
    dir.create(file.path(DIR_UPLOAD, "pdf"), recursive = TRUE, showWarnings = FALSE)
    file.copy(pdf, file.path(DIR_UPLOAD, "pdf", basename(pdf)), overwrite = TRUE)
    zipfile <- file.path(normalizePath(DIR_UPLOAD),
                         paste0(gsub(" ", "_", land$name), ".zip"))
    old_wd <- setwd(dir)
    on.exit(setwd(old_wd), add = TRUE)
    nr_zip_land(c(basename(xlsx), basename(pdf), file.path("png", list.files("png"))),
                zipfile)
    setwd(old_wd)
    if (!nr_keep_outputs()) unlink(dir, recursive = TRUE)
  }

  status <- if (!is.null(smoothed)) dplyr::count(smoothed, status) else NULL
  list(rows = dplyr::mutate(rows, land_id = id, .before = 1), status = status)
}
