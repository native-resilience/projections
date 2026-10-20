# extract.R — NEX-GDDP-CMIP6 extraction worker (see projections-extract.R).
# Lives here, not in the script, so mirai daemons have it after sourcing
# R/setup.R.

nex_open <- function(key, access) {
  if (access == "stream") {
    Sys.setenv(AWS_NO_SIGN_REQUEST = "YES", AWS_REGION = "us-west-2",
               GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR")
    return(list(rast = terra::rast(paste0("/vsis3/", NEX_BUCKET, "/", key)),
                file = NULL))
  }
  f <- tempfile(fileext = ".nc")
  processx::run("aws", c("s3", "cp", paste0("s3://", NEX_BUCKET, "/", key), f,
                         "--no-sign-request", "--only-show-errors"))
  list(rast = terra::rast(f), file = f)
}

extract_task <- function(task, lands, landset, access, outdir, publish) {
  lands_rot <- st_rotate(lands)
  e <- terra::ext(sf::st_bbox(lands_rot)) + 0.5

  vals <- purrr::map(stats::setNames(NEX_VARIABLES, NEX_VARIABLES), \(v) {
    key <- task$files$key[task$files$variable == v]
    if (length(key) == 0) return(NULL)   # e.g. MIROC6 huss
    src <- nex_open(key, access)
    on.exit(if (!is.null(src$file)) unlink(src$file))
    r <- terra::crop(src$rast, e)
    dates <- as.Date(terra::time(r))
    m <- exactextractr::exact_extract(r, lands_rot, fun = "weighted_mean",
                                      weights = "area", progress = FALSE)
    list(dates = dates, values = as.matrix(m))
  })

  dates <- purrr::detect(vals, Negate(is.null))$dates
  out <- tidyr::expand_grid(land_id = lands$land_id, date = dates)
  out <- dplyr::left_join(out, tibble::tibble(land_id = lands$land_id,
                                              geom_hash = lands$geom_hash),
                          by = "land_id")
  for (v in NEX_VARIABLES) {
    out[[v]] <- if (is.null(vals[[v]])) NA_real_ else {
      stopifnot(identical(vals[[v]]$dates, dates))
      as.vector(t(vals[[v]]$values))   # land-major, matching expand_grid
    }
  }
  out <- dplyr::mutate(out, task_key = task$task_key, run = task$run,
                       .after = geom_hash)

  rel <- file.path(paste0("model=", task$model), paste0("scenario=", task$scenario),
                   paste0(task$year, "-", landset, ".parquet"))
  dest <- file.path(outdir, rel)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(out, dest, compression = "zstd")
  if (publish)
    s3_put(s3_bucket_name, file.path(s3_prefix, DIR_CACHE, rel), dest,
           content_type = "application/vnd.apache.parquet")

  tibble::tibble(stage = "extract", land_id = landset, item = task$task_id,
                 key = task$task_key)
}
