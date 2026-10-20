# setup.R — packages, options and shared constants for the projections
# pipeline. Sourced by every projections-*.R script.

# Install once (gamlss2 is not on CRAN; it comes from the gamlss-dev universe):
# options(repos = c(gamlss = "https://gamlss-dev.r-universe.dev",
#                   CRAN = "https://cloud.r-project.org"))
# pak::pak(c("magrittr", "tidyverse", "sf", "terra", "exactextractr", "arrow",
#            "duckdb", "mirai", "carrier", "mgcv", "gamlss2", "gamlss.dist",
#            "RcppRoll", "weathermetrics", "units", "openxlsx", "png", "ragg",
#            "arcgislayers", "tigris", "processx", "digest", "jsonlite",
#            "curl", "zip"), ask = FALSE)

suppressPackageStartupMessages({
  library(magrittr)
  library(ggplot2)
  # sf registers the dplyr/vctrs methods for sf objects; without it loaded,
  # a daemon handed an sf object cannot subset it.
  library(sf)
})

source("R/s3-archive.R")
source("R/geo.R")
source("R/ledger.R")
source("R/metrics.R")
source("R/smooth.R")
source("R/outputs.R")
source("R/extract.R")
source("R/land.R")

openxlsx::openxlsx_setOp("dateFormat", "yyyy-mm-dd")
openxlsx::openxlsx_setOp("tableStyle", "TableStyleLight1")
openxlsx::openxlsx_setOp("numFmt", "NUMBER")

# ---- Run configuration (environment) ----
# NR_STAGES  comma list of stages to run (default: all of a script's stages)
# NR_LANDS   comma list of land_ids to restrict to (default: all)
# NR_FORCE   comma list of stages to rebuild regardless of fingerprints
# NR_WORKERS mirai daemons (default: cores - 1)
# PUBLISH    0 builds locally without touching S3
nr_env_list <- function(var) {
  x <- Sys.getenv(var, unset = "")
  if (!nzchar(x)) return(character(0))
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

nr_stage <- function(stage, all = TRUE) {
  stages <- nr_env_list("NR_STAGES")
  if (length(stages) == 0) all else stage %in% stages
}

nr_force <- function(stage) stage %in% nr_env_list("NR_FORCE")

nr_workers <- function() {
  as.integer(Sys.getenv("NR_WORKERS",
                        unset = max(1L, parallel::detectCores() - 1L)))
}

nr_publish <- function() Sys.getenv("PUBLISH", unset = "1") != "0"

# ---- S3 ----
s3_bucket_name <- Sys.getenv("S3_BUCKET", unset = "native-resilience")
s3_prefix      <- Sys.getenv("S3_PREFIX", unset = "projections")
nr_base_url    <- paste0(Sys.getenv("CLOUDFRONT_BASE",
                                    unset = "https://data.native-resilience.com"),
                         "/", s3_prefix, "/")

# ---- Source data: NASA NEX-GDDP-CMIP6 ----
NEX_BUCKET  <- "nex-gddp-cmip6"
NEX_VERSION <- "v2.0"
NEX_INDEX   <- paste0("https://nex-gddp-cmip6.s3.us-west-2.amazonaws.com/index_",
                      NEX_VERSION, "_md5.txt")

NEX_MODELS <- c("ACCESS-ESM1-5",
                "CNRM-ESM2-1",
                "EC-Earth3",
                "GFDL-ESM4",
                "GISS-E2-1-G",
                "MIROC6",
                "MPI-ESM1-2-HR",
                "MRI-ESM2-0")

NEX_VARIABLES <- c("hurs", "huss", "pr", "rlds", "rsds",
                   "sfcWind", "tas", "tasmax", "tasmin")

# Experiment code -> display label. Order is the display order.
SCENARIOS <- c(historical = "Historical Emissions",
               ssp126 = "Moderating Emissions (SSP1-2.6)",
               ssp245 = "Middle of the Road (SSP2-4.5)",
               ssp370 = "High Emissions (SSP3-7.0)",
               ssp585 = "Accelerating Emissions (SSP5-8.5)")
SSPS <- names(SCENARIOS)[-1]

# ---- Local working tree (mirrors s3://<bucket>/<prefix>/) ----
DIR_CACHE  <- file.path("_cache", "nex-gddp-cmip6", NEX_VERSION)
DIR_DATA   <- Sys.getenv("NR_DATA_DIR", unset = "data")
DIR_DAILY  <- file.path(DIR_DATA, "daily")
DIR_ANNUAL <- file.path(DIR_DATA, "annual")
DIR_SMOOTH <- file.path(DIR_DATA, "smoothed")
DIR_OUTPUT <- Sys.getenv("NR_OUTPUT_DIR", unset = "output")  # per-land xlsx/pdf/png
DIR_UPLOAD <- Sys.getenv("NR_UPLOAD_DIR", unset = "upload")  # published zips and pdf/

# Bump to force the matching stage to rebuild everywhere.
SMOOTH_SPEC_VERSION <- 1L
OUTPUT_SPEC_VERSION <- 1L

# ---- Workbook / README text ----
nr_metadata_text <- function(name, census_year) {
  c(
    paste0("CMIP6 ag-climate projections for ", name),
    paste0("This workbook contains place-based climate data for Native American, Alaska Native, and Native Hawaiian lands located in the United States. Climate data and projections for temperature, precipitation, and other metrics related to crop, livestock and forestry agriculture are shown in the accompanying graphs. The data derive from eight Coupled Model Intercomparison Project Phase 6 (CMIP6) global climate models and four socioeconomic scenarios for the period from 2015 to 2100, as well as the historical simulation for each model for the period 1950 to 2014. Raw data are area-weighted averages extracted for the location of the reservation from version ", sub("^v", "", NEX_VERSION), " of the NASA Earth Exchange (NEX) Global Daily Downscaled Projections (GDDP) dataset (NEX-GDDP-CMIP6). Further information on the NASA NEX downscaled product, including descriptions of the projected climate variables, can be found at https://www.nccs.nasa.gov/services/data-collections/land-based-products/nex-gddp-cmip6. NASA notes that downscaled values over smaller islands may not be realistic, because the observational data used for bias correction are not validated over oceans."),
    paste0("Spatial data on Native lands were derived from the ", census_year, " US Census TIGER/Line database, which includes all tribally controlled lands in the United States, as well as Alaska Native Village Statistical Areas (ANVSA) and State Designated Tribal Statistical Areas (SDTSA). Reservations and off-reservation trust lands are treated separately; projections are provided for reservations and statistical areas. We divided the Navajo Nation into its five agencies to better represent climate differences across the Nation. We used the recently defined climate divisions for the State of Hawai‘i (Luo et al 2024) to represent climate difference on the Hawaiian Islands."),
    "We aggregated daily data from each model and scenario into seasonal or annual statistics (the Annual Projections worksheet), and then we combined the results from the eight models using a generalized additive model for location, scale and shape (GAMLSS; the Smoothed Projections worksheet), which reports the median and the 5th, 25th, 75th and 95th percentiles of the fitted distribution in each year. We provide raw daily data for each scenario in the Raw worksheets. Growing Degree Days utilize a 50 ºF base temperature in their calculation, making them relevant to corn production.",
    "These data were produced by the Montana Climate Office and the Native Resilience project (formerly Native Climate) with funding from the National Institute of Food and Agriculture (NIFA), US Department of Agriculture.",
    "Please contact Kyle Bocinsky at kyle.bocinsky@umontana.edu with any questions. Code for producing all data supplied here is freely available on Github: https://github.com/native-resilience/projections."
  )
}

# ---- Parallel map via purrr::in_parallel() + mirai ----
# in_parallel() serialises `.f` without the caller's environment, so data and
# script-local functions the closure needs go through `...` (name = value).
# Pipeline helpers need not: every daemon sources R/setup.R first.
nr_map <- function(.x, .f, ..., .workers = nr_workers(), .progress = TRUE) {
  if (length(.x) == 0) return(list())
  if (.workers <= 1L) {
    environment(.f) <- list2env(list(...), parent = globalenv())
    return(purrr::map(.x, .f, .progress = .progress))
  }
  mirai::daemons(.workers)
  on.exit(mirai::daemons(0), add = TRUE)
  wd <- getwd()
  mirai::everywhere({
    setwd(wd)
    suppressMessages(source("R/setup.R"))
  }, wd = wd)
  # in_parallel() only accepts a function written inside its call, so wrap
  # `.f` and rebind its environment on the daemon to the named data.
  purrr::map(.x,
             purrr::in_parallel(\(x) {
               env <- list2env(fn_data, parent = globalenv())
               # Functions passed as data (script-local helpers) see each
               # other and the data too.
               for (n in names(fn_data))
                 if (is.function(env[[n]])) environment(env[[n]]) <- env
               environment(fn) <- env
               fn(x)
             }, fn = .f, fn_data = list(...)),
             .progress = .progress)
}
