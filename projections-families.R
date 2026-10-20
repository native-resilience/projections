# projections-families.R — choose each metric's gamlss.dist family.
#
# Fits every candidate family to a sample of lands (two scenarios each) and
# scores it by GAIC, rejecting degenerate fits whose median strays from the
# yearly ensemble median by more than one standard deviation of the data.
# The winner per metric is the family with the best mean GAIC rank among
# those that are degenerate in < 5% of series. Results go to
# _state/family-selection.csv; copy the chosen families into R/metrics.R
# (and bump SMOOTH_SPEC_VERSION if the formulas changed, or each changed
# metric's family change is picked up by the ledger automatically).
#
#   NR_LANDS=0010R,0305R,… Rscript projections-families.R

source("R/setup.R")

CANDIDATES <- list(
  value = list(
    temp_mean = c("NO", "TF", "LO"),
    gdd50 = c("ZAGA", "ZAIG", "NO"),
    pr_max_3day = c("WEI", "GA", "LOGNO", "ZAGA"),
    pr_wet_day_mean = c("ZAGA", "GA", "LOGNO"),
    pr_wet_day_mean_trace = c("ZAGA", "GA", "LOGNO"),
    wind_mean = c("ZAGA", "GA", "LOGNO", "NO"),
    .default = c("ZAGA", "ZAIG", "GA")           # precipitation totals
  ),
  doy = c("NBI", "NO", "TF", "LO"),
  doy_late = c("NBI", "NO", "TF", "LO"),
  count = c("BB")                                # nests BI; kept fixed
)

candidates_for <- function(m) {
  c_ <- CANDIDATES[[m$type]]
  if (is.list(c_)) c_[[m$id]] %||% c_$.default else c_
}

scenarios <- nr_env_list("NR_FAMILY_SCENARIOS")
if (!length(scenarios)) scenarios <- c("ssp245", "ssp585")

lands <- list.dirs(DIR_ANNUAL, recursive = FALSE, full.names = FALSE)
lands <- sub("^land_id=", "", lands)
if (length(nr_env_list("NR_LANDS")))
  lands <- intersect(lands, nr_env_list("NR_LANDS"))
message("family selection over ", length(lands), " lands × ",
        length(scenarios), " scenarios")

score_land <- function(id, scenarios) {
  annual <- arrow::read_parquet(file.path(DIR_ANNUAL, paste0("land_id=", id),
                                          "part-0.parquet"))
  purrr::map_dfr(METRICS, \(m) {
    fams <- candidates_for(m)
    if (length(fams) < 2) return(NULL)
    purrr::map_dfr(scenarios, \(scn) {
      d <- dplyr::filter(annual, metric_id == m$id, scenario == scn, !is.na(value))
      if (nrow(d) < 50 || length(unique(d$value)) < 5) return(NULL)
      target <- dplyr::summarise(d, med = stats::median(value), .by = Year)
      scale <- stats::sd(d$value)
      purrr::map_dfr(fams, \(fam) {
        fit <- tryCatch(nr_smooth_fit(d, fam, m$type), error = function(e) NULL)
        gaic <- if (is.null(fit)) NA_real_ else
          tryCatch(stats::AIC(fit), error = function(e) NA_real_)
        q <- if (is.null(fit)) NULL else
          tryCatch(nr_smooth_quantiles(fit, fam, m$type, target$Year),
                   error = function(e) NULL)
        mae <- if (is.null(q)) NA_real_ else
          mean(abs(q$p50 - target$med)) / scale
        tibble::tibble(land_id = id, scenario = scn, metric_id = m$id,
                       family = fam, gaic = gaic, median_error = mae)
      })
    })
  })
}

scores <-
  nr_map(lands, \(id) score_land(id, scenarios), scenarios = scenarios,
         score_land = score_land, candidates_for = candidates_for,
         CANDIDATES = CANDIDATES) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(degenerate = is.na(gaic) | is.na(median_error) | median_error > 1)

summary <-
  scores %>%
  dplyr::mutate(rank = rank(ifelse(degenerate, Inf, gaic), ties.method = "min"),
                .by = c(land_id, scenario, metric_id)) %>%
  dplyr::summarise(series = dplyr::n(),
                   degenerate_pct = 100 * mean(degenerate),
                   mean_rank = mean(rank),
                   median_error = stats::median(median_error, na.rm = TRUE),
                   .by = c(metric_id, family)) %>%
  dplyr::mutate(current = family == purrr::map_chr(METRICS, \(m) m$family[[1]])[metric_id],
                eligible = degenerate_pct < 5) %>%
  dplyr::arrange(metric_id, !eligible, mean_rank) %>%
  dplyr::mutate(chosen = dplyr::row_number() == 1 & eligible, .by = metric_id)

dir.create(DIR_STATE, showWarnings = FALSE, recursive = TRUE)
readr::write_csv(scores, file.path(DIR_STATE, "family-scores.csv"))
readr::write_csv(summary, file.path(DIR_STATE, "family-selection.csv"))
print(as.data.frame(summary), digits = 3)
