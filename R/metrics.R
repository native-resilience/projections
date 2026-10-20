# metrics.R — the metric registry and the daily → annual computation.
#
# Every annual metric is one metric() entry. To add one, append an entry and
# rerun projections.R: the ledger sees a (metric_id, version) it has not
# built and computes, smooths and publishes only that metric. Bump `version`
# when a metric's definition changes to rebuild it everywhere.
#
#   id        stable ASCII slug; the `variable=` partition value on S3
#   label     column/plot name in the workbook and PDFs (keep stable)
#   annual    expression evaluated per model × scenario × Year over the
#             prepared daily data (US units; see nr_prepare_daily())
#   needs     daily derivations the expression uses (see DERIVATIONS)
#   family    gamlss.dist family for the ensemble smooth; further entries are
#             fallbacks tried in order if a fit fails. Chosen by
#             projections-families.R (GAIC over a sample of lands).
#             Avoid families whose quantile function inverts the CDF
#             numerically (e.g. ZAIG): far too slow for 151 years × 5
#             percentiles.
#   type      "count" (days out of the year; smoothed with denominator
#             n_days), "doy" (day of year), "doy_late" (day of year,
#             smoothed on 366 - doy) or "value"
#   plot      "line", "doy", "heat_index" (stacked area), or NA (no plot)
#   bounds    c(lower, upper) the smoothed percentiles are clamped to
#             (default 1–366 for day-of-year metrics)

metric <- function(id, label, annual, needs = character(0), family,
                   type = "value", plot = "line", bounds = NULL, version = 1L) {
  stopifnot(grepl("^[a-z0-9_]+$", id),
            type %in% c("value", "count", "doy", "doy_late"))
  if (is.null(bounds) && type %in% c("doy", "doy_late")) bounds <- c(1, 366)
  list(id = id, label = label, annual = annual, needs = needs,
       family = family, type = type, plot = plot, bounds = bounds,
       version = as.integer(version))
}

trace_inches <-
  units::set_units(2, "mm") |>
  units::set_units("in") |>
  units::drop_units()

# NWS heat index hazard thresholds (27, 32, 41, 54 ºC) in ºF
heat_index_breaks <-
  c(27, 32, 41, 54) |>
  units::set_units("degC") |>
  units::set_units("degF") |>
  units::drop_units()

METRICS <- list(
  ## Temperature
  metric("temp_mean", "Average Temperature [degF]",
         rlang::expr(mean(tas, na.rm = TRUE)), family = "NO"),
  metric("gdd50", "Growing Degree Days [degF]",
         rlang::expr(sum(gdd50, na.rm = TRUE)), needs = "gdd50",
         family = "ZAGA"),
  metric("days_frost_free", "Number of Frost Free Days",
         rlang::expr(sum(tasmin > 32, na.rm = TRUE)),
         family = c("BB", "BEINF", "BI"), type = "count"),

  ## Precipitation
  metric("pr_annual", "Annual Precipitation [in]",
         rlang::expr(sum(pr, na.rm = TRUE)), family = c("GA", "ZAGA")),
  metric("pr_frozen", "Annual Frozen Precipitation [in]",
         rlang::expr(sum(frozen_pr, na.rm = TRUE)), needs = "frozen_pr",
         family = "ZAGA"),
  metric("pr_spring", "Spring Precipitation [in]",
         rlang::expr(sum(pr[month %in% 3:5], na.rm = TRUE)), family = c("GA", "ZAGA")),
  metric("pr_summer", "Summer Precipitation [in]",
         rlang::expr(sum(pr[month %in% 6:8], na.rm = TRUE)), family = c("GA", "ZAGA")),
  metric("pr_fall", "Fall Precipitation [in]",
         rlang::expr(sum(pr[month %in% 9:11], na.rm = TRUE)), family = c("GA", "ZAGA")),
  metric("pr_winter", "Winter Precipitation [in]",
         rlang::expr(sum(pr[month %in% c(12, 1, 2)], na.rm = TRUE)),
         family = c("GA", "ZAGA")),

  ## Extreme precipitation
  metric("pr_max_3day", "Maximum 3-Day Precipitation [in]",
         rlang::expr(max(pr_3day, na.rm = TRUE)), needs = "pr_3day",
         family = c("LOGNO", "ZAGA")),
  metric("pr_wet_day_mean", "Average Precipitation on Wet Days [in]",
         rlang::expr(mean(pr[pr > 0], na.rm = TRUE)), family = c("GA", "ZAGA")),
  metric("pr_wet_day_mean_trace", "Average Precipitation on Wet Days (trace) [in]",
         rlang::expr(mean(pr[pr > !!trace_inches], na.rm = TRUE)),
         family = c("GA", "ZAGA")),
  metric("days_wet", "Number of Wet Days",
         rlang::expr(sum(pr > 0, na.rm = TRUE)), family = c("BB", "BEINF", "BI"), type = "count"),
  metric("days_wet_trace", "Number of Wet Days (trace)",
         rlang::expr(sum(pr > !!trace_inches, na.rm = TRUE)),
         family = c("BB", "BEINF", "BI"), type = "count"),
  metric("days_dry", "Number of Dry Days",
         rlang::expr(sum(pr <= 0, na.rm = TRUE)), family = c("BB", "BEINF", "BI"), type = "count"),
  metric("days_dry_trace", "Number of Dry Days (trace)",
         rlang::expr(sum(pr <= !!trace_inches, na.rm = TRUE)),
         family = c("BB", "BEINF", "BI"), type = "count"),

  ## Extreme heat
  metric("days_tasmax_ge_100", "Number of Days >= 100 ºF",
         rlang::expr(sum(tasmax >= 100, na.rm = TRUE)),
         family = c("BB", "BEINF", "BI"), type = "count"),
  metric("days_hi_caution", 'Number of Days with Heat Index at "Caution" Level',
         rlang::expr(sum(heat_index >= !!heat_index_breaks[[1]] &
                           heat_index < !!heat_index_breaks[[2]], na.rm = TRUE)),
         needs = "heat_index", family = c("BB", "BEINF", "BI"), type = "count", plot = "heat_index"),
  metric("days_hi_extreme_caution", 'Number of Days with Heat Index at "Extreme caution" Level',
         rlang::expr(sum(heat_index >= !!heat_index_breaks[[2]] &
                           heat_index < !!heat_index_breaks[[3]], na.rm = TRUE)),
         needs = "heat_index", family = c("BB", "BEINF", "BI"), type = "count", plot = "heat_index"),
  metric("days_hi_danger", 'Number of Days with Heat Index at "Danger" Level',
         rlang::expr(sum(heat_index >= !!heat_index_breaks[[3]] &
                           heat_index < !!heat_index_breaks[[4]], na.rm = TRUE)),
         needs = "heat_index", family = c("BB", "BEINF", "BI"), type = "count", plot = "heat_index"),
  metric("days_hi_extreme_danger", 'Number of Days with Heat Index at "Extreme danger" Level',
         rlang::expr(sum(heat_index >= !!heat_index_breaks[[4]], na.rm = TRUE)),
         needs = "heat_index", family = c("BB", "BEINF", "BI"), type = "count", plot = "heat_index"),

  ## Wind
  metric("wind_mean", "Average Surface Wind Speed [mph]",
         rlang::expr(mean(sfcWind, na.rm = TRUE)), family = c("NO", "ZAGA")),

  ## Normal growing season (days with a 6-day run of normal temperature
  ## above / below 50 ºF, before / after the normal hottest day)
  metric("gs_first_day", "Normal First Day of Growing Season",
         rlang::expr(gs_first(yday, warm6, hottest_doy)),
         needs = c("warm6", "hottest_doy"), family = c("TF", "NO"), type = "doy",
         plot = "doy"),
  metric("gs_last_day", "Normal Last Day of Growing Season",
         rlang::expr(gs_last(yday, warm6, cool6, hottest_doy)),
         needs = c("warm6", "cool6", "hottest_doy"), family = c("TF", "NO"),
         type = "doy_late", plot = "doy"),
  metric("gs_length", "Normal Length of Growing Season [days]",
         rlang::expr(gs_length(yday, warm6, cool6, hottest_doy)),
         needs = c("warm6", "cool6", "hottest_doy"), family = c("TF", "NO"),
         bounds = c(0, 366)),

  ## Day of first snow after the normal hottest day of the year
  metric("first_snow_day", "Day of First Snow",
         rlang::expr(first_after(yday, frozen_pr > 0, hottest_doy)),
         needs = c("frozen_pr", "hottest_doy"), family = c("NO", "TF"),
         type = "doy_late", plot = "doy")
)
names(METRICS) <- purrr::map_chr(METRICS, "id")
stopifnot(!anyDuplicated(purrr::map_chr(METRICS, "label")))

metrics_table <- function(metrics = METRICS) {
  tibble::tibble(
    metric_id = purrr::map_chr(metrics, "id"),
    label = purrr::map_chr(metrics, "label"),
    metric_version = purrr::map_int(metrics, "version"),
    family = purrr::map_chr(metrics, \(m) paste(m$family, collapse = ",")),
    type = purrr::map_chr(metrics, "type"),
    plot = purrr::map_chr(metrics, \(m) as.character(m$plot)),
    order = seq_along(metrics)
  )
}

# ---- Growing-season helpers (one model × scenario × year at a time) ----
# No 6-day warm run before the hottest day means no growing season: the
# first/last days are NA and the length is 0.
gs_first <- function(yday, warm6, hottest) {
  hottest <- hottest[[1]]
  as.integer(dplyr::first(yday[yday <= hottest & warm6 %in% 1],
                          default = NA_integer_))
}

gs_last <- function(yday, warm6, cool6, hottest) {
  hottest <- hottest[[1]]
  if (is.na(gs_first(yday, warm6, hottest))) return(NA_integer_)
  # Never cools after the hottest day: the season runs to the end of the year.
  as.integer(dplyr::first(yday[yday >= hottest & cool6 %in% 1],
                          default = max(yday)))
}

gs_length <- function(yday, warm6, cool6, hottest) {
  first <- gs_first(yday, warm6, hottest)
  if (is.na(first)) return(0L)
  gs_last(yday, warm6, cool6, hottest) - first + 1L
}

first_after <- function(yday, cond, hottest) {
  hottest <- hottest[[1]]
  as.integer(dplyr::first(yday[yday > hottest & cond %in% TRUE],
                          default = NA_integer_))
}

# Right-aligned run test over the previous n days. The first n - 1 days use
# the days available, rather than NA, so the first year is not truncated.
roll_all <- function(x, n = 6) {
  out <- RcppRoll::roll_prod(as.numeric(x), n = n, align = "right", fill = NA)
  head_n <- seq_len(min(n - 1, length(x)))
  out[head_n] <- cumprod(as.numeric(x[head_n]))
  out
}

# ---- Daily derivations ----
# Each takes the prepared daily data (grouped by nothing) and returns it with
# its column(s) added. `needs` lists derivations that must run first.
DERIVATIONS <- list(
  gdd50 = list(needs = character(0), fn = \(d) {
    dplyr::mutate(d, gdd50 = pmax(tas - 50, 0))
  }),
  frozen_pr = list(needs = character(0), fn = \(d) {
    dplyr::mutate(d, frozen_pr = ifelse(tasmin <= 32, pr, 0))
  }),
  heat_index = list(needs = character(0), fn = \(d) {
    dplyr::mutate(d, heat_index = weathermetrics::heat.index(t = tasmax,
                                                             rh = hurs,
                                                             round = 1))
  }),
  pr_3day = list(needs = character(0), fn = \(d) {
    d %>%
      dplyr::mutate(pr_3day = RcppRoll::roll_sum(pr, n = 3, align = "right",
                                                 fill = NA),
                    .by = c(model, scenario))
  }),
  # Smoothed daily "normal" temperature per model × scenario: a cyclic
  # seasonal term plus a slow trend over years.
  normal_tas = list(needs = character(0), fn = \(d) {
    d %>%
      dplyr::mutate(
        normal_tas = mgcv::gam(
          tas ~ s(yday, k = -1, bs = "cc") + s(year, k = 4, bs = "cr"),
          data = tibble::tibble(tas = tas, yday = yday, year = year)
        ) |>
          mgcv::predict.gam() |>
          as.numeric(),
        .by = c(model, scenario))
  }),
  warm6 = list(needs = "normal_tas", fn = \(d) {
    dplyr::mutate(d, warm6 = roll_all(normal_tas > 50), .by = c(model, scenario))
  }),
  cool6 = list(needs = "normal_tas", fn = \(d) {
    dplyr::mutate(d, cool6 = roll_all(normal_tas < 50), .by = c(model, scenario))
  }),
  hottest_doy = list(needs = "normal_tas", fn = \(d) {
    dplyr::mutate(d,
                  hottest_doy = as.integer(stats::median(
                    yday[normal_tas == max(normal_tas, na.rm = TRUE)])),
                  .by = c(model, scenario, Year))
  })
)

derivation_order <- function(needs) {
  out <- character(0)
  visit <- function(n) {
    if (n %in% out) return()
    if (!n %in% names(DERIVATIONS)) stop("Unknown derivation: ", n)
    purrr::walk(DERIVATIONS[[n]]$needs, visit)
    out <<- c(out, n)
  }
  purrr::walk(unique(needs), visit)
  out
}

# ---- Daily data in, annual metrics out ----

# Convert NEX native units to the US units every metric is written in, clamp
# relative humidity, and give every SSP its own copy of the historical run so
# each scenario is a continuous 1950–2100 series.
nr_prepare_daily <- function(daily) {
  daily %>%
    dplyr::mutate(
      pr = units::set_units(pr * 86400, "mm") |>
        units::set_units("in") |>
        units::drop_units(),
      sfcWind = units::set_units(sfcWind, "m/s") |>
        units::set_units("mi/hr") |>
        units::drop_units(),
      dplyr::across(c(tas, tasmax, tasmin),
                    \(x) units::set_units(x, "kelvin") |>
                      units::set_units("degF") |>
                      units::drop_units()),
      hurs = pmin(pmax(hurs, 0), 100)
    ) %>%
    nr_expand_historical() %>%
    dplyr::mutate(year = as.integer(lubridate::year(date)),
                  Year = year,
                  month = lubridate::month(date),
                  yday = lubridate::yday(date)) %>%
    dplyr::arrange(model, scenario, date)
}

nr_expand_historical <- function(x, col = "scenario") {
  hist <- x[x[[col]] == "historical", , drop = FALSE]
  futr <- x[x[[col]] != "historical", , drop = FALSE]
  hist <- tidyr::crossing(dplyr::select(hist, -dplyr::all_of(col)),
                          !!col := SSPS)
  dplyr::bind_rows(hist, futr)
}

nr_compute_annual <- function(prepared, metrics = METRICS) {
  if (length(metrics) == 0) return(NULL)
  needs <- unique(unlist(purrr::map(metrics, "needs")))
  for (n in derivation_order(needs))
    prepared <- DERIVATIONS[[n]]$fn(prepared)

  exprs <- purrr::map(metrics, "annual")
  prepared %>%
    dplyr::summarise(n_days = dplyr::n(),
                     !!!exprs,
                     .by = c(model, scenario, Year)) %>%
    tidyr::pivot_longer(dplyr::all_of(names(metrics)),
                        names_to = "metric_id", values_to = "value",
                        values_transform = as.numeric) %>%
    dplyr::mutate(value = ifelse(is.finite(value), value, NA_real_),
                  metric_version = purrr::map_int(metrics, "version")[metric_id]) %>%
    dplyr::relocate(metric_id, metric_version, .after = n_days) %>%
    dplyr::arrange(metric_id, model, scenario, Year)
}
