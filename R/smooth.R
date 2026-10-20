# smooth.R — ensemble smoothing with gamlss2.
#
# For each scenario × metric, the annual values of all eight models are pooled
# and fit with a GAMLSS whose parameters vary smoothly with Year. Reported
# percentiles are quantiles of the fitted distribution in each year, so the
# bands carry both inter-model and inter-annual spread.

SMOOTH_PROBS <- c(p05 = 0.05, p25 = 0.25, p50 = 0.5, p75 = 0.75, p95 = 0.95)

# Families whose response is a count out of n (fit with cbind(y, n - y)).
BINOMIAL_FAMILIES <- c("BI", "BB", "ZIBI", "ZIBB", "ZABI", "ZABB")

# Seconds allowed for one family's fit and quantiles before moving on.
SMOOTH_TIME_LIMIT <- 120

with_time_limit <- function(expr, seconds) {
  setTimeLimit(elapsed = seconds, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  expr
}

# Basis size per distribution parameter (mu, sigma, nu, tau).
SMOOTH_K <- c(5, 4, 3, 3)

nr_family_npar <- function(family) {
  get(family, envir = asNamespace("gamlss.dist"))()$nopar
}

nr_smooth_formula <- function(family, binomial) {
  lhs <- if (binomial) "cbind(value, n_days - value)" else "value"
  rhs <- paste0("s(Year, k = ", SMOOTH_K[seq_len(nr_family_npar(family))], ")",
                collapse = " | ")
  stats::as.formula(paste(lhs, "~", rhs))
}

# Response on the fitting scale. Count metrics fit with a non-binomial family
# (e.g. BEINF) are modelled as the proportion of the year.
nr_smooth_response <- function(data, type, family) {
  # Calendars without leap days still carry day 366 in leap years, so a
  # whole-year season can count one more day than the year has rows.
  if (type == "count")
    data$value <- pmin(data$value, data$n_days)
  if (type == "count" && !family %in% BINOMIAL_FAMILIES)
    data$value <- data$value / data$n_days
  if (type == "doy_late")
    data$value <- 366 - data$value
  data
}

nr_smooth_fit <- function(data, family, type) {
  binomial <- type == "count" && family %in% BINOMIAL_FAMILIES
  data <- nr_smooth_response(data, type, family)
  suppressWarnings(
    gamlss2::gamlss2(nr_smooth_formula(family, binomial),
                     data = data,
                     family = get(family, envir = asNamespace("gamlss.dist")),
                     control = gamlss2::gamlss2_control(trace = FALSE))
  )
}

# Quantiles of a fitted model at `years`, back on the metric's own scale.
# Counts are reported per 365-day year.
nr_smooth_quantiles <- function(fit, family, type, years, probs = SMOOTH_PROBS) {
  nd <- tibble::tibble(Year = years, n_days = 365L)
  par <- suppressWarnings(stats::predict(fit, newdata = nd, type = "parameter"))
  qfun <- get(paste0("q", family), envir = asNamespace("gamlss.dist"))
  extra <- if (family %in% BINOMIAL_FAMILIES) list(bd = nd$n_days) else list()
  out <- purrr::map(probs, \(p) do.call(qfun, c(list(p = p), as.list(par), extra)))
  out <- tibble::as_tibble(out)
  if (type == "count" && !family %in% BINOMIAL_FAMILIES)
    out <- out * 365
  if (type == "doy_late") {
    # 366 - x reverses the order, so the lower tail becomes the upper tail.
    out <- 366 - out
    out <- stats::setNames(rev(out), names(out))
  }
  dplyr::bind_cols(tibble::tibble(Year = years), out)
}

# Values on the reported scale: counts per 365-day year.
nr_per_year <- function(data, type) {
  if (type == "count") pmin(data$value, data$n_days) / data$n_days * 365 else data$value
}

nr_smooth_empirical <- function(data, type, years, probs = SMOOTH_PROBS,
                                halfwidth = 10) {
  v <- nr_per_year(data, type)
  out <- purrr::map(years, \(y) {
    w <- v[abs(data$Year - y) <= halfwidth]
    stats::quantile(w, probs, names = FALSE, na.rm = TRUE)
  })
  out <- do.call(rbind, out)
  colnames(out) <- names(probs)
  dplyr::bind_cols(tibble::tibble(Year = years), tibble::as_tibble(out))
}

# A converged fit can still be nonsense (a U-shaped beta, a t with almost
# no degrees of freedom). Accept it only if, in every year, its median lies
# within the ensemble's own 5–95% range over ±10 years and its 5–95% band is
# no more than 3× as wide as that range (tolerance: 5% of the series' SD).
nr_smooth_plausible <- function(q, data, type, halfwidth = 10) {
  v <- as.matrix(q[-1])
  if (!all(is.finite(v))) return(FALSE)
  emp <- nr_smooth_empirical(data, type, q$Year, halfwidth = halfwidth)
  tol <- 0.05 * stats::sd(nr_per_year(data, type))
  emp_width <- emp$p95 - emp$p05
  all(q$p50 >= emp$p05 - tol & q$p50 <= emp$p95 + tol) &&
    all(q$p95 - q$p05 <= 3 * emp_width + tol)
}

# Every percentile equal to `value` (constant series, or NA for no fit).
smooth_const <- function(years, value) {
  tibble::as_tibble(c(list(Year = years),
                      as.list(stats::setNames(rep(value, length(SMOOTH_PROBS)),
                                              names(SMOOTH_PROBS)))))
}

# Smooth one land's annual table (long: model, scenario, Year, n_days,
# metric_id, value) for the given metrics. Returns one row per scenario ×
# metric × Year, with fit status: ok, fallback (a later family), constant,
# empirical (no family could fit) or no_data.
nr_smooth_land <- function(annual, metrics = METRICS, land_id = NA_character_) {
  years <- sort(unique(annual$Year))
  annual %>%
    dplyr::filter(metric_id %in% names(metrics)) %>%
    dplyr::summarise(data = list(dplyr::pick(Year, n_days, value)),
                     .by = c(scenario, metric_id)) %>%
    dplyr::mutate(
      smooth = purrr::pmap(list(data, metric_id, scenario), \(data, id, scn) {
        m <- metrics[[id]]
        data <- dplyr::filter(data, !is.na(value))
        status <- "ok"
        q <- NULL
        used <- m$family[[1]]
        if (nrow(data) == 0) {
          status <- "no_data"
        } else if (length(unique(nr_per_year(data, m$type))) == 1) {
          # A constant series (no 100 ºF days ever; a growing season that is
          # always the whole year) needs no fit.
          status <- "constant"
          q <- smooth_const(years, nr_per_year(data, m$type)[[1]])
        } else {
          # Try the metric's families in order; later ones are fallbacks
          # (e.g. ZAGA when a gamma-type family meets a zero).
          status <- "failed"
          for (fam in m$family) {
            # A pathological series must not stall a worker: cap each
            # family's fit + quantiles and fall through to the next.
            q <- tryCatch(
              with_time_limit(
                nr_smooth_quantiles(nr_smooth_fit(data, fam, m$type),
                                    fam, m$type, years),
                SMOOTH_TIME_LIMIT),
              error = function(e) {
                message("gamlss2 fit failed: ", land_id, " / ", scn, " / ", id,
                        " (", fam, "): ", conditionMessage(e))
                NULL
              })
            if (!is.null(q) && !nr_smooth_plausible(q, data, m$type)) {
              message("gamlss2 fit implausible: ", land_id, " / ", scn, " / ",
                      id, " (", fam, ")")
              q <- NULL
            }
            if (!is.null(q)) {
              status <- if (fam == m$family[[1]]) "ok" else "fallback"
              used <- fam
              break
            }
            q <- NULL
          }
          # Too sparse for any family (e.g. a handful of extreme-danger heat
          # days across all models): empirical percentiles of every model's
          # values within ±10 years.
          if (is.null(q)) {
            q <- nr_smooth_empirical(data, m$type, years)
            status <- "empirical"
            used <- "empirical"
          }
        }
        if (is.null(q))
          q <- smooth_const(years, NA_real_)
        if (!is.null(m$bounds))
          q <- dplyr::mutate(q, dplyr::across(-Year, \(x) pmin(pmax(x, m$bounds[[1]]),
                                                                m$bounds[[2]])))
        dplyr::mutate(q, family = used, status = status)
      })
    ) %>%
    dplyr::select(scenario, metric_id, smooth) %>%
    tidyr::unnest(smooth) %>%
    dplyr::mutate(metric_version = purrr::map_int(metrics, "version")[metric_id],
                  smooth_version = SMOOTH_SPEC_VERSION,
                  .after = metric_id) %>%
    dplyr::arrange(metric_id, scenario, Year)
}

# Generalised AIC of each candidate family for one series, for family
# selection (see projections-families.R).
nr_family_gaic <- function(data, type, candidates) {
  data <- dplyr::filter(data, !is.na(value))
  purrr::map_dfr(candidates, \(fam) {
    fit <- tryCatch(nr_smooth_fit(data, fam, type), error = function(e) NULL)
    gaic <- if (is.null(fit)) NA_real_ else
      tryCatch(stats::AIC(fit), error = function(e) NA_real_)
    tibble::tibble(family = fam, gaic = gaic)
  })
}
