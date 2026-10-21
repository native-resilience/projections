# outputs.R — per-land workbook, graphs, PDF compression and zip.

RAW_COLUMNS <- c(
  Model = "model",
  run = "run",
  year = "year",
  Date = "date",
  `Near-Surface Relative Humidity [%]` = "hurs",
  `Near-Surface Specific Humidity [kg/kg]` = "huss",
  `Precipitation [in]` = "pr",
  `Surface Downwelling Longwave Radiation [W/m^2]` = "rlds",
  `Surface Downwelling Shortwave Radiation [W/m^2]` = "rsds",
  `Daily-Mean Near-Surface Wind Speed [mph]` = "sfcWind",
  `Daily Average Near-Surface Air Temperature [degF]` = "tas",
  `Daily Maximum Near-Surface Air Temperature [degF]` = "tasmax",
  `Daily Minimum Near-Surface Air Temperature [degF]` = "tasmin"
)

nr_scenario_factor <- function(x, historical = FALSE) {
  lv <- if (historical) SCENARIOS else SCENARIOS[SSPS]
  factor(unname(SCENARIOS[x]) %|% x, levels = unname(lv), ordered = TRUE)
}

`%|%` <- function(x, y) ifelse(is.na(x), y, x)

round4 <- function(df) {
  dplyr::mutate(df, dplyr::across(dplyr::where(is.double), \(x) round(x, digits = 4)))
}

# ---- Workbook ----
nr_write_workbook <- function(outfile, name, census_year, prepared, annual,
                              smoothed, metrics = METRICS) {
  labels <- purrr::map_chr(metrics, "label")

  raw <-
    prepared %>%
    dplyr::select(dplyr::all_of(RAW_COLUMNS), scenario) %>%
    dplyr::arrange(Model, Date) %>%
    round4() %>%
    split(nr_scenario_factor(.$scenario)) %>%
    purrr::map(\(x) dplyr::select(x, -scenario)) %>%
    magrittr::set_names(paste0("Raw — ",
                               trimws(sub("\\(.*", "", names(.)))))

  annual_wide <-
    annual %>%
    dplyr::filter(metric_id %in% names(metrics)) %>%
    dplyr::mutate(Variable = factor(labels[metric_id], levels = labels)) %>%
    dplyr::select(Model = model, Scenario = scenario, Year, Variable, value) %>%
    dplyr::arrange(Variable) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = value) %>%
    dplyr::mutate(Scenario = nr_scenario_factor(Scenario)) %>%
    dplyr::arrange(Model, Scenario, Year) %>%
    round4()

  smoothed_out <-
    smoothed %>%
    dplyr::filter(metric_id %in% names(metrics)) %>%
    dplyr::transmute(Scenario = nr_scenario_factor(scenario),
                     Variable = factor(labels[metric_id], levels = labels),
                     Year,
                     Value = p50,
                     `5th Percentile` = p05,
                     `25th Percentile` = p25,
                     `75th Percentile` = p75,
                     `95th Percentile` = p95) %>%
    dplyr::arrange(Scenario, Variable, Year) %>%
    dplyr::mutate(Variable = as.character(Variable)) %>%
    round4()

  sheets <- c(list(`Annual Projections` = annual_wide,
                   `Smoothed Projections` = smoothed_out),
              raw)

  wb <- openxlsx::createWorkbook(
    creator = "Montana Climate Office",
    title = paste0("CMIP6 climate projections for ", name))

  openxlsx::addWorksheet(wb, sheetName = "Metadata", gridLines = FALSE)
  purrr::walk(names(sheets),
              \(s) openxlsx::addWorksheet(wb, sheetName = s, gridLines = FALSE))

  openxlsx::setColWidths(wb, sheet = "Metadata", cols = 1, widths = 80)
  meta <- nr_metadata_text(name, census_year)
  openxlsx::writeData(wb, sheet = "Metadata",
                      x = tibble::tibble(meta), colNames = FALSE)
  openxlsx::addStyle(wb, sheet = "Metadata",
                     style = openxlsx::createStyle(textDecoration = "bold"),
                     rows = 1, cols = 1)
  openxlsx::addStyle(wb, sheet = "Metadata",
                     style = openxlsx::createStyle(wrapText = TRUE),
                     rows = seq_along(meta)[-1], cols = 1, gridExpand = TRUE)

  wrap <- openxlsx::createStyle(wrapText = TRUE)
  purrr::iwalk(sheets, \(x, s) {
    openxlsx::writeDataTable(wb, sheet = s, x = x)
    n <- ncol(x)
    openxlsx::addStyle(wb, sheet = s, style = wrap, rows = 1, cols = seq_len(n),
                       gridExpand = TRUE)
  })

  openxlsx::setColWidths(wb, sheet = "Annual Projections", cols = 1:2,
                         widths = "auto")
  openxlsx::setColWidths(wb, sheet = "Annual Projections",
                         cols = 4:ncol(annual_wide), widths = 20)
  openxlsx::setColWidths(wb, sheet = "Smoothed Projections", cols = 1:3,
                         widths = "auto")
  purrr::walk(names(raw), \(s) {
    openxlsx::setColWidths(wb, sheet = s, cols = 1, widths = "auto")
    openxlsx::setColWidths(wb, sheet = s, cols = 3:ncol(raw[[s]]), widths = 20)
  })

  dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, file = outfile, overwrite = TRUE)
  outfile
}

# ---- Graphs ----
SCENARIO_COLORS <-
  c("Historical Emissions" = rgb(0, 0, 0, maxColorValue = 255),
    "transparent",
    " " = "transparent",
    "  " = "transparent",
    "Moderating Emissions (SSP1-2.6)" = rgb(34, 46, 77, maxColorValue = 255),
    "Middle of the Road (SSP2-4.5)" = rgb(223, 146, 71, maxColorValue = 255),
    "High Emissions (SSP3-7.0)" = rgb(187, 54, 51, maxColorValue = 255),
    "Accelerating Emissions (SSP5-8.5)" = rgb(122, 41, 40, maxColorValue = 255))

HEAT_INDEX_COLORS <-
  c("Caution" = rgb(255, 253, 113, maxColorValue = 255),
    "Extreme Caution" = rgb(254, 214, 48, maxColorValue = 255),
    "Danger" = rgb(253, 140, 37, maxColorValue = 255),
    "Extreme Danger" = rgb(252, 13, 27, maxColorValue = 255))

nr_logo_grobs <- function() {
  list(
    grid::rasterGrob(png::readPNG("native-resilience.png"),
                     y = -0.275, height = grid::unit(0.2, "npc"),
                     x = 0, vjust = 0, hjust = 0, interpolate = TRUE),
    grid::rasterGrob(png::readPNG("mco.png"),
                     y = -0.225, height = grid::unit(0.1, "npc"),
                     x = 0.16, vjust = 0, hjust = 0, interpolate = TRUE)
  )
}

# Web address printed on every graph (bottom right, under the legend).
NR_PLOT_CAPTION <- "native-resilience.com"

nr_theme <- function() {
  list(
    theme_minimal(14),
    labs(caption = NR_PLOT_CAPTION),
    theme(legend.title = element_blank(),
          plot.caption = element_text(colour = "grey35", size = 11,
                                      margin = margin(t = 6)),
          legend.justification = c(1, 1),
          legend.position = "bottom",
          legend.key.width = unit(0.25, "in"),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          plot.margin = unit(c(0.1, 0.2, 0.1, 0.1), "in")),
    scale_x_continuous(expand = c(0, 0), breaks = seq(1950, 2100, 10),
                       limits = c(1950, 2100))
  )
}

# Years before 2015 are the shared historical run: label them as such and, for
# the smooths, average the four scenario fits over that period.
nr_plot_data <- function(annual, smoothed, metrics) {
  labels <- purrr::map_chr(metrics, "label")
  cap365 <- c("count", "doy", "doy_late")
  types <- purrr::map_chr(metrics, "type")

  annual <-
    annual %>%
    dplyr::filter(metric_id %in% names(metrics)) %>%
    dplyr::mutate(scenario = ifelse(Year < 2015, "historical", scenario)) %>%
    dplyr::distinct(model, scenario, Year, metric_id, .keep_all = TRUE) %>%
    dplyr::mutate(Value = ifelse(types[metric_id] %in% cap365,
                                 pmin(value, 365), value))

  smoothed <-
    smoothed %>%
    dplyr::filter(metric_id %in% names(metrics)) %>%
    dplyr::mutate(scenario = ifelse(Year < 2015, "historical", scenario)) %>%
    dplyr::summarise(Value = mean(p50, na.rm = TRUE),
                     lower = suppressWarnings(min(p25, na.rm = TRUE)),
                     upper = suppressWarnings(max(p75, na.rm = TRUE)),
                     .by = c(scenario, metric_id, Year)) %>%
    dplyr::mutate(dplyr::across(c(Value, lower, upper),
                                \(x) ifelse(is.finite(x), x, NA_real_)))

  list(annual = dplyr::mutate(annual, Scenario = nr_scenario_factor(scenario, TRUE),
                              Variable = labels[metric_id]),
       smoothed = dplyr::mutate(smoothed, Scenario = nr_scenario_factor(scenario, TRUE),
                                Variable = labels[metric_id]))
}

doy_as_date <- function(x) {
  x <- round(pmin(x, 365))
  as.Date(x - 1, origin = "2023-01-01")
}

nr_plot_metric <- function(m, pd, name, logos) {
  ann <- dplyr::filter(pd$annual, metric_id == m$id)
  smo <- dplyr::filter(pd$smoothed, metric_id == m$id)
  if (nrow(smo) == 0 || all(is.na(smo$Value))) return(NULL)
  is_doy <- identical(m$plot, "doy")
  yfun <- if (is_doy) doy_as_date else identity

  p <-
    ggplot(mapping = aes(x = Year, color = Scenario, fill = Scenario)) +
    geom_line(data = ann,
              mapping = aes(y = yfun(Value), group = interaction(model, Scenario)),
              linewidth = 0.1, na.rm = TRUE) +
    geom_ribbon(data = smo,
                mapping = aes(ymin = yfun(lower), ymax = yfun(upper)),
                color = NA, alpha = 0.5, linewidth = 0.25, na.rm = TRUE) +
    geom_line(data = smo, mapping = aes(y = yfun(Value)), linewidth = 1.25,
              na.rm = TRUE) +
    scale_color_manual(values = SCENARIO_COLORS) +
    scale_fill_manual(values = SCENARIO_COLORS) +
    labs(x = NULL, y = m$label, title = name,
         subtitle = paste0("Climate Projections of ", m$label)) +
    nr_theme() +
    ggplot2::guides(colour = guide_legend(ncol = 2)) +
    annotation_custom(logos[[1]], xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_custom(logos[[2]], xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    coord_cartesian(clip = "off")

  if (is_doy) p <- p + scale_y_date()
  p
}

nr_plot_heat_index <- function(ms, pd, name) {
  levels <- purrr::map_chr(ms, "label")
  short <- c("Caution", "Extreme Caution", "Danger", "Extreme Danger")
  smo <- dplyr::filter(pd$smoothed, metric_id %in% purrr::map_chr(ms, "id"))
  if (nrow(smo) == 0) return(NULL)

  smo %>%
    dplyr::mutate(Variable = factor(Variable, levels = levels,
                                    labels = short[seq_along(levels)],
                                    ordered = TRUE)) %>%
    nr_expand_historical() %>%
    dplyr::mutate(Scenario = nr_scenario_factor(scenario)) %>%
    ggplot(mapping = aes(x = Year, y = Value, color = Variable, fill = Variable,
                         group = Variable)) +
    geom_area(na.rm = TRUE) +
    scale_color_manual(values = HEAT_INDEX_COLORS) +
    scale_fill_manual(values = HEAT_INDEX_COLORS) +
    facet_wrap("Scenario", ncol = 1) +
    scale_y_continuous(expand = c(0, NA), limits = c(0, NA)) +
    labs(x = NULL, y = "Number of Days at Each Heat Hazard Level", title = name,
         subtitle = paste0("Climate Projections of ", "Heat Index Hazard")) +
    nr_theme() +
    ggplot2::guides(colour = guide_legend(ncol = 4))
}

# All graphs for one land, in registry order; the heat-index levels share one
# stacked graph placed where the first of them sits in the registry.
nr_plot_land <- function(name, annual, smoothed, metrics = METRICS) {
  pd <- nr_plot_data(annual, smoothed, metrics)
  logos <- nr_logo_grobs()
  plotted <- purrr::keep(metrics, \(m) !is.na(m$plot))
  hi <- purrr::keep(plotted, \(m) m$plot == "heat_index")

  graphs <- list()
  for (m in plotted) {
    if (m$plot == "heat_index") {
      if (!"Heat Index" %in% names(graphs))
        graphs[["Heat Index"]] <- nr_plot_heat_index(hi, pd, name)
    } else {
      graphs[[m$label]] <- nr_plot_metric(m, pd, name, logos)
    }
  }
  purrr::compact(graphs)
}

nr_save_graphs <- function(graphs, pdf_file, png_dir) {
  dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)
  grDevices::cairo_pdf(pdf_file, height = 7.5, width = 10, onefile = TRUE)
  on.exit(grDevices::dev.off())
  purrr::walk(graphs, print)
  purrr::iwalk(graphs, \(x, n)
               ggplot2::ggsave(filename = file.path(png_dir, paste0(n, ".png")),
                               plot = x, device = ragg::agg_png,
                               height = 7.5, width = 10, bg = "white"))
  pdf_file
}

# ---- PDF compression ----
# Same steps as the old ~/git/compress.sh, but every file works in its own
# temp dir so parallel runs cannot overwrite each other's intermediates. Any
# step whose tool is missing is skipped with a message.
nr_compress_pdf <- function(file,
                            icc = Sys.getenv("NR_ICC_PROFILE",
                                             unset = "~/git/sRGB_v4_ICC_preference.icc")) {
  tmp <- tempfile("pdf-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  have <- \(tool) nzchar(Sys.which(tool))
  cur <- file

  if (have("mutool")) {
    out <- file.path(tmp, "clean.pdf")
    processx::run("mutool", c("clean", "-ggggz", "-Z",
                              "--color-image-recompress-method=jpeg",
                              "--color-image-subsample-dpi=150",
                              "-O", "compression-effort=100",
                              "-O", "compress=brotli", cur, out))
    cur <- out
  }
  if (have("gs")) {
    out <- file.path(tmp, "flattened.pdf")
    icc <- path.expand(icc)
    icc_args <- if (file.exists(icc))
      c(paste0("-sOutputICCProfile=", icc), paste0("--permit-file-read=", icc))
    processx::run("gs", c("-o", out, "-sDEVICE=pdfwrite",
                          "-dPDFA=2", "-dPDFACompatibilityPolicy=1",
                          "-dPDFSETTINGS=/prepress",
                          "-sColorConversionStrategy=RGB", icc_args,
                          "-dCompatibilityLevel=1.7", "-dNOPAUSE", "-dBATCH",
                          "-dNOOUTERSAVE", "-dQUIET", "-dFlattenAllForms=true",
                          "-dPrinted=false", cur))
    cur <- out
  }
  if (have("qpdf")) {
    out <- file.path(tmp, "optimized.pdf")
    processx::run("qpdf", c("--linearize", cur, out))
    cur <- out
  }
  if (have("pdfcpu")) {
    v <- processx::run("pdfcpu", c("validate", cur), error_on_status = FALSE)
    if (v$status != 0)
      warning("pdfcpu validate failed for ", file, ": ",
              trimws(paste(v$stdout, v$stderr)), call. = FALSE)
  }

  missing <- c("mutool", "gs", "qpdf", "pdfcpu")[!vapply(c("mutool", "gs", "qpdf", "pdfcpu"), have, logical(1))]
  if (length(missing))
    message("nr_compress_pdf: skipped ", paste(missing, collapse = ", "),
            " (not installed)")
  if (!identical(cur, file)) file.copy(cur, file, overwrite = TRUE)
  invisible(file)
}

nr_zip_land <- function(files, zipfile) {
  dir.create(dirname(zipfile), recursive = TRUE, showWarnings = FALSE)
  unlink(zipfile)
  zip::zip(zipfile = zipfile, files = files, include_directories = FALSE,
           mode = "cherry-pick")
  zipfile
}
