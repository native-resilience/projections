# install.packages("pak")

packages <- c("magrittr",
              "terra",
              "tidyverse",
              "multidplyr",
              "openxlsx",
              "png",
              "RcppRoll",
              "mgcv",
              "tigris",
              "weathermetrics",
              "gamlss",
              "ragg",
              "furrr",
              "future.callr",
              "parallelly"
)

pak::pak(packages, ask = FALSE)
# install.packages("arrow", type = "source")
# purrr::walk(packages, devtools::install_cran)
purrr::walk(packages,
            library,
            character.only = TRUE)
library(arrow)

options(future.rng.onMisuse="ignore")
terra::gdalCache(32000)

openxlsx_setOp("dateFormat", "yyyy-mm-dd")
openxlsx_setOp("tableStyle", "TableStyleLight1")
openxlsx_setOp("numFmt", "NUMBER")

if(!file.exists("native-land.parquet")){
  unlink("native-land.parquet")
  
  tribal_land <- 
    tigris::native_areas() %>%
    dplyr::arrange(NAME) %>%
    dplyr::filter(LSAD != "OT",
                  COMPTYP != "T") %>%
    dplyr::group_by(AIANNHCE, NAMELSAD) %>%
    dplyr::summarise() %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326) 
  
  dine_agencies <-
    tigris::tribal_subdivisions_national() %>%
    dplyr::filter(AIANNHCE == 2430) %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326) %>%
    dplyr::select(Chapter = NAME) %>%
    dplyr::mutate(Chapter = 
                    dplyr::case_when(
                      Chapter == "San Juan Southern Paiute Northern" ~ "Navajo Mountain",
                      Chapter == "San Juan Southern Paiute Southern" ~ "Tuba City",
                      .default = Chapter
                    )) %>%
    dplyr::arrange(Chapter) %>%
    dplyr::left_join(
      arcgislayers::arc_read("https://services4.arcgis.com/ul3DV3LWHhAM8P61/ArcGIS/rest/services/Navajo_Nation_2013/FeatureServer/0") %>%
        sf::st_drop_geometry() %>%
        dplyr::select(Chapter, Agency) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_to_title)) %>%
        tibble::as_tibble() %>%
        dplyr::arrange(Chapter) %>%
        dplyr::mutate(Chapter = 
                        dplyr::case_when(
                          Chapter == "Pinon" ~ "Piñon",
                          Chapter == "Canoncito" ~ "Cañoncito",
                          Chapter == "Baca/Prewitt" ~ "Baca",
                          Chapter == "Beclahbito" ~ "Beclabito",
                          Chapter == "Bodaway Gap" ~ "Bodaway",
                          Chapter == "Baahaalii" ~ "Bread Springs",
                          Chapter == "Tohajilee" ~ "Cañoncito",
                          Chapter == "Chichiltah" ~ "Chi Chil Tah",
                          Chapter == "Churchrock" ~ "Church Rock",
                          Chapter == "Dilkon" ~ "Dilcon",
                          Chapter == "Upper Fruitland" ~ "Fruitland",
                          Chapter == "Gadiahi" ~ "Gadii'ahi",
                          Chapter == "Greasewood Springs" ~ "Greasewood",
                          Chapter == "Lechee" ~ "LeChee",
                          Chapter == "Nahat'a'dzil" ~ "Nahatadziil",
                          Chapter == "Nenahnezad" ~ "Nenahnezad/San Juan",
                          # Chapter == "" ~ "San Juan Southern Paiute Northern",
                          # Chapter == "" ~ "San Juan Southern Paiute Southern",
                          Chapter == "Saint Michaels" ~ "St. Michaels",
                          Chapter == "Sweet Water" ~ "Sweetwater",
                          Chapter == "Tachee/Bluegap" ~ "Tachee",
                          Chapter == "Tsaile/Wheatfields" ~ "Tsaile-Wheatfields",
                          Chapter == "Whitehorse Lake" ~ "White Horse Lake",
                          Chapter == "Whiterock" ~ "White Rock",
                          # Chapter == "" ~ "Winslow Tract",
                          .default = Chapter
                        ))
    ) %>%
    dplyr::filter(!is.na(Agency)) %>%
    dplyr::group_by(Agency) %>%
    dplyr::summarise() %>%
    dplyr::transmute(NAMELSAD = paste0("Navajo Nation — ", Agency, " Agency"))
  
  hawaii_climate_divisions <- 
    sf::read_sf("HI_ClimateDivisions/ClimDiv12_polygon_clip.shp") %>%
    dplyr::transmute(`Division number` = as.integer(Id)) %>%
    dplyr::left_join(
      readr::read_csv("HI_ClimateDivisions.csv")
    ) %>%
    dplyr::transmute(NAMELSAD = paste0("Hawai‘i Climate Divisions — ", `Division name`)) %>%
    sf::st_transform(4326)
  
  tribal_land %<>%
    dplyr::filter(AIANNHCE != 2430) %>%
    dplyr::bind_rows(dine_agencies) %>%
    dplyr::bind_rows(hawaii_climate_divisions) %>%
    dplyr::select(`Native Land` = NAMELSAD) %>%
    dplyr::group_by(`Native Land`) %>%
    dplyr::summarise() %>%
    dplyr::arrange(`Native Land`) %>%
    sf::st_cast("MULTIPOLYGON") %>%
    dplyr::mutate(`Native Land`  = stringr::str_replace_all(`Native Land`, "/", " and "),
                  `Native Land` = factor(`Native Land`, ordered = TRUE)) %T>%
    sf::write_sf("native-land.parquet",
                 layer_options = c("COMPRESSION=BROTLI",
                                   "GEOMETRY_ENCODING=GEOARROW",
                                   "WRITE_COVERING_BBOX=NO"),
                 driver = "Parquet")
}

tribal_land <- 
  sf::read_sf("native-land.parquet") %>%
  dplyr::mutate(`Native Land` = factor(`Native Land`, ordered = TRUE)) %>%
  cmip6:::st_rotate()

# cmip6_dir <-
#   "/Volumes/SSD8x2/cmip6/"
# 
# cmip6_files <-
#   tibble::tibble(file = list.files(cmip6_dir, full.names = TRUE, pattern = ".nc")) %>%
#   dplyr::mutate(dataset = tools::file_path_sans_ext(basename(file))) %>%
#   tidyr::separate_wider_delim(dataset,
#                               names = c("element", "timestep", "model", "scenario", "run", "type", "year", "version"),
#                               delim = "_",
#                               cols_remove = FALSE,
#                               too_few = "align_start") %>%
#   dplyr::select(model, scenario, run, year, element, file) %>%
#   dplyr::filter(model %in%
#                   c("ACCESS-ESM1-5",
#                     "CNRM-ESM2-1",
#                     "EC-Earth3",
#                     "GFDL-ESM4",
#                     "GISS-E2-1-G",
#                     "MIROC6",
#                     "MPI-ESM1-2-HR",
#                     "MRI-ESM2-0")) %>%
#   dplyr::arrange(model, scenario, run, year, element, file) %>%
#   dplyr::summarise(rasts = list(c(file)),
#                    .by = c(model, scenario, run, year)) %>%
#   dplyr::mutate(dplyr::across(!rasts, \(x) factor(x, ordered = TRUE)))
# 
# dir.create(
#   file.path("data-derived",
#             "cmip6"),
#   recursive = TRUE,
#   showWarnings = FALSE
# )
# 
# extract_year <-
#   function(x, model, scenario, run, year){
#     out_file <-
#       file.path("data-derived",
#                 "cmip6",
#                 paste(model,
#                       scenario,
#                       run,
#                       year, sep = "_")) %>%
#       paste0(".parquet")
#     
#     if(!file.exists(out_file)){
#       x %>%
#         terra::rast() %>%
#         # terra::set.names(., terra::time(.)) %>%
#         exactextractr::exact_extract(.,
#                                      tribal_land,
#                                      fun = "mean",
#                                      colname_fun = function(values, weights, fun_name, fun_value, nvalues, nweights){values},
#                                      append_cols = "Native Land",
#                                      max_cells_in_memory = 689314395,
#                                      progress = FALSE) %>%
#         tidyr::pivot_longer(-`Native Land`, names_to = "element") %>%
#         tidyr::separate_wider_delim(element, names = c("element","doy"), delim = "_") %>%
#         dplyr::mutate(doy = as.integer(doy)) %>%
#         dplyr::group_by(element, doy) %>%
#         tidyr::nest() %>%
#         dplyr::ungroup() %>%
#         dplyr::arrange(element, doy) %>%
#         dplyr::mutate(date =
#                         x %>%
#                         terra::rast() %>%
#                         terra::time()) %>%
#         tidyr::unnest(data) %>%
#         dplyr::select(!c(doy)) %>%
#         tidyr::pivot_wider(names_from = element,
#                            values_from = value) %>%
#         dplyr::arrange(`Native Land`, date) %>%
#         arrow::write_parquet(sink = out_file,
#                              version = "latest",
#                              compression = "brotli")
#     }
#     return(out_file)
#   }
# 
# cl <- multidplyr::new_cluster(8)
# multidplyr::cluster_copy(cl, c("tribal_land", "extract_year"))
# multidplyr::cluster_library(cl, "magrittr")
# 
# tribal_land_data <-
#   cmip6_files %>%
#   # magrittr::extract(1:8,) %>%
#   dplyr::rowwise() %>%
#   multidplyr::partition(cl) %>%
#   dplyr::mutate(
#     extractions = extract_year(x = rasts,
#                                model = model,
#                                scenario = scenario,
#                                run = run,
#                                year = year)
#   ) %>%
#   dplyr::collect()
# 
# rm(cl)
# gc();gc()

# arrow::read_parquet("data-derived/cmip6/ACCESS-ESM1-5_historical_r1i1p1f1_1950.parquet") %>%
#  dplyr::filter(stringr::str_starts(`Native Land`, "Navajo")) %>%
#    ggplot(aes(x = date,
#              y = tasmax,
#              color = `Native Land`)) +
#   geom_line() + 
#   guides(color = "none")


# cmip6_reservations <-
#   list.files("data-derived/cmip6", full.names = TRUE) |>
#   tibble::tibble(file = _) |>
#   dplyr::mutate(dataset = tools::file_path_sans_ext(basename(file))) |>
#   tidyr::separate_wider_delim(dataset,
#                               names = c("model", "scenario", "run", "year"),
#                               delim = "_",
#                               cols_remove = TRUE,
#                               too_few = "align_start") |>
#   dplyr::select(model, scenario, run, year, file) |>
#   dplyr::mutate(model = factor(model),
#                 scenario = scenario |>
#                   forcats::as_factor() |>
#                   forcats::fct_recode("Moderating Emissions (SSP1-2.6)" = "ssp126",
#                                       "Middle of the Road (SSP2-4.5)" = "ssp245",
#                                       "High Emissions (SSP3-7.0)" = "ssp370",
#                                       "Accelerating Emissions (SSP5-8.5)" = "ssp585",
#                                       "Historical Emissions" = "historical"),
#                 run = factor(run),
#                 year = as.integer(year)
#   ) |>
#   dplyr::rowwise() |>
#   dplyr::mutate(file = arrow::read_parquet(file) |>
#                   list()) |>
#   dplyr::ungroup() |>
#   tidyr::unnest(file)
# 
# gc();gc()
# 
# cmip6_reservations |>
#   group_by(`Native Land`) |>
#   write_dataset(path = "data-derived/cmip6-reservations",
#                 format = "feather")
# 
# rm(cmip6_reservations)
# 
# gc();gc();



ndays_family <- "BEINF"
yday_family <- "NBI"
precip_family <- "ZAGA"

variable_dists <- 
  tibble::tribble(
    ~ Variable, ~ Family,
    
    ## Temperature
    "Average Temperature [degF]", "NO",
    "Growing Degree Days [degF]", "ZAGA",
    "Number of Frost Free Days", ndays_family, 
    
    ## Precipitation
    "Annual Precipitation [in]", precip_family,
    "Annual Frozen Precipitation [in]", precip_family,
    "Spring Precipitation [in]", precip_family,
    "Summer Precipitation [in]", precip_family,
    "Fall Precipitation [in]", precip_family,
    "Winter Precipitation [in]", precip_family,
    
    ## Extreme Precipitation
    "Maximum 3-Day Precipitation [in]", "WEI",
    "Average Precipitation on Wet Days [in]", precip_family,
    "Average Precipitation on Wet Days (trace) [in]", precip_family,
    "Number of Wet Days", ndays_family,
    "Number of Wet Days (trace)", ndays_family,
    "Number of Dry Days", ndays_family,
    "Number of Dry Days (trace)", ndays_family,
    
    ## Extreme Heat
    "Number of Days >= 100 ºF", ndays_family,
    'Number of Days with Heat Index at "Caution" Level', ndays_family,
    'Number of Days with Heat Index at "Extreme caution" Level', ndays_family,
    'Number of Days with Heat Index at "Danger" Level', ndays_family,
    'Number of Days with Heat Index at "Extreme danger" Level', ndays_family,
    
    ## Wind
    "Average Surface Wind Speed [mph]", "ZAGA",
    
    ## Normal Growing Season
    # "Normal Average Daily Temperature [degF]", "SHASHo2",
    # "Normal Maximum Daily Temperature [degF]", "SHASHo2",
    # "Normal Minimum Daily Temperature [degF]", "SHASHo2",
    # "Normal Hottest Day of the Year", yday_family,
    "Normal First Day of Growing Season", yday_family,
    "Normal Last Day of Growing Season", yday_family,
    # "Length of Growing Season [days]", "WEI3",
    "Normal Length of Growing Season [days]", ndays_family,
    
    # Day of first snow after normal hottest day of the year
    "Day of First Snow", yday_family
  )

process_reservation <- 
  function(x = "Acoma Pueblo", force = FALSE){
    
    dir.create(
      file.path("output", x),
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    outfile <- file.path("output", x, paste0(x, "_CMIP6-climate-projections.xlsx"))
    
    if(file.exists(outfile) & !force)
      return(outfile)
    
    cmip6_reservation <-
      arrow::open_dataset("data-derived/cmip6-reservations",
                          format = "feather") |>
      dplyr::filter(`Native Land` == x) |>
      dplyr::select(!c(`Native Land`)) |>
      dplyr::collect() |>
      dplyr::mutate(
        # hurs = units::set_units(hurs,"percent"),
        #             huss = units::set_units(huss,"kg/kg"),
        pr = units::set_units(pr * 86400, "mm") |>
          units::set_units("in") |>
          units::drop_units(),
        # rlds = units::set_units(rlds, "W m-2"),
        # rsds = units::set_units(rsds, "W m-2"),
        sfcWind = units::set_units(sfcWind,"m/s") |>
          units::set_units("mi/hr") |>
          units::drop_units(),
        tas = units::set_units(tas, "kelvin") |>
          units::set_units("degF") |>
          units::drop_units(),
        tasmax = units::set_units(tasmax, "kelvin") |>
          units::set_units("degF") |>
          units::drop_units(),
        tasmin = units::set_units(tasmin, "kelvin") |>
          units::set_units("degF") |>
          units::drop_units())
    
    gam_predict <- function(x){
      mgcv::gam(
        data = x,
        formula = 
          tas ~ s(yday, k = -1, bs = "cc") + s(year, k = 4, bs = "cr"),
        # correlation = nlme::corARMA(form = ~ 1|year, p = 1),
        # control = list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")
      ) %>%
        # gam %>%
        mgcv::predict.gam() %>%
        as.numeric()
    }
    
    # cmip6_reservation %>%
    #   dplyr::filter(model == "CNRM-ESM2-1",
    #                 scenario == "Historical Emissions",
    #                 year == 1980) %>%
    #   ggplot(aes(x = date,
    #              y = tas)) +
    #   geom_line()
    
    projections_falcon <-
      cmip6_reservation %>%
      {dplyr::bind_rows(., dplyr::filter(., scenario == "Historical Emissions") %>%
                          dplyr::mutate(scenario = "Moderating Emissions (SSP1-2.6)"))} %>%
      {dplyr::bind_rows(., dplyr::filter(., scenario == "Historical Emissions") %>%
                          dplyr::mutate(scenario = "Moderating Emissions (SSP1-2.6)"))} %>%
      {dplyr::bind_rows(., dplyr::filter(., scenario == "Historical Emissions") %>%
                          dplyr::mutate(scenario = "Middle of the Road (SSP2-4.5)"))} %>%
      {dplyr::bind_rows(., dplyr::filter(., scenario == "Historical Emissions") %>%
                          dplyr::mutate(scenario = "High Emissions (SSP3-7.0)"))} %>%
      {dplyr::bind_rows(., dplyr::filter(., scenario == "Historical Emissions") %>%
                          dplyr::mutate(scenario = "Accelerating Emissions (SSP5-8.5)"))} %>%
      dplyr::filter(scenario != "Historical Emissions") %>%
      dplyr::arrange(model, scenario, date) %>%
      dplyr::distinct() %>%
      dplyr::mutate(hurs = ifelse(hurs < 0, 0, hurs),
                    hurs = ifelse(hurs > 100, 100, hurs),
                    yday = lubridate::yday(date),
                    `Growing Degree Days [degF]` = tas - 50,
                    `Growing Degree Days [degF]` = 
                      ifelse(
                        `Growing Degree Days [degF]` < 0, 
                        0, 
                        `Growing Degree Days [degF]`),
                    `Frozen Precipitation [in]` = 
                      ifelse(
                        tasmin <= 32, 
                        pr, 
                        0),
                    `Heat Index [degF]` = weathermetrics::heat.index(t = tasmax,
                                                                     rh = hurs,
                                                                     round = 1)
      ) %>%
      dplyr::group_by(model, 
                      scenario) %>%
      dplyr::arrange(model, 
                     scenario,
                     year,
                     yday) %>%
      dplyr::mutate(`Normal Average Temperature [degF]` = 
                      gam_predict(x = tibble::tibble(year = year,
                                                     yday = yday,
                                                     tas = tas))
                    # mgcv::gamm(
                    #   formula = 
                    #     tas ~ s(yday, k = -1, bs = "cc") + s(year, k = 4, bs = "cr"),
                    #   correlation = nlme::corARMA(form = ~ 1|year, p = 2),
                    #   control = list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")
                    # ) %$%
                    # gam %>%
                    # mgcv::predict.gam() %>%
                    # as.numeric()#,
                    # `Normal High Temperature [degF]` = 
                    #   mgcv::gam(
                    #     formula = 
                    #       tasmax ~ s(yday, k = -1, bs = "cc") + s(year, k = -1, bs = "cr")
                    #   ) %>%
                    #   mgcv::predict.gam() %>%
                    #   as.numeric(),
                    # `Normal Low Temperature [degF]` = 
                    #   mgcv::gam(
                    #     formula = 
                    #       tasmin ~ s(yday, k = 12, bs = "cc") + s(year, k = 3, bs = "cr")
                    #   ) %>%
                    #   mgcv::predict.gam() %>%
                    #   as.numeric()
      ) %>%
      dplyr::ungroup() %>%
      # dplyr::group_by(model, date) %>%
      # dplyr::mutate(`Normal Average Temperature [degF]` = ifelse(year < 2015, mean(`Normal Average Temperature [degF]`), `Normal Average Temperature [degF]`)) %>%
      # dplyr::ungroup() %>%
      dplyr::rename(`Day of Calendar Year` = yday) %>%
      dplyr::group_by(model,
                      scenario) %>%
      dplyr::arrange(model,
                     scenario,
                     date) %>%
      dplyr::mutate(`3-Day Precipitation [in]` = 
                      RcppRoll::roll_sum(pr,
                                         n = 3,
                                         align = "right",
                                         fill = NA),
                    `6-Day Above 50 ºF` = 
                      RcppRoll::roll_prod(`Normal Average Temperature [degF]` > 50,
                                          n = 6,
                                          align = "right",
                                          fill = NA),
                    `6-Day Below 50 ºF` = 
                      RcppRoll::roll_prod(`Normal Average Temperature [degF]` < 50,
                                          n = 6,
                                          align = "right",
                                          fill = NA)) %>%
      dplyr::ungroup()
    
    # projections_falcon %>%
    # dplyr::arrange(date, model, scenario)
    #   dplyr::filter(model == "ACCESS-ESM1-5") %>%
    #   ggplot(aes(y = `Normal Average Temperature [degF]`,
    #              x = date,
    #              color = scenario)) +
    #   geom_line()
    
    trace_inches <- 
      units::set_units(2, "mm") |>
      units::set_units("in") |>
      units::drop_units()
    
    heat_index_breaks <-
      c(27, 32, 41, 54) %>%
      units::set_units("degC") %>%
      units::set_units("degF") %>%
      units::drop_units()
    
    # Calculate annual calendar year statistics
    projections_calendar_year <-
      projections_falcon %>%
      dplyr::group_by(model,
                      scenario,
                      Year = as.integer(lubridate::year(date))) %>%
      dplyr::arrange(model, scenario, date) %>%
      dplyr::mutate(`Hottest Day of the Year` = as.integer(median(`Day of Calendar Year`[`Normal Average Temperature [degF]` == max(`Normal Average Temperature [degF]`, na.rm = TRUE)]))) %>%
      dplyr::summarise(
        
        ## Temperature
        `Average Temperature [degF]` = mean(tas, na.rm = T),
        `Growing Degree Days [degF]` = sum(`Growing Degree Days [degF]`, na.rm = T),
        `Number of Frost Free Days` = sum(tasmin > 32, na.rm = TRUE),
        
        ## Extreme Heat
        `Number of Days >= 100 ºF` = sum(tasmax > 100, na.rm = TRUE),
        `Number of Days with Heat Index at "Caution" Level` =
          sum(`Heat Index [degF]` >= heat_index_breaks[[1]] &
                `Heat Index [degF]` < heat_index_breaks[[2]],
              na.rm = TRUE),
        `Number of Days with Heat Index at "Extreme caution" Level` =
          sum(`Heat Index [degF]` >= heat_index_breaks[[2]] &
                `Heat Index [degF]` < heat_index_breaks[[3]],
              na.rm = TRUE),
        `Number of Days with Heat Index at "Danger" Level` =
          sum(`Heat Index [degF]` >= heat_index_breaks[[3]] &
                `Heat Index [degF]` < heat_index_breaks[[4]],
              na.rm = TRUE),
        `Number of Days with Heat Index at "Extreme danger" Level` =
          sum(`Heat Index [degF]` >= heat_index_breaks[[4]],
              na.rm = TRUE),
        
        ## Precipitation
        `Annual Precipitation [in]` = sum(pr, na.rm = T),
        `Annual Frozen Precipitation [in]` = sum(`Frozen Precipitation [in]`, na.rm = T),
        `Spring Precipitation [in]` = 
          sum(pr[lubridate::month(date) %in% c(3,4,5)], na.rm = T),
        `Summer Precipitation [in]` = 
          sum(pr[lubridate::month(date) %in% c(6,7,8)], na.rm = T),
        `Fall Precipitation [in]` = 
          sum(pr[lubridate::month(date) %in% c(9,10,11)], na.rm = T),
        `Winter Precipitation [in]` = 
          sum(pr[lubridate::month(date) %in% c(12,1,2)], na.rm = T),
        
        ## Extreme Precipitation
        `Maximum 3-Day Precipitation [in]` = 
          max(`3-Day Precipitation [in]`, na.rm = T),
        `Average Precipitation on Wet Days [in]` = 
          mean(pr[pr > 0], na.rm = T),
        `Average Precipitation on Wet Days (trace) [in]` = 
          mean(pr[pr > trace_inches], na.rm = T),
        `Number of Wet Days` = 
          sum(pr > 0, na.rm = T),
        `Number of Wet Days (trace)` = 
          sum(pr > trace_inches, na.rm = T),
        `Number of Dry Days` = sum(pr <= 0, na.rm = T),
        `Number of Dry Days (trace)` = sum(pr <= trace_inches, na.rm = T),
        
        ## Wind
        `Average Surface Wind Speed [mph]` = mean(sfcWind, na.rm = T),
        
        ## Normal Growing Season
        # `Normal Average Temperature [degF]` = mean(`Normal Average Temperature [degF]`, na.rm = T),
        `Normal Hottest Day of the Year` = as.integer(median(`Hottest Day of the Year`)),
        `Normal First Day of Growing Season` = as.integer(dplyr::first(`Day of Calendar Year`[`Day of Calendar Year` <= `Hottest Day of the Year` & `6-Day Above 50 ºF` == 1], default = as.integer(median(`Hottest Day of the Year`)), na_rm = TRUE)),
        `Normal Last Day of Growing Season` = as.integer(dplyr::first(`Day of Calendar Year`[`Day of Calendar Year` >= `Hottest Day of the Year` & `6-Day Below 50 ºF` == 1], default = as.integer(median(`Hottest Day of the Year`)), na_rm = TRUE)),
        `Normal Length of Growing Season [days]` = (`Normal Last Day of Growing Season` - `Normal First Day of Growing Season`),
        
        ## Day of first snow after normal hottest day of the year
        `Day of First Snow` = as.integer(dplyr::first(`Day of Calendar Year`[`Day of Calendar Year` > `Hottest Day of the Year` & `Frozen Precipitation [in]` > 0], default = as.integer(NA), na_rm = TRUE))
        
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(`Normal First Day of Growing Season` = ifelse(Year == 1950 & `Normal First Day of Growing Season` == 6,
                                                                  1,
                                                                  `Normal First Day of Growing Season`),
                    `Normal Last Day of Growing Season` = ifelse(`Normal First Day of Growing Season` != `Normal Hottest Day of the Year` & `Normal Last Day of Growing Season` == `Normal Hottest Day of the Year`,
                                                                 lubridate::yday(paste0(Year,"-12-31")),
                                                                 `Normal Last Day of Growing Season`),
                    `Normal Length of Growing Season [days]` = `Normal Last Day of Growing Season` - `Normal First Day of Growing Season` + 1) %>%
      dplyr::select(model, scenario, Year, dplyr::all_of(variable_dists$Variable)) %>%
      dplyr::arrange(model, scenario, Year)
    
    # projections_calendar_year %>%
    #   # dplyr::mutate(`Normal First Day of Growing Season` = ifelse(Year == 1950 & `Normal First Day of Growing Season` == 6,
    #   #                                                             1,
    #   #                                                             `Normal First Day of Growing Season`),
    #   #               `Normal Last Day of Growing Season` = ifelse(`Normal First Day of Growing Season` != `Normal Hottest Day of the Year` & `Normal Last Day of Growing Season` == `Normal Hottest Day of the Year`,
    #   #                                                            lubridate::yday(paste0(Year,"-12-31")),
    #   #                                                            `Normal Last Day of Growing Season`),
    #   #               `Normal Length of Growing Season [days]` = `Normal Last Day of Growing Season` - `Normal First Day of Growing Season` + 1) %>%
    #   dplyr::select(model,
    #                 scenario,
    #                 Year,
    #                 # `Normal Hottest Day of the Year`,
    #                 `Normal First Day of Growing Season`,
    #                 `Normal Last Day of Growing Season`,
    #                 `Normal Length of Growing Season [days]`) %>%
    #   dplyr::arrange(Year, model) %>%
    #   dplyr::filter(scenario == "High Emissions (SSP3-7.0)",
    #                 Year >= 2070) %>%
    #   print(n = 500)
    
    # projections_calendar_year %>%
    #   dplyr::filter(model == 1) %>%
    #   ggplot(aes(y = `Normal Length of Growing Season [days]`,
    #              x = Year,
    #              color = scenario)) +
    #   geom_line()
    
    
    # 
    # projections_falcon %>%
    #   dplyr::select(model,
    #                 scenario,
    #                 year,
    #                 `Day of Calendar Year`,
    #                 `6-Day Above 50 ºF`,
    #                 `6-Day Below 50 ºF`) %>%
    #   dplyr::arrange(year,
    #                  `Day of Calendar Year`) %>%
    #   dplyr::filter(!is.na(`6-Day Above 50 ºF`)) %>%
    #   print(n = 100)
    
    gamlss_conf <- 
      function(model, 
               data,
               newdata = 
                 data$Year |>
                 unique() |>
                 sort() |>
                 tibble::tibble(Year = _),
               p = c(0.25, 0.5, 0.75)
      ){
        
        if(!is.gamlss(model))
          return(
            p %>%
              magrittr::set_names(., paste0(. * 100, "%")) %>%
              purrr::map(\(x) data$value[[1]]) %>%
              dplyr::bind_cols(newdata, .)
          )
        
        predictions <-
          suppressWarnings(
            predictAll(model,
                       data = data,
                       newdata = newdata,
                       output = "data.frame") %>%
              tibble::as_tibble()
          )
        
        qfun <- paste0("q", model$family[[1]])
        lpar <- length(model$parameters)
        
        p %>%
          magrittr::set_names(., paste0(. * 100, "%")) %>%
          purrr::map(\(x){
            if (lpar == 1) {
              newcall <- call(qfun, 
                              p = x, 
                              mu = predictions$mu)
            }
            else if (lpar == 2) {
              newcall <- call(qfun, p = x, 
                              mu = predictions$mu, 
                              sigma = predictions$sigma)
            }
            else if (lpar == 3) {
              newcall <- call(qfun, p = x,
                              mu = predictions$mu, 
                              sigma = predictions$sigma, 
                              nu = predictions$nu)
            }
            else {
              newcall <- call(qfun, p = x, 
                              mu = predictions$mu, 
                              sigma = predictions$sigma, 
                              nu = predictions$nu, 
                              tau = predictions$tau)
            }
            
            eval(newcall)
          }) %>%
          dplyr::bind_cols(newdata, .)
        
      }
    
    invert_yday <- function(x){
      as.integer((1 - (x / 366)) * 366)
    }
    
    projections_calendar_year_smooth <-
      projections_calendar_year %>%
      dplyr::arrange(model, scenario, Year) %>%
      dplyr::select(-model) %>%
      dplyr::mutate_if(function(x){class(x) == "units"},
                       units::drop_units) %>%
      dplyr::group_by(scenario, Year) %>%
      tidyr::pivot_longer(-scenario:-Year, names_to = "Variable") %>%
      dplyr::filter(!is.na(value)) %>%
      na.omit() %>%
      dplyr::arrange(scenario, Variable, Year) %>%
      dplyr::group_by(scenario, Variable) %>%
      tidyr::nest(data = c(Year, value)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(variable_dists,
                       by = dplyr::join_by(Variable)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(data = ifelse(Family == ndays_family,
                                  list(data %>%
                                         dplyr::mutate(value = value / lubridate::yday(paste0(Year,"-12-31")))),
                                  list(data))) %>%
      dplyr::mutate(data = ifelse(Variable %in% c("Day of First Snow",
                                                  "Normal Last Day of Growing Season"),
                                  list(data %>%
                                         dplyr::mutate(value = invert_yday(value))),
                                  list(data))) %>%
      # dplyr::mutate(cov = sd(data$value, na.rm = TRUE)/mean(data$value, na.rm = TRUE)) %>%
      # dplyr::mutate(cov = ifelse(Family == ndays_family & cov > 2,
      #                                 0,
      #                                 cov)) %>%
      
      # dplyr::filter(Variable == "Number of Days >= 100 ºF") %>%
      # magrittr::extract(18,)  %>%
      dplyr::mutate(
        gamlss =
          list(
            tryCatch(
              gamlss::gamlss(
                data = dplyr::filter(data, !is.na(value)),
                formula = 
                  value ~ pb(Year, df = 2, inter = 2),
                sigma.formula = ~ pb(Year, df = 2, inter = 1),
                nu.formula = ~ pb(Year, df = 1, inter = 1),
                tau.formula = ~ pb(Year, df = 1, inter = 1),
                family = Family,
                control = gamlss.control(trace = FALSE)
              ),
              error = function(e){NA}
            )
          )
      )  %>%
      dplyr::mutate(
        gamlss_conf = list(
          gamlss_conf(model = gamlss,
                      data = dplyr::filter(data, !is.na(value)))
        )
      ) %>%
      dplyr::mutate(gamlss_conf = 
                      ifelse(Family == ndays_family,
                             list(
                               gamlss_conf %>%
                                 dplyr::mutate(dplyr::across(!c(Year), \(x) x * 365))
                             ),
                             list(gamlss_conf))) %>%
      dplyr::mutate(gamlss_conf = ifelse(Variable %in% c("Day of First Snow",
                                                         "Normal Last Day of Growing Season"),
                                         list(
                                           gamlss_conf %>%
                                             dplyr::mutate(dplyr::across(!c(Year), \(x) invert_yday(x))) %>%
                                             dplyr::rename(`75%` = `25%`,
                                                           `25%` = `75%`)
                                         ),
                                         list(gamlss_conf))) %>%
      dplyr::ungroup() %>%
      dplyr::select(scenario, Variable, gamlss_conf) %>%
      tidyr::unnest(gamlss_conf) %>%
      # dplyr::mutate(scenario = ifelse(Year < 2015, "Historical Emissions", scenario)) %>%
      dplyr::rename(value = `50%`,
                    lower = `25%`,
                    upper = `75%`) %>%
      dplyr::group_by(scenario, Variable, Year) %>%
      dplyr::summarise(value = mean(value, na.rm = TRUE),
                       lower = min(lower, na.rm = TRUE),
                       upper = max(upper, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::arrange(scenario, Variable, Year)
    

# 
#     test_gamlss <- function(data, family){
# 
#       test <- gamlss::gamlss(
#         data = data,
#         formula =
#           value ~ pb(Year, df = 2, inter = 2),
#         sigma.formula = ~ pb(Year, df = 2, inter = 1),
#         nu.formula = ~ pb(Year, df = 1, inter = 1),
#         tau.formula = ~ pb(Year, df = 1, inter = 1),
#         family = family
#       )
# 
#       gamlss_conf(test,
#                   data = data) %>%
#         dplyr::rename(`75%` = `25%`,
#                       `25%` = `75%`) %>%
#         ggplot(aes(x = Year)) +
#         geom_ribbon(aes(ymin = `25%`,
#                         ymax = `75%`)) +
#         geom_line(aes(y = `50%`)) +
#         geom_point(aes(y = value),
#                    data = data)
#     }
# 
#     test_gamlss(
#       data = projections_calendar_year_smooth$data[[37]],
#       family = "NBI"
#     )

    # 
    # 
    # projections_calendar_year %>%
    #   dplyr::select(model, scenario, Year, `Average Temperature [degF]`, `Normal First Day of Growing Season`,`Normal Last Day of Growing Season`)
    # 
    # print(projections_calendar_year_smooth, n = 104)
    # test <-
    #   gamlss::gamlss(
    #   data = projections_calendar_year_smooth[10,]$data[[1]],
    #   formula =
    #     value ~ cs(Year, df = 2),
    #   sigma.formula = ~ cs(Year, df = 2),
    #   nu.formula = ~ cs(Year, df = 2),
    #   tau.formula = ~ cs(Year, df = 2),
    #   family = "SHASHo2",
    #   # mu.start = 0.00
    #   # control = gamlss.control(trace = FALSE),
    #   # method = mixed(2,10)
    # )
    # 
    # predictAll(test, newdata = newdata) %$%
    #   qSHASHo2( p = 0.25,
    #                   mu = mu,
    #                   sigma = sigma,
    #                   nu = nu,
    #           tau = tau
    #           )
    
    outfiles <-
      projections_falcon %>%
      # dplyr::mutate(scenario = ifelse(year < 2015, "Historical Emissions", scenario)) %>%
      dplyr::select(model:tasmin) %>%
      # dplyr::distinct() %>%
      dplyr::mutate(NAME = x) %>%
      # dplyr::select(-run) %>%
      dplyr::mutate(dplyr::across(where(is.double), ~round(.x, digits = 4))) %>%
      dplyr::rename(Model = model,
                    Scenario = scenario,
                    Date = date,
                    `Near-Surface Relative Humidity [%]` = hurs,
                    `Near-Surface Specific Humidity [kg/kg]` = huss,
                    `Precipitation [in]` = pr,
                    `Surface Downwelling Longwave Radiation [W/m^2]` = rlds,
                    `Surface Downwelling Shortwave Radiation [W/m^2]` = rsds,
                    `Daily-Mean Near-Surface Wind Speed [mph]` = sfcWind,
                    `Daily Average Near-Surface Air Temperature [degF]` = tas,
                    `Daily Maximum Near-Surface Air Temperature [degF]` = tasmax,
                    `Daily Minimum Near-Surface Air Temperature [degF]` =  tasmin) %>%
      dplyr::mutate(Scenario = factor(Scenario,
                                      levels = c(#"Historical Emissions",
                                        "Moderating Emissions (SSP1-2.6)",
                                        "Middle of the Road (SSP2-4.5)",
                                        "High Emissions (SSP3-7.0)",
                                        "Accelerating Emissions (SSP5-8.5)"),
                                      ordered = TRUE)) %>%
      dplyr::arrange(NAME, Model, Scenario, Date) %>%
      dplyr::group_by(NAME, Scenario) %>%
      tidyr::nest() %>%
      tidyr::pivot_wider(names_from = Scenario,
                         values_from = data,
                         names_prefix = "Raw — ") %>%
      dplyr::left_join(
        projections_calendar_year %>%
          # dplyr::mutate(scenario = ifelse(Year < 2015, "Historical Emissions", scenario)) %>%
          # dplyr::distinct() %>%
          dplyr::mutate(NAME = x) %>%
          dplyr::rename(Model = model,
                        Scenario = scenario) %>%
          # dplyr::select(-run) %>%
          dplyr::mutate(dplyr::across(where(is.double), ~round(.x, digits = 4))) %>%
          dplyr::mutate(Scenario = factor(Scenario,
                                          levels = c(#"Historical Emissions",
                                            "Moderating Emissions (SSP1-2.6)",
                                            "Middle of the Road (SSP2-4.5)",
                                            "High Emissions (SSP3-7.0)",
                                            "Accelerating Emissions (SSP5-8.5)"),
                                          ordered = TRUE)) %>%
          dplyr::arrange(NAME, Model, Scenario, Year) %>%
          dplyr::group_by(NAME) %>%
          tidyr::nest(`Annual Projections` = !NAME)
      ) %>%
      dplyr::left_join(
        projections_calendar_year_smooth %>%
          # dplyr::mutate(scenario = ifelse(Year < 2015, "Historical Emissions", scenario)) %>%
          # dplyr::distinct() %>%
          dplyr::mutate(NAME = x) %>%
          dplyr::rename(Scenario = scenario,
                        Value = value,
                        `25th Percentile` = lower,
                        `75th Percentile` = upper) %>%
          dplyr::mutate(dplyr::across(where(is.double), ~round(.x, digits = 4))) %>%
          dplyr::mutate(Scenario = factor(Scenario,
                                          levels = c(#"Historical Emissions",
                                            "Moderating Emissions (SSP1-2.6)",
                                            "Middle of the Road (SSP2-4.5)",
                                            "High Emissions (SSP3-7.0)",
                                            "Accelerating Emissions (SSP5-8.5)"),
                                          ordered = TRUE)) %>%
          dplyr::arrange(NAME, Scenario, Variable, Year) %>%
          dplyr::group_by(NAME) %>%
          tidyr::nest(`Smoothed Projections` = !NAME)
      ) %>%
      dplyr::select(NAME,
                    `Annual Projections`,
                    `Smoothed Projections`,
                    dplyr::everything()) %>%
      rename_with(~str_remove(., "\\(.*"), everything()) %>%
      dplyr::rename_all(stringr::str_trim)
    
    outfiles %>%
      dplyr::mutate(
        Metadata =
          paste0("CMIP6 ag-climate projections for ", NAME) %>%
          purrr::map(
            c,
            "This workbook contains place-based climate data for Native American, Alaska Native, and Native Hawaiian lands located in the United States. Climate data and projections for temperature, precipitation, and other metrics related to crop, livestock and forestry agriculture are shown in the accompanying graphs. The data derive from eight Coupled Model Intercomparison Project Phase 6 (CMIP6) global climate models and four socioeconomic scenarios for the period from 2015 to 2100, as well as the historical simulation for each model for the period 1950 to 2014. Raw data are extracted for the location of the reservation from the NASA Earth Exchange (NEX) Global Daily Downscaled Projections (GDDP) dataset (NEX-GDDP-CMIP6). Further information on the NASA NEX downscaled product, including descriptions of the projected climate variables, can be found at https://www.nccs.nasa.gov/services/data-collections/land-based-products/nex-gddp-cmip6.",
            "Spatial data on Native lands were derived from the US Census TIGER/Line database, which includes all tribally controlled lands in the United States, as well as Alaska Native Village Statistical Areas (ANVSA) and State Designated Tribal Statistical Areas (SDTSA). We divided the Navajo Nation into its five agencies to better represent climate differences across the Nation. We used the recently defined climate divisions for the State of Hawai‘i (Luo et al 2024) to represent climate difference on the Hawaiian Islands.",
            "We aggregated daily data from each model and scenario into seasonal or annual statistics (the Annual Projections worksheet), and then we combined the results from the eight models using a generalized additive model (the Smoothed Projections worksheet). We provide raw daily data for each scenario in the Raw worksheets. Growing Degree Days utilize a 50 ºF base temperature in their calculation, making them relevant to corn production.",
            "These data were produced by the Montana Climate Office and the Native Climate project with funding from the National Institute of Food and Agriculture (NIFA), US Department of Agriculture.",
            "Please contact Kyle Bocinsky at kyle.bocinsky@umontana.edu with any questions. Code for producing all data supplied here is freely available on Github: https://github.com/native-climate/cmip6-reservations."
          ) %>%
          purrr::map(tibble::as_tibble)
      ) %>%
      dplyr::select(NAME, Metadata, dplyr::everything()) %>%
      dplyr::group_by(NAME) %>%
      dplyr::group_split(.keep = FALSE) %>%
      purrr::map(~unlist(as.list(.x), recursive = FALSE)) %>%
      magrittr::set_names(outfiles$NAME) %>%
      purrr::iwalk(function(x, y){
        wb <- openxlsx::createWorkbook(
          creator = "Montana Climate Office",
          title = paste0("CMIP6 climate projections for ", y))
        
        names(x) %>%
          purrr::walk(~openxlsx::addWorksheet(wb,
                                              sheetName = .x,
                                              gridLines = FALSE))
        
        openxlsx::setColWidths(wb,
                               sheet = "Metadata",
                               cols = 1,
                               widths = 80)
        
        openxlsx::writeData(wb,
                            sheet = "Metadata",
                            x = x[["Metadata"]],
                            colNames = FALSE)
        
        ## style for body
        openxlsx::addStyle(wb,
                           sheet = "Metadata",
                           style = openxlsx::createStyle(
                             textDecoration = "bold"
                           ),
                           rows = 1,
                           cols = 1,
                           gridExpand = TRUE)
        openxlsx::addStyle(wb,
                           sheet = "Metadata",
                           style = openxlsx::createStyle(wrapText = TRUE),
                           rows = 2:6,
                           cols = 1,
                           gridExpand = TRUE)
        
        x[-1] %>%
          purrr::iwalk(~openxlsx::writeDataTable(wb,
                                                 sheet = .y,
                                                 x = .x))
        
        openxlsx::setColWidths(wb, sheet = "Annual Projections",
                               cols = c(1,2), 
                               widths = "auto")
        openxlsx::setColWidths(wb, sheet = "Annual Projections", 
                               cols = 4:30, 
                               widths = 20)
        openxlsx::addStyle(wb,
                           sheet = "Annual Projections",
                           style = openxlsx::createStyle(wrapText = TRUE),
                           rows = 1,
                           cols = 1:30,
                           gridExpand = TRUE)
        
        openxlsx::setColWidths(wb, sheet = "Smoothed Projections",
                               cols = c(1,3), widths = "auto")
        
        4:7 %>%
          purrr::walk(~openxlsx::setColWidths(wb, sheet = .x, cols = 1, widths = "auto"))
        4:7 %>%
          purrr::walk(~openxlsx::setColWidths(wb, sheet = .x, cols = 3:11, widths = 20))
        4:7 %>%
          purrr::walk(~openxlsx::addStyle(wb,
                                          sheet = .x,
                                          style = openxlsx::createStyle(wrapText = TRUE),
                                          rows = 1,
                                          cols = 1:11,
                                          gridExpand = TRUE))
        
        
        openxlsx::saveWorkbook(wb,
                               file = outfile,
                               overwrite = TRUE)
        
      })
    
    return(outfile)
    
  }


plot_ag_projections <-
  function(
    xlsx,
    college = dirname(xlsx) %>%
      basename(),
    outfile = paste0(
      tools::file_path_sans_ext(xlsx),
      ".pdf"
    ),
    colors =
      c("Historical Emissions" = rgb(0,0,0,
                                     maxColorValue = 255),
        "transparent",
        " "  = "transparent",
        "  "  = "transparent",
        "Moderating Emissions (SSP1-2.6)" = rgb(34,46,77,
                                                maxColorValue = 255),
        "Middle of the Road (SSP2-4.5)" = rgb(223,146,71,
                                              maxColorValue = 255),
        "High Emissions (SSP3-7.0)" = rgb(187,54,51,
                                          maxColorValue = 255),
        "Accelerating Emissions (SSP5-8.5)" = rgb(122,41,40,
                                                  maxColorValue = 255)
        
      )
  ){
    
    # on.exit(dev.off())
    
    projections <-
      readxl::read_xlsx(xlsx, sheet = "Annual Projections") %>%
      readr::type_convert(guess_integer = TRUE) %>%
      dplyr::mutate(Scenario = ifelse(Year < 2015, "Historical Emissions", Scenario)) %>%
      dplyr::group_by(Model, Scenario, Year) %>%
      dplyr::mutate(index = dplyr::row_number()) %>%
      dplyr::mutate(
        Scenario = factor(Scenario,
                          levels = c("Historical Emissions",
                                     "Moderating Emissions (SSP1-2.6)",
                                     "Middle of the Road (SSP2-4.5)",
                                     "High Emissions (SSP3-7.0)",
                                     "Accelerating Emissions (SSP5-8.5)"),
                          ordered = TRUE)
      ) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("Number of"), \(x) ifelse(x > 365, 365, x))) %>%
      dplyr::mutate(dplyr::across(dplyr::contains("[days]"), \(x) ifelse(x > 365, 365, x))) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("Normal Last"), \(x) ifelse(x > 365, 365, x))) %>%
      tidyr::pivot_longer(!c(Model, Scenario, Year, index), names_to = "Variable", values_to = "Value") %>%
      dplyr::group_by(Model, Scenario, Variable, Year) %>%
      dplyr::mutate(index = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(Model, Scenario, Variable, Year) %>%
      group_by(Variable) %>%
      tidyr::nest(Annual = !Variable) %>%
      dplyr::left_join(readxl::read_xlsx(xlsx, sheet = "Smoothed Projections") %>%
                         dplyr::mutate(Scenario = ifelse(Year < 2015, "Historical Emissions", Scenario)) %>%
                         dplyr::mutate(Scenario = factor(Scenario,
                                                         levels = c("Historical Emissions",
                                                                    "Moderating Emissions (SSP1-2.6)",
                                                                    "Middle of the Road (SSP2-4.5)",
                                                                    "High Emissions (SSP3-7.0)",
                                                                    "Accelerating Emissions (SSP5-8.5)"),
                                                         ordered = TRUE)) %>%
                         dplyr::group_by(Scenario, Variable, Year) %>%
                         dplyr::summarise(Value = mean(Value, na.rm = TRUE),
                                          `25th Percentile` = min(`25th Percentile`, na.rm = TRUE),
                                          `75th Percentile` = max(`75th Percentile`, na.rm = TRUE)) %>%
                         group_by(Variable) %>%
                         tidyr::nest(Smoothed = !Variable))
    
    
    
    native_climate <-
      grid::rasterGrob(
        png::readPNG("native-climate.png"),
        y=-0.275,
        height = unit(0.2, "npc"),
        x=0,
        # width = unit(0.2, "npc"),
        vjust=0,
        hjust=0,
        interpolate = TRUE)
    
    mco <-
      grid::rasterGrob(
        png::readPNG("mco.png"),
        y=-0.225,
        height = unit(0.1, "npc"),
        x=0.16,
        # width = unit(0.2, "npc"),
        vjust=0,
        hjust=0,
        interpolate = TRUE)
    
    graphs <-
      c("Annual Precipitation [in]",
        "Annual Frozen Precipitation [in]",
        "Spring Precipitation [in]",
        "Summer Precipitation [in]",
        "Fall Precipitation [in]",
        "Winter Precipitation [in]",
        "Maximum 3-Day Precipitation [in]",
        "Average Precipitation on Wet Days [in]",
        "Average Precipitation on Wet Days (trace) [in]",
        "Number of Wet Days",
        "Number of Wet Days (trace)",
        "Number of Dry Days",
        "Number of Dry Days (trace)",
        "Average Temperature [degF]",
        "Number of Days >= 100 ºF",
        "Growing Degree Days [degF]",
        "Number of Frost Free Days",
        "Average Surface Wind Speed [mph]",
        # "Normal Average Temperature [degF]",
        "Normal Length of Growing Season [days]"
      ) %>%
      magrittr::set_names(.,.) %>%
      purrr::map(function(variable){
        ggplot(mapping =
                 aes(x = Year,
                     y = Value,
                     color = Scenario,
                     fill = Scenario)) +
          geom_line(data =
                      dplyr::filter(projections, Variable == variable)$Annual[[1]],
                    mapping = aes(group = interaction(Model, Scenario, index)),
                    linewidth = 0.1) +
          geom_ribbon(data =
                        dplyr::filter(projections, Variable == variable)$Smoothed[[1]],
                      mapping =
                        aes(ymin = `25th Percentile`,
                            ymax = `75th Percentile`),
                      color = NA,
                      alpha = 0.5,
                      linewidth = 0.25) +
          geom_line(data =
                      dplyr::filter(projections, Variable == variable)$Smoothed[[1]],
                    linewidth = 1.25) +
          scale_color_manual(values = colors) +
          scale_fill_manual(values = colors) +
          scale_x_continuous(expand = c(0,0),
                             breaks = seq(1950,2100,10),
                             limits = c(1950,2100)) +
          labs(x = NULL,
               y = variable,
               title = college,
               subtitle = paste0("Climate Projections of ", variable)) +
          theme_minimal(14) +
          theme(
            legend.title = element_blank(),
            legend.justification = c(1, 1),
            legend.position = "bottom",
            legend.key.width = unit(0.25,"in"),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            # axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
            plot.margin=unit(c(0.1,0.2,0.1,0.1), "in")
          ) +
          ggplot2::guides(colour = guide_legend(ncol = 2)) +
          annotation_custom(native_climate, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
          annotation_custom(mco, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
          coord_cartesian(clip = "off")
      })
    
    day_graphs <-
      c(#"Normal Hottest Day of the Year",
        "Normal First Day of Growing Season",
        "Normal Last Day of Growing Season",
        "Day of First Snow"
      ) %>%
      magrittr::set_names(.,.) %>%
      purrr::map(function(variable){
        if(is.null(dplyr::filter(projections, Variable == variable)$Smoothed[[1]]))
          return(NULL)
        
        ggplot(mapping =
                 aes(x = Year,
                     y = ifelse(Value > 365, 365, Value) %>%
                       round() %>%
                       as.character() %>%
                       paste0("-2023") %>%
                       lubridate::parse_date_time(orders = "j-Y") %>%
                       lubridate::as_date(),
                     color = Scenario,
                     fill = Scenario)) +
          geom_line(data =
                      dplyr::filter(projections, Variable == variable)$Annual[[1]],
                    mapping = aes(group = interaction(Model, Scenario, index)),
                    linewidth = 0.1) +
          geom_ribbon(data =
                        dplyr::filter(projections, Variable == variable)$Smoothed[[1]],
                      mapping =
                        aes(ymin = ifelse(`25th Percentile` > 365, 365, `25th Percentile`) %>%
                              round() %>%
                              as.character() %>%
                              paste0("-2023") %>%
                              lubridate::parse_date_time(orders = "j-Y") %>%
                              lubridate::as_date(),
                            ymax = ifelse(`75th Percentile` > 365, 365, `75th Percentile`) %>%
                              round() %>%
                              as.character() %>%
                              paste0("-2023") %>%
                              lubridate::parse_date_time(orders = "j-Y") %>%
                              lubridate::as_date()),
                      color = NA,
                      alpha = 0.5,
                      linewidth = 0.25) +
          geom_line(data =
                      dplyr::filter(projections, Variable == variable)$Smoothed[[1]],
                    linewidth = 1.25) +
          scale_color_manual(values = colors) +
          scale_fill_manual(values = colors) +
          scale_x_continuous(expand = c(0,0),
                             breaks = seq(1950,2100,10),
                             limits = c(1950,2100)) +
          scale_y_date() +
          labs(x = NULL,
               y = variable,
               title = college,
               subtitle = paste0("Climate Projections of ", variable)) +
          theme_minimal(14) +
          theme(
            legend.title = element_blank(),
            legend.justification = c(1, 1),
            legend.position = "bottom",
            legend.key.width = unit(0.25,"in"),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            # axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
            plot.margin=unit(c(0.1,0.2,0.1,0.1), "in")
          ) +
          ggplot2::guides(colour = guide_legend(ncol = 2)) +
          annotation_custom(native_climate, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
          annotation_custom(mco, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
          coord_cartesian(clip = "off")
      })
    
    hi_graph <-
      list(
        "Heat Index" = projections %>%
          dplyr::filter(Variable %in% c("Number of Days with Heat Index at \"Caution\" Level",
                                        "Number of Days with Heat Index at \"Danger\" Level",
                                        "Number of Days with Heat Index at \"Extreme caution\" Level",
                                        "Number of Days with Heat Index at \"Extreme danger\" Level" )) %>%
          dplyr::select(Variable, Smoothed) %>%
          tidyr::unnest(Smoothed) %>%
          dplyr::mutate(Variable = factor(Variable,
                                          levels = c("Number of Days with Heat Index at \"Caution\" Level",
                                                     "Number of Days with Heat Index at \"Extreme caution\" Level",
                                                     "Number of Days with Heat Index at \"Danger\" Level",
                                                     "Number of Days with Heat Index at \"Extreme danger\" Level" ),
                                          labels = c("Caution", "Extreme Caution", "Danger", "Extreme Danger"),
                                          ordered = TRUE)) %>%
          {dplyr::bind_rows(., dplyr::filter(., Scenario == "Historical Emissions") %>%
                              dplyr::mutate(Scenario = "Moderating Emissions (SSP1-2.6)"))} %>%
          {dplyr::bind_rows(., dplyr::filter(., Scenario == "Historical Emissions") %>%
                              dplyr::mutate(Scenario = "Moderating Emissions (SSP1-2.6)"))} %>%
          {dplyr::bind_rows(., dplyr::filter(., Scenario == "Historical Emissions") %>%
                              dplyr::mutate(Scenario = "Middle of the Road (SSP2-4.5)"))} %>%
          {dplyr::bind_rows(., dplyr::filter(., Scenario == "Historical Emissions") %>%
                              dplyr::mutate(Scenario = "High Emissions (SSP3-7.0)"))} %>%
          {dplyr::bind_rows(., dplyr::filter(., Scenario == "Historical Emissions") %>%
                              dplyr::mutate(Scenario = "Accelerating Emissions (SSP5-8.5)"))} %>%
          dplyr::filter(Scenario != "Historical Emissions") %>%
          dplyr::distinct(Variable, Scenario, Year, .keep_all = TRUE) %>%
          ggplot(mapping =
                   aes(x = Year,
                       y = Value,
                       color = Variable,
                       fill = Variable,
                       group = Variable)) +
          geom_area() +
          scale_color_manual(values = c("Caution" = rgb(255,253,113,
                                                        maxColorValue = 255),
                                        "Extreme Caution" = rgb(254,214,48,
                                                                maxColorValue = 255),
                                        "Danger" = rgb(253,140,37,
                                                       maxColorValue = 255),
                                        "Extreme Danger" = rgb(252,13,27,
                                                               maxColorValue = 255)
                                        
          )) +
          scale_fill_manual(values = c("Caution" = rgb(255,253,113,
                                                       maxColorValue = 255),
                                       "Extreme Caution" = rgb(254,214,48,
                                                               maxColorValue = 255),
                                       "Danger" = rgb(253,140,37,
                                                      maxColorValue = 255),
                                       "Extreme Danger" = rgb(252,13,27,
                                                              maxColorValue = 255)
                                       
          )) +
          facet_wrap("Scenario",
                     ncol = 1) +
          scale_x_continuous(expand = c(0,0),
                             breaks = seq(1950,2100,10),
                             limits = c(1950,2100)) +
          scale_y_continuous(expand = c(0,NA),
                             limits = c(0,NA)) +
          labs(x = NULL,
               y = "Number of Days at Each Heat Hazard Level",
               title = college,
               subtitle = paste0("Climate Projections of ", "Heat Index Hazard")) +
          theme_minimal(14) +
          theme(
            legend.title = element_blank(),
            legend.justification = c(1, 1),
            legend.position = "bottom",
            legend.key.width = unit(0.25,"in"),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
            plot.margin=unit(c(0.1,0.2,0.1,0.1), "in")
          ) +
          ggplot2::guides(colour = guide_legend(ncol = 4))
      )
    
    graphs <-
      c(graphs, day_graphs, hi_graph) %>%
      magrittr::extract(
        variable_dists$Variable %>%
          stringr::str_replace_all("Number of Days with Heat Index.*",
                                   "Heat Index") %>%
          unique()
      ) %>%
      purrr::compact()
    
    cairo_pdf(outfile,
              height = 7.5,
              width = 10,
              onefile = TRUE)
    
    print(graphs)
    
    dev.off()
    
    png_dir <-
      outfile |>  
      dirname() |>
      file.path("png")
    
    dir.create(png_dir,
               recursive = TRUE,
               showWarnings = FALSE)
    
    graphs |>
      purrr::imap(\(x,n) ggplot2::ggsave(filename = file.path(png_dir, paste0(n, ".png")),
                                         plot = x,
                                         device = ragg::agg_png,
                                         height = 7.5,
                                         width = 10,
                                         bg = "white"))
    
    # system(paste0("~/git/compress.sh ", stringr::str_replace_all(outfile, " ", "\\\\ ")))
    
    return(outfile)
    
  }
# 
# xlsxs <-
#   tribal_land$`Native Land` %>%
#   purrr::map(process_reservation)

# process_reservation("Akutan ANVSA", force = TRUE) %>%
#   plot_ag_projections()
# 
# process_reservation("Blackfeet Indian Reservation", force = TRUE) %>%
#   plot_ag_projections()
# 
# process_reservation("Acoma Pueblo") %>%
#   plot_ag_projections()

process_all <-
  function(reservation){
    process_reservation(reservation) %>%
      plot_ag_projections()
  }

# Start 4 cores
plan(future.callr::callr,
     workers = min(parallelly::availableCores() - 1, 48))
# plan(future.callr::callr, 
#      workers = 2)

cmip6_reservations <-
  tribal_land$`Native Land` %>%
  # c("Akutan ANVSA", 
  #   "Blackfeet Indian Reservation",
  #   "Acoma Pueblo",
  #   "Hopi Reservation",
  #   "Tunica-Biloxi Reservation",
  #   "Arctic Village ANVSA",
  #   "Hawai‘i Climate Divisions — Kona") %>%
  furrr::future_map(
    .f = process_all,
    .env_globals = globalenv(),
    .options = furrr::furrr_options(scheduling = FALSE),
    .progress = TRUE
  )

plan(sequential)
gc()
gc()


# Start 4 cores
plan(future.callr::callr,
     workers = min(parallelly::availableCores() - 1, 48))
# plan(future.callr::callr, 
#      workers = 2)

list.files("upload/pdf",
           pattern = "pdf",
           recursive = TRUE,
           full.names = TRUE) %>%
  furrr::future_map(
    .f = \(x) system(paste0("~/git/compress.sh ", x)),
    .env_globals = globalenv(),
    .options = furrr::furrr_options(scheduling = FALSE),
    .progress = TRUE
  )

plan(sequential)
gc()
gc()

# Publish upload/ to s3://native-resilience/projections/ with
# Rscript projections-publish.R


# list.files("output",
#            pattern = "pdf",
#            recursive = TRUE,
#            full.names = TRUE) %>%
#   purrr::walk(
#     ~qpdf::pdf_combine(input = c("Native Climate Agricultural Projections Summary.pdf", .x),
#                        output = paste0(.x, ".new.pdf"))
#   )






# 
# list.files("output",
#            pattern = "pdf",
#            recursive = TRUE,
#            full.names = TRUE) %>%
#   purrr::walk(
#     ~qpdf::pdf_combine(input = c("Native Climate Agricultural Projections Summary.pdf", .x),
#                        output = paste0(.x, ".new.pdf"))
#   )
# 
# list.files("output",
#            pattern = "pdf.new",
#            recursive = TRUE,
#            full.names = TRUE) %>%
#   file.rename(from = .,
#               to = stringr::str_remove(., ".new.pdf"))
# 
# # list.dirs("output", recursive = FALSE) %>%
# #   basename() %>%
# #   file.path("printing", .) %>%
# #   purrr::walk(~dir.create(path = .x,
# #                           recursive = TRUE,
# #                           showWarnings = FALSE))
# 
# dir.create("upload")
# list.dirs("output", recursive = FALSE) %>%
#   purrr::walk(
#     ~zip::zip(
#       zipfile = file.path("upload",
#                           paste0(stringr::str_replace_all(basename(.x), " ", "_"), ".zip")),
#       files = fs::dir_ls(.x,
#                          all = FALSE,
#                          recurse = FALSE,
#                          type = "file"),
#       include_directories = FALSE,
#       mode = "cherry-pick"
#     )
#   )
# 
# 
# list.files("output",
#            pattern = "pdf",
#            recursive = TRUE,
#            full.names = TRUE) %>%
#   purrr::walk(
#     ~qpdf::pdf_rotate_pages(input = .x,
#                             pages = 4:(qpdf::pdf_length(.x)),
#                             output = stringr::str_replace(.x, "output", "printing"))
#   )
# 
# 
# # list.dirs("output") %>%
# #   purrr::walk(~file.copy("Smuda_Himu.pdf", .x))
# 
# 
# 
