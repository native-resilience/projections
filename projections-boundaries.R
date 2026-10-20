# projections-boundaries.R — build native-land.parquet, the units the
# projections are extracted for.
#
# Boundaries come from the census-aiannh archive (TIGER/Line AIANNH, one row
# per GEOID component), using its latest vintage. Reservations / statistical
# areas (COMPTYP R) and off-reservation trust lands (COMPTYP T, which includes
# Hawaiian Home Lands) are separate units. Both are extracted; only R units
# (`modeled`) get annual metrics, smoothing and published graphs. The Navajo
# Nation reservation is split into its five agencies, and the Hawai‘i climate
# divisions (Luo et al. 2024) are added as modeled units.
#
#   Rscript projections-boundaries.R

source("R/setup.R")

CENSUS_AIANNH <-
  Sys.getenv("CENSUS_AIANNH_URL",
             unset = "https://data.sustainable-fsa.com/census-aiannh")

census_year <-
  rev(2020:(lubridate::year(lubridate::today()) + 1)) %>%
  purrr::detect(\(y) url_exists(file.path(CENSUS_AIANNH, "data", "parquet",
                                          paste0(y, "-aiannh.parquet"))))
stopifnot("no census-aiannh vintage found" = !is.null(census_year))
message("Using census-aiannh vintage ", census_year)

aiannh <-
  file.path(CENSUS_AIANNH, "data", "parquet",
            paste0(census_year, "-aiannh.parquet")) %>%
  sf::read_sf() %>%
  dplyr::transmute(land_id = GEOID, name = NameLSAD, GEOID, AIANNHCE,
                   COMPTYP, modeled = COMPTYP == "R") %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

# ---- Navajo Nation agencies ----
navajo_chapters <-
  arcgislayers::arc_read("https://services4.arcgis.com/ul3DV3LWHhAM8P61/ArcGIS/rest/services/Navajo_Nation_2013/FeatureServer/0") %>%
  sf::st_drop_geometry() %>%
  dplyr::select(Chapter, Agency) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_to_title)) %>%
  tibble::as_tibble() %>%
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
                    Chapter == "Saint Michaels" ~ "St. Michaels",
                    Chapter == "Sweet Water" ~ "Sweetwater",
                    Chapter == "Tachee/Bluegap" ~ "Tachee",
                    Chapter == "Tsaile/Wheatfields" ~ "Tsaile-Wheatfields",
                    Chapter == "Whitehorse Lake" ~ "White Horse Lake",
                    Chapter == "Whiterock" ~ "White Rock",
                    .default = Chapter
                  )) %>%
  dplyr::distinct(Chapter, .keep_all = TRUE)

dine_agencies <-
  tigris::tribal_subdivisions_national(year = census_year,
                                       progress_bar = FALSE) %>%
  dplyr::filter(AIANNHCE == "2430") %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326) %>%
  dplyr::select(Chapter = NAME) %>%
  dplyr::mutate(Chapter =
                  dplyr::case_when(
                    Chapter == "San Juan Southern Paiute Northern" ~ "Navajo Mountain",
                    Chapter == "San Juan Southern Paiute Southern" ~ "Tuba City",
                    .default = Chapter
                  )) %>%
  dplyr::left_join(navajo_chapters, by = "Chapter") %>%
  dplyr::filter(!is.na(Agency)) %>%
  dplyr::group_by(Agency) %>%
  dplyr::summarise() %>%
  dplyr::transmute(land_id = paste0("2430R-", nr_slug(Agency)),
                   name = paste0("Navajo Nation — ", Agency, " Agency"),
                   GEOID = "2430R", AIANNHCE = "2430", COMPTYP = "R",
                   modeled = TRUE)

stopifnot("expected the five Navajo agencies" = nrow(dine_agencies) == 5)

# ---- Hawai‘i climate divisions ----
hawaii_climate_divisions <-
  sf::read_sf("HI_ClimateDivisions/ClimDiv12_polygon_clip.shp") %>%
  dplyr::transmute(`Division number` = as.integer(Id)) %>%
  dplyr::left_join(readr::read_csv("HI_ClimateDivisions.csv",
                                   show_col_types = FALSE),
                   by = "Division number") %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326) %>%
  dplyr::transmute(land_id = sprintf("HI-%02d", `Division number`),
                   name = paste0("Hawai‘i Climate Divisions — ", `Division name`),
                   GEOID = NA_character_, AIANNHCE = NA_character_,
                   COMPTYP = NA_character_, modeled = TRUE)

# ---- Assemble ----
native_land <-
  aiannh %>%
  dplyr::filter(land_id != "2430R") %>%
  dplyr::bind_rows(dine_agencies, hawaii_climate_divisions) %>%
  dplyr::mutate(name = stringr::str_replace_all(name, "/", " and "),
                census_year = as.integer(census_year)) %>%
  sf::st_cast("MULTIPOLYGON") %>%
  dplyr::arrange(name) %>%
  dplyr::mutate(geom_hash = st_geom_hash(.)) %>%
  dplyr::select(land_id, name, GEOID, AIANNHCE, COMPTYP, modeled,
                census_year, geom_hash)

stopifnot(
  "duplicate land_id" = !anyDuplicated(native_land$land_id),
  "duplicate names" = !anyDuplicated(native_land$name),
  "invalid geometries" = all(sf::st_is_valid(native_land)),
  "empty geometries" = !any(sf::st_is_empty(native_land)),
  "land_id must be partition-safe" = all(grepl("^[A-Za-z0-9-]+$", native_land$land_id))
)

message(nrow(native_land), " units (", sum(native_land$modeled), " modeled, ",
        sum(!native_land$modeled), " trust-land / home-land extraction only)")

unlink("native-land.parquet")
sf::write_sf(native_land, "native-land.parquet",
             driver = "Parquet",
             layer_options = c("COMPRESSION=BROTLI",
                               "GEOMETRY_ENCODING=GEOARROW",
                               "WRITE_COVERING_BBOX=NO"))

# Old (2024) archive name -> new unit, for retiring or redirecting old keys.
old_names_file <- file.path("_state", "native-land-2024-names.txt")
if (file.exists(old_names_file)) {
  old <- readLines(old_names_file, encoding = "UTF-8")
  crosswalk <-
    tibble::tibble(old_name = old) %>%
    dplyr::left_join(sf::st_drop_geometry(native_land) %>%
                       dplyr::select(land_id, name),
                     by = c(old_name = "name")) %>%
    dplyr::mutate(land_id = dplyr::case_when(
      is.na(land_id) & old_name == "Eastern Cherokee Reservation" ~
        native_land$land_id[native_land$name == "Qualla Boundary"],
      .default = land_id))
  readr::write_csv(crosswalk, file.path("_state", "native-land-crosswalk.csv"))
  unmatched <- crosswalk$old_name[is.na(crosswalk$land_id)]
  if (length(unmatched))
    message("Old names with no successor: ", paste(unmatched, collapse = "; "))
}

if (nr_publish()) {
  s3_preflight()
  s3_put(s3_bucket_name, paste0(s3_prefix, "/native-land.parquet"),
         "native-land.parquet",
         content_type = "application/vnd.apache.parquet",
         cache_control = "max-age=3600")
}
