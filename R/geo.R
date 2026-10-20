# geo.R — geometry helpers.

# Shift longitudes to 0–360 so features line up with the NEX-GDDP-CMIP6 grid,
# splitting anything that straddles the antimeridian (the Aleutians).
st_rotate <- function(x) {
  x2 <- (sf::st_geometry(x) + c(360, 90)) %% c(360) - c(0, 90)
  x3 <- sf::st_wrap_dateline(sf::st_set_crs(x2 - c(180, 0), 4326)) + c(180, 0)
  x4 <- sf::st_set_crs(x3, 4326)
  sf::st_set_geometry(x, x4)
}

# Stable per-feature fingerprint of the geometry (WKB, 7 dp in WGS84), used to
# decide whether a land must be re-extracted.
st_geom_hash <- function(x) {
  g <- sf::st_geometry(sf::st_transform(x, 4326))
  g <- sf::st_set_precision(g, 1e7)
  vapply(sf::st_as_binary(g, precision = 1e7),
         \(b) digest::digest(b, algo = "xxhash64", serialize = FALSE),
         character(1))
}

# ASCII slug for file names and partition values.
nr_slug <- function(x) {
  x <- stringi::stri_trans_general(x, "Any-Latin; Latin-ASCII")
  x <- tolower(gsub("[^A-Za-z0-9]+", "-", x))
  gsub("(^-|-$)", "", x)
}

# Read native-land.parquet. sf's Parquet driver needs a GDAL built with
# Arrow, which some images (e.g. rocker/geospatial) lack; the file stores
# WKB geometry, so arrow alone can read it there.
nr_read_lands <- function(file = "native-land.parquet") {
  if ("Parquet" %in% sf::st_drivers()$name)
    return(sf::read_sf(file))
  x <- arrow::read_parquet(file)
  geom <- sf::st_as_sfc(structure(as.list(x$geometry), class = "WKB"),
                        crs = 4326)
  sf::st_sf(tibble::as_tibble(x[setdiff(names(x), "geometry")]),
            geometry = geom)
}
