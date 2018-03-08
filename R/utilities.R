strip_geography <- function(x) {
  sf::st_geometry(x) <- NULL
  return(x)
}

iscontained <- function(code, correspondence, id) {
  return(code %in% correspondence[[id]])
}

missingcodes <- function(original, new, id) {
  codes <- unique(original[[id]])
  return(!sapply(codes, FUN = iscontained, correspondence = new, id = id))
}

findoverlaps <- function(geo1, geo2) {
  geo1$area_1 <- as.numeric(sf::st_area(geo1))
  result <- sf::st_intersection(sf::st_buffer(geo1, 0), sf::st_buffer(geo2, 0)) # The buffer fixes occasional problems with non-valid geometry intersections
  if (!all(sf::st_is(result, "POLYGON") | sf::st_is(result, "MULTIPOLYGON"))) {
    result <- sf::st_collection_extract(result, "POLYGON")
  }
  result$overlap.area <- units::drop_units(sf::st_area(result))
  result <- result[result$overlap.area >= 1, ] # Ignore any areas with area less than 1m^2. These will just be inaccuracies
  result <- tibble::as_tibble(strip_geography(result))
  return(result)
}

only2d <- function(x) {
  dims <- sf::st_dimension(x)
  x <- x[!is.na(dims), ]
  dims <- dims[!is.na(dims)]
  return(x[dims == 2, ])
}
