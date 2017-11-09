trysfintersection <- function(geo1, geo2) {
  tryCatch({
    inter <- sf::st_intersection(geo1, geo2)
    return(inter)
  },
  error = function(cond) {
    warning(paste("Intersection failed: ", conditionMessage(cond), "\nAdding a small buffer to geo2 and reattempting.", sep = ""))
    # need to recalculate the area, incorporating the buffer
    geo2$area_2 <- as.numeric(sf::st_area(geo2))
    inter <- sf::st_intersection(geo1, sf::st_buffer(geo2, BUFFERMARGIN))
    return(inter)
  }
  )
}

intersection <- function(geo1, geo2, method) {
  if (method == 1) {
    inter <- trysfintersection(geo1, geo2)
    return(inter)
  } else if (method == 2) {
    inter <- raster::intersect(methods::as(geo1, "Spatial"), methods::as(geo2, "Spatial"))
    if (!is.null(inter)) {
      return(sf::st_as_sf(inter))
    } else {
      return(geo1[FALSE,]) # Return an empty SF collection, which then gets ignored in overlaps
    }
  } else {
    stop(paste("Unknown method ", method, sep = ""))
  }
}

noteintersectionmethod <- function(method) {
  if (method == 1) {
    message("Using sf::st_intersection to calculate geographic intersections.")
  } else if (method == 2) {
    message("Using raster::intersect to calculate geographic intersections.")
  } else {
    stop(paste("Unknown intersection method ", method, sep = ""))
  }
}
