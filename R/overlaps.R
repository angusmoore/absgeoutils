findoverlaps <- function(geo1, geo2, method) {
  geo1$area_1 <- as.numeric(sf::st_area(geo1))
  geo2$area_2 <- as.numeric(sf::st_area(geo2))

  overlaps <- intersection(geo1, geo2, method)

  # Fitler to only area-valid geometries (e.g. ignore MULTILINESTRINGs etc formed from boundary, but not area, overlaps)
  nonpolygon <- sum(!sf::st_is(overlaps, c("POLYGON", "MULTIPOLYGON")))
  if (nonpolygon > 0) {
    message(paste(nonpolygon, " non-polygon geometries were formed.", sep = ""))
  }

  overlaps <- overlaps[sf::st_is(overlaps, c("POLYGON", "MULTIPOLYGON")), ]
  # check validity
  if (!all(sf::st_is_valid(overlaps))){
    message("Some geographies from the intersection were invalid. Removing.")
    overlaps <- overlaps[sf::st_is_valid(overlaps), ]
  }
  overlaps$overlap.area <- as.numeric(sf::st_area(overlaps))

  return(overlaps)
}
