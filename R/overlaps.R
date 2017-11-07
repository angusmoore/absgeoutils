findoverlaps <- function(geo1, geo2, method) {
  geo1$area_1 <- as.numeric(sf::st_area(geo1))

  overlaps <- intersection(geo1, geo2, method)
  nonpolygon <- 0
  
  pb <- progress::progress_bar$new(total = nrow(geo1))
  
  for (i in 1:nrow(geo1)) {
    pb$tick()
    
    result <- intersection(geo1[i, ], geo2, method)
    
    # Filter to only area-valid geometries (e.g. ignore MULTILINESTRINGs etc formed from boundary, but not area, overlaps)
    nonpolygon <- nonpolygon + sum(!sf::st_is(result, c("POLYGON", "MULTIPOLYGON")))
    result <- result[sf::st_is(result, c("POLYGON", "MULTIPOLYGON")), ]
    # check validity
    if (!all(sf::st_is_valid(result))){
      result <- result[sf::st_is_valid(result), ]
    }
    # calculate area
    result$overlap.area <- as.numeric(sf::st_area(result))
    
    # Remove geography (helps keep memory usage down)
    sf::st_geometry(result) <- NULL
    
    if (i == 1) {
      overlaps <- result
    } else {
      overlaps <- rbind(overlaps, result)
    }
  }
  
  if (nonpolygon > 0) {
    message(paste(nonpolygon, " non-polygon geometries were formed.", sep = ""))
  }

  return(overlaps)
}
