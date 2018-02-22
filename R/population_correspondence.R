populationcorrespondence.calculate <- function(from, to, from.ID, to.ID, MB, URcode) {
  from <- from %>% dplyr::select_(from.ID) %>% dplyr::rename_(.dots = setNames(from.ID, "from"))
  to <- to %>% dplyr::select_(to.ID) %>% dplyr::rename_(.dots = setNames(to.ID, "to"))
  MB <- MB %>% dplyr::select_(URcode) %>% dplyr::rename_(.dots = setNames(URcode, "UR"))

  # Clean out any non-2d geographies
  from <- only2d(from)
  to <- only2d(to)
  MB <- only2d(MB)

  # First, create an intersection of the from and to geographies
  message(paste0("Finding intersections for the two supplied geographies...(started at ", Sys.time(), ")"))
  start.time <- Sys.time()
  combined <- sf::st_intersection(from, to)
  if (!all(sf::st_is(combined, "POLYGON") | sf::st_is(combined, "MULTIPOLYGON"))) {
    combined <- sf::st_collection_extract(combined, "POLYGON")
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  message(paste0("Finding intersections for the two supplied geographies...done. (Completed in ", round(time.taken,2), " ",  attr(time.taken, 'units'), ")"))

  # Then do a find overlap with the MB
  message(paste0("Calculating mesh block overlaps... (started at ", Sys.time(), ")"))
  start.time <- Sys.time()
  correspondence <- findoverlaps(MB, combined)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  message(paste0("Calculating mesh block overlaps...done. (Completed in ", round(time.taken,2), " ",  attr(time.taken, 'units'), ")"))

  # From that, construct pop counts for that very fine geography (assign MB's population to the sub-MB areas by area weights)
  correspondence <- dplyr::mutate(correspondence, UR = UR * overlap.area / area_1) %>% dplyr::select(-area_1)

  # Collapse on from.ID to get population for the from.IDs
  frompop <- correspondence %>% dplyr::group_by(from) %>% dplyr::summarise(frompop = sum(UR, na.rm = TRUE))
  frompop <- frompop %>% dplyr::mutate(frompop = ifelse(frompop < POPULATIONTHRESHOLD, 0, frompop)) # If populatoin is too small, do by area instead

  # Use that, and the populations in the correspondence geography to get weights
  correspondence <- correspondence %>% dplyr::left_join(frompop) %>% dplyr::mutate(weight = UR / frompop)

  # What if from pop is zero? Do the correspondence on area
  nopop <- correspondence %>% dplyr::filter(frompop == 0) %>% dplyr::select(from) %>% unlist() %>% unname
  nopopareas <- from %>% dplyr::filter(from %in% nopop)
  nopopareas$from_area <- units::drop_units(sf::st_area(nopopareas))
  sf::st_geometry(nopopareas) <- NULL
  correspondence <- dplyr::left_join(correspondence, nopopareas) %>% dplyr::mutate(weight = ifelse(frompop == 0, overlap.area / from_area, weight)) %>% dplyr::select(-from_area)

    # Collapse on from.ID and to.ID, ignore small weights
  correspondence <- correspondence %>% dplyr::group_by(from, to) %>%
    dplyr::summarise(weight = sum(weight, na.rm = TRUE)) %>%
    dplyr::filter(weight > CORRESPONDENCETOLERANCE)

    # Ensure weights sum to 1 (may not, because we ignored small intersections)
  correspondence <- correspondence %>% dplyr::summarise(totalweight = sum(weight, na.rm = TRUE)) %>%
    dplyr::right_join(correspondence) %>% dplyr::mutate(weight= weight / totalweight) %>% dplyr::select(-totalweight)

  # Rename the codes
  correspondence <- correspondence %>% dplyr::rename_(.dots = setNames("from", from.ID))
  correspondence <- correspondence %>% dplyr::rename_(.dots = setNames("to", to.ID))

  return(correspondence)
}

populationcorrespondence <- function(from, to, from.ID, to.ID, MB, URcode = "Persons_Usually_Resident", filename = NULL) {
  correspondence <- populationcorrespondence.calculate(from, to, from.ID, to.ID, MB, URcode)
  missingcodes <- missingcodes(strip_geography(from), correspondence, from.ID)
  if (sum(missingcodes > 0)) {
    warning(paste("There are ", sum(missingcodes), " missing from the correspondence.", sep = ""))
  }

  # Write to csv if desired
  if (!is.null(filename)) {
    write.csv(correspondence, file = filename, row.names = FALSE)
  }
  return(correspondence)
}
