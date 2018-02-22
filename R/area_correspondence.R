areacorrespondence.calculate <- function(from, to, from.ID, to.ID) {
  from <- from %>% dplyr::select_(from.ID) %>% dplyr::rename_(.dots = setNames(from.ID, "from"))
  to <- to %>% dplyr::select_(to.ID) %>% dplyr::rename_(.dots = setNames(to.ID, "to"))

  # Clean out any non-2d geographies
  from <- only2d(from)
  to <- only2d(to)

  # Create an intersection of the from and to geographies
  message(paste0("Finding intersections for the two supplied geographies...(started at ", Sys.time(), ")"))
  start.time <- Sys.time()
  correspondence <- findoverlaps(from, to)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  message(paste0("Finding intersections for the two supplied geographies...done. (Completed in ", round(time.taken,2), " ",  attr(time.taken, 'units'), ")"))

  # Calculate weights
  correspondence <- correspondence %>% dplyr::mutate(weight = overlap.area / area_1)

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

areacorrespondence <- function(from, to, from.ID, to.ID, filename = NULL) {
  correspondence <- areacorrespondence.calculate(from, to, from.ID, to.ID)
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
