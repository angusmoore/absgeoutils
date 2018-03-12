areacorrespondence.calculate <- function(from, to, from.ID, to.ID) {
  from <- dplyr::select_(from, from.ID)
  from <- dplyr::rename_(from, .dots = stats::setNames(from.ID, "from"))

  to <- dplyr::select_(to, to.ID)
  to <- dplyr::rename_(to, .dots = stats::setNames(to.ID, "to"))

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
  correspondence$weight <- correspondence$overlap.area / correspondence$area_1

  # Collapse on from.ID and to.ID, ignore small weights
  correspondence <- dplyr::group_by_(correspondence, "from", "to")
  correspondence <- dplyr::summarise(correspondence, weight = sum(weight, na.rm = TRUE))
  correspondence <- correspondence[correspondence$weight > CORRESPONDENCETOLERANCE, ]

  # Ensure weights sum to 1 (may not, because we ignored small intersections)
  tweight <- dplyr::summarise(correspondence, totalweight = sum(weight, na.rm = TRUE))
  correspondence <- dplyr::left_join(correspondence, tweight)
  correspondence$weight <- correspondence$weight / correspondence$totalweight
  correspondence <- dplyr::select_(correspondence, "-totalweight")

  # Rename the codes
  correspondence <- dplyr::rename_(correspondence, .dots = stats::setNames("from", from.ID))
  correspondence <- dplyr::rename_(correspondence, .dots = stats::setNames("to", to.ID))

  return(correspondence)
}

#' Construct an area-weighted correspondence between two digital boundary files
#'
#' @param from The simple feature geography you wish to correspond from
#' @param to The simple feature geography you wish to correspond to
#' @param from.ID The column name that identifies the areas in the from geography
#' @param to.ID The column name that identifies the areas in the to geography
#' @param filename (optional) A csv filename to write the correspondence to when done
#'
#' @examples
#' \dontrun{
#'   ASGS11_SA2 <- read_sf("extdata/ASGS2011/SA2_2011_AUST.shp")
#'   ASGC06_SLA <- read_sf("extdata/ASGC2006/SLA06aAUST.shp")
#'   areacorrespondence(ASGC06_SLA, ASGS11_SA2, "SLA_CODE06", "SA2_MAIN11")
#' }
#'
#' @export
areacorrespondence <- function(from, to, from.ID, to.ID, filename = NULL) {
  correspondence <- areacorrespondence.calculate(from, to, from.ID, to.ID)
  missingcodes <- missingcodes(strip_geography(from), correspondence, from.ID)
  if (sum(missingcodes > 0)) {
    warning(paste("There are ", sum(missingcodes), " missing from the correspondence.", sep = ""))
  }

  # Write to csv if desired
  if (!is.null(filename)) {
    utils::write.csv(correspondence, file = filename, row.names = FALSE)
  }
  return(correspondence)
}
