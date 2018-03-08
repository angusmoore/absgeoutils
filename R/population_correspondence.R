populationcorrespondence.calculate <- function(from, to, from.ID, to.ID, MB, URcode) {
  from <- dplyr::select_(from, from.ID)
  from <- dplyr::rename_(from, .dots = stats::setNames(from.ID, "from"))

  to <- dplyr::select_(to, to.ID)
  to <- dplyr::rename_(to, .dots = stats::setNames(to.ID, "to"))

  MB <- dplyr::select_(MB, URcode)
  MB <- dplyr::rename_(MB,.dots = stats::setNames(URcode, "UR"))

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
  correspondence$UR <- correspondence$UR * correspondence$overlap.area / correspondence$area_1
  correspondence <- dplyr::select_(correspondence, "-area_1")

  # Collapse on from.ID to get population for the from.IDs
  frompop <- dplyr::group_by_(correspondence, "from")
  frompop <- dplyr::summarise(frompop, frompop = sum(UR, na.rm = TRUE))
  frompop$frompop <- ifelse(frompop$frompop < POPULATIONTHRESHOLD, 0, frompop$frompop) # If population is too small, do by area instead

  # Use that, and the populations in the correspondence geography to get weights
  correspondence <-  dplyr::left_join(correspondence, frompop)
  correspondence$weight <-  correspondence$UR / correspondence$frompop

  # What if from pop is zero? Do the correspondence on area
  nopop <- dplyr::filter(correspondence, frompop == 0)
  nopop <- unname(unlist(dplyr::select(nopop, from)))
  nopopareas <-  dplyr::filter(from, from %in% nopop)
  nopopareas$from_area <- units::drop_units(sf::st_area(nopopareas))
  sf::st_geometry(nopopareas) <- NULL
  correspondence <- dplyr::left_join(correspondence, nopopareas)
  correspondence$weight <- ifelse(correspondence$frompop == 0, correspondence$overlap.area / correspondence$from_area, correspondence$weight)
  correspondence <- dplyr::select_(correspondence, "-from_area")

    # Collapse on from.ID and to.ID, ignore small weights
  correspondence <- dplyr::group_by_(correspondence, "from", "to")
  correspondence <- dplyr::summarise(correspondence, weight = sum(weight, na.rm = TRUE))
  correspondnece <- correspondence[correspondence$weight > CORRESPONDENCETOLERANCE, ]

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

#' Construct a population-weighted correspondence betwene two digital boundary files
#'
#' Population weights are determined from Census Mesh Block files (which you supply)
#'
#' @param from The simple feature geography you wish to correspond from
#' @param to The simple feature geography you wish to correspond to
#' @param from.ID The column name that identifies the areas in the from geography
#' @param to.ID The column name that identifies the areas in the to geography
#' @param MB A (whole of Australia) mesh block simple feature geography
#' @param URcode The column name in MB that has the persons usually resident count
#' @param filename (optional) A csv filename to write the correspondence to when done
#'
#' @examples
#' \dontrun{
#'   NSW <- read_sf("extdata/ASGC2006/MB_NSW_2006_census.shp")
#'   VIC <- read_sf("extdata/ASGC2006/MB_Vic_2006_census.shp")
#'   QLD <- read_sf("extdata/ASGC2006/MB_Qld_2006_census.shp")
#'   SA <- read_sf("extdata/ASGC2006/MB_SA_2006_census.shp")
#'   WA <- read_sf("extdata/ASGC2006/MB_WA_2006_census.shp")
#'   TAS <- read_sf("extdata/ASGC2006/MB_Tas_2006_census.shp")
#'   NT <- read_sf("extdata/ASGC2006/MB_NT_2006_census.shp")
#'   ACT <- read_sf("extdata/ASGC2006/MB_ACT_2006_census.shp")
#'   OT <- read_sf("extdata/ASGC2006/MB_OT_2006_census.shp")
#'   AUST_MB <- rbind(NSW, VIC, QLD, SA, WA, TAS, NT, ACT, OT)
#'   rm(list = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "OT"))
#'   ASGS11_SA2 <- read_sf("extdata/ASGS2011/SA2_2011_AUST.shp")
#'   ASGC06_SLA <- read_sf("extdata/ASGC2006/SLA06aAUST.shp")
#'   populationcorrespondence(ASGC06_SLA, ASGS11_SA2, "SLA_CODE06", "SA2_MAIN11", AUST_MB, "TURPOP2006")
#' }
#'
#' @export
populationcorrespondence <- function(from, to, from.ID, to.ID, MB, URcode = "Persons_Usually_Resident", filename = NULL) {
  correspondence <- populationcorrespondence.calculate(from, to, from.ID, to.ID, MB, URcode)
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
