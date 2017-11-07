populationcorrespondence.calculate <- function(from, to, from.ID, to.ID, MB, MBcode, method) {
  # First, create an intersection of the from and to geographies
  message("Finding intersections for the two supplied geographies (this will take a little while)...", appendLF = FALSE)
  combined <- intersection(from, to, method)
  # Filter to only area-valid geometries (e.g. ignore MULTILINESTRINGs etc formed from boundary, but not area, overlaps)
  combined <- combined[sf::st_is(combined, c("POLYGON", "MULTIPOLYGON")), ]
  message("done.")

  # Then do a find overlap with the MB
  message(paste("Calculating mesh block overlaps (this is very slow)... (started at ", Sys.time(), ")", sep = ""))
  start.time <- Sys.time()
  MBfraction <- findoverlaps(MB, combined, method)
  sf::st_geometry(MBfraction) <- NULL
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  message(paste("Calculating mesh block overlaps (this is very slow)...done. (Completed in ", round(time.taken,2), " ",  attr(time.taken, 'units'), ")", sep = ""))

  # From that, construct pop counts for that very fine geography (assign MB's population to the sub-MB areas by area weights)
  MBfraction$Persons_Usually_Resident <- MBfraction$Persons_Usually_Resident * MBfraction$overlap.area/MBfraction$area_1

  # Collapse on from.ID to get population for the from.IDs
  from.pop <- aggregate(as.formula(paste("Persons_Usually_Resident ~ ", from.ID, sep = "")), data = MBfraction, FUN = sum)
  # Use that, and the populations in the MBfraction geography to get weights
  colnames(from.pop) <- c(from.ID, "fromPop")
  MBfraction <- merge(MBfraction, from.pop, by.x = from.ID, by.y = from.ID, all.x = TRUE)
  MBfraction$weight <- MBfraction$Persons_Usually_Resident / MBfraction$fromPop

  # Collapse on from.ID and to.ID
  correspondence <- aggregate(as.formula(paste("weight ~ " , from.ID , " + " , to.ID, sep = "")), data = MBfraction, FUN = sum)

  # Ignore small entries and ensure the weights sum to 1 for each from area
  correspondence <- correspondence[correspondence$weight > CORRESPONDENCETOLERANCE, ]
  correspondence <- rebase(correspondence, from.ID)

  return(correspondence)
}

populationcorrespondence <- function(from, to, from.ID, to.ID, filename = NULL, MBpath = paste(getwd(), "/meshblocks/", sep =""), MB = NULL, MBcode = "MB_CODE11", method = 2) {
  noteintersectionmethod(method)
  if (is.null(MB)) {
    MB <- loadAUSmb(MBpath, MBcode)
    # For some reason, the mesh blocks don't have a coordinate reference system, set to the same as the from geography (which whould be right)
    sf::st_crs(MB) <- sf::st_crs(from)
  }
  # Drop all but the required columns from MB (because otherwise we might wind up with duplicated colnames)
  for (name in colnames(MB)) {
    if (!(name %in% c(MBcode, "Persons_Usually_Resident", "geometry"))) {
      MB[, name] <- NULL
    }
  }

  correspondence <- populationcorrespondence.calculate(from, to, from.ID, to.ID, MB, MBcode, method)
  missingcodes <- missingcodes(from, correspondence, from.ID)
  if (sum(missingcodes > 0)) {
    warning(paste("There are ", sum(missingcodes), " missing from the correspondence.", sep = ""))
  }

  # Write to csv if desired
  if (!is.null(filename)) {
    write.csv(correspondence, file = filename, row.names = FALSE)
  }
  return(correspondence)
}
