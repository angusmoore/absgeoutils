areacorrespondence <- function(from, to, from.ID, to.ID, filename = NULL, method = 2) {

  overlaps <- findoverlaps(from, to, method)
  sf::st_geometry(overlaps) <- NULL;

  correspondence <- data.frame(from = overlaps[, from.ID], to = overlaps[, to.ID], weight = overlaps$overlap.area/overlaps$area_1)
  colnames(correspondence) <- c(from.ID, to.ID, "weight")

  # Drop any weights below 1e-10 - these are likely just small inaccuracies in the boundaries
  correspondence <- correspondence[correspondence$weight > 1e-5, ]

  # Write to csv if desired
  if (!is.null(filename)) {
    write.csv(correspondence, file = filename, row.names = FALSE)
  }

  return(correspondence)
}
