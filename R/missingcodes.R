iscontained <- function(code, correspondence, id) {
  return(code %in% correspondence[, id])
}

missingcodes <- function(original, new, id) {
  codes <- unique(original[, id])
  sf::st_geometry(codes) <- NULL
  return(!sapply(codes, FUN = iscontained, correspondence = new, id = id))
}
