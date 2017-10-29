rebase <- function(correspondence, from.id) {
  codes <- unique(correspondence[, from.id])
  for (code in codes) {
    match <- correspondence[, from.id] == code
    totalweight <- sum(correspondence$weight[match])
    correspondence$weight[match] <- correspondence$weight[match] / totalweight
  }
  return(correspondence)
}
