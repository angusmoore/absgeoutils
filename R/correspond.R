correspond.column <- function(correspondence, data, data.by, data.y, correspondence.from = "from", correspondence.to = "to", correspondence.weight = "weight", na.rm = TRUE) {
  tolist <- sort(unique(correspondence[, correspondence.to]))

  out <- data.frame(code = tolist)
  out[, data.y] <- 0
  hit <- out

  for (to in tolist) {
    r <- out$code == to
    froms <- correspondence[correspondence[, correspondence.to] == to, ]

    for (from in froms[, correspondence.from]) {
      datafrom <- data[data[,data.by] == from, data.y]
      if (length(datafrom) > 1) {
        stop(paste(from, " had more than one entry in the original data.", sep = ""))
      } else if (length(datafrom) == 1 && !is.na(datafrom)) {
        out[r, data.y] <- out[r, data.y] + datafrom * froms[froms[, correspondence.from] == from, correspondence.weight]
        hit[r, data.y] <- 1
      } else if (!na.rm) {
        out[r, data.y] <- NA
        hit[r, data.y] <- 1
      }
    }
  }
  # Anything that didn't get hit, set to NA
  out[hit==0] <- NA
  return(out)
}

sanitycheck <- function(data, varlist) {
  for (var in varlist) {
    if (!var %in% colnames(data)) {
      stop(paste(var, " is not in data.", sep = ""))
    }
  }
}

correspond <- function(correspondence, data, varlist = colnames(data), data.by, correspondence.from = "from", correspondence.to = "to", correspondence.weight = "weight", na.rm = TRUE) {
  sanitycheck(data, varlist)
  varlist <- varlist[varlist != data.by]
  tolist <- sort(unique(correspondence[, correspondence.to]))
  out <- data.frame(code = tolist)

  for (var in varlist) {
    message(var)
    out <- merge(out, correspond.column(correspondence, data, data.by, var, correspondence.from, correspondence.to, correspondence.weight, na.rm))
  }
  return(out)
}
