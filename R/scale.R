testscaleoptions <- function(significand, minval, maxval, permittedsteps) {
  # Now we try the different combinations of and see which works best
  jointdeviation <- matrix(NA, length(PERMITTEDLABELS), length(permittedsteps))

  for (i in 1:length(PERMITTEDLABELS)) {
    step <- PERMITTEDLABELS[i]*10^significand
    minscale <- plyr::round_any(minval, step, f = floor)

    # This bit could be better. There's surely an algebraic way I can determine the best option here
    for (j in 1:length(permittedsteps)) {
      maxscale <- ((permittedsteps[j]-1)*step + minscale)
      if (maxscale > maxval) {
        jointdeviation[i,j] <- (minval - minscale) + (maxscale - maxval)
      } else {
        jointdeviation[i,j] <- NA
      }
    }
  }
  return(jointdeviation)
}

defaultscale <- function(data,permittedsteps=PERMITTEDSTEPS) {

  maxval <- max(data,na.rm=TRUE)
  minval <- min(data,na.rm=TRUE)
  span <- maxval-minval

  significand <- floor(log10(span/min(permittedsteps)))
  if (is.na(significand) || significand == Inf || significand == -Inf) {
    # Happens if span is zero, possibly other cases
    significand <- 0
  }

  jointdeviation <- testscaleoptions(significand, minval, maxval, permittedsteps)
  # Check here if none are feasible
  if (all(is.na(jointdeviation))) {
    # Try a larger significand
    significand <- significand + 1
    jointdeviation <- testscaleoptions(significand, minval, maxval, permittedsteps)
    if (all(is.na(jointdeviation))) {
      message("No feasible scale found, defaulting to something bad.")
      return(list("min" = minval, "max" = maxval, "nsteps" = 5))
    }
  }

  ideal <- which(jointdeviation == min(jointdeviation,na.rm=TRUE), arr.ind=TRUE)
  if (!is.null(nrow(ideal))) {
    ideal <- ideal[1, ] # Just take the first match
  }
  step <- PERMITTEDLABELS[ideal[1]]*10^significand
  nsteps <- permittedsteps[ideal[2]]
  minscale <- plyr::round_any(minval, step, f = floor)
  maxscale <- minscale + (nsteps-1)*step
  return(list("min" = minscale, "max" = maxscale, "nsteps" = nsteps))
}

createscale <- function(minscale, maxscale, nsteps, sigdig=2) {
  scale <- rep(NA,nsteps)
  stepsize <- (maxscale-minscale)/(nsteps-1)
  for (i in 1:nsteps) {
    scale[i] <- signif(minscale + (i-1)*stepsize,sigdig)
  }
  return(scale)
}
