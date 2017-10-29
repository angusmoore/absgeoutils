assigncolors <- function(data, datacol, scale, colpal, N, greyoutliers) {
  scalemax <- max(scale)
  scalemin <- min(scale)
  pad <- (scalemax-scalemin)/(2*N)
  cutpoints <- seq(from=(scalemin-pad), to=(scalemax+pad), length.out=N)
  cuts <- cut(as.data.frame(data)[, datacol], breaks=cutpoints, label=FALSE)
  exceed <- sum(as.data.frame(data)[, datacol] > scalemax, na.rm=TRUE)
  precede <- sum(as.data.frame(data)[, datacol] < scalemin, na.rm=TRUE)

  if (exceed > 0) {
    if (greyoutliers) {
      warning(paste(exceed, " data points exceeded the maximum scale. These have been set to NA (grey)."))
    } else {
      warning(paste(exceed, " data points exceeded the maximum scale. These have been set equal to the maximum scale value."))
      cuts[as.data.frame(data)[, datacol] > scalemax] <- N
    }
  }
  if (precede > 0) {
    if (greyoutliers) {
      warning(paste(precede, " data points were below the minimum scale. These have been set to NA (grey)."))
    } else {
      warning(paste(precede, " data points exceeded the minimum scale. These have been set equal to the minimum scale value."))
      cuts[as.data.frame(data)[, datacol] < scalemin] <- 1
    }
  }
  colorlist <- colpal(N)
  regioncolors <- colorlist[cuts]
  regioncolors[is.na(regioncolors)] <- "#808080"
  return(list("regioncolors"=regioncolors,"colorlist"=colorlist,"cutpoints"=cutpoints))
}

createheatmaplabels <- function(cutpoints, scale) {
  labels <- rep(NA,length(cutpoints))
  for (i in 1:length(scale)) {
    labels[which.min(abs(cutpoints-scale[i]))] <- scale[i]
  }
  return(labels)
}

handleheatmapscale <- function(scale, scalemin, scalemax, scalepoints, data) {
  if (length(scale) > 1 && all(!is.na(scale))) {
    # User-defined
    return(scale)
  } else if (!is.na(scalemin) && !is.na(scalemax) && !is.na(scalepoints)) {
    return(createscale(scalemin, scalemax, scalepoints))
  } else {
    scale <- defaultscale(data)
    return(createscale(scale$min, scale$max, scale$nsteps))
  }
}
