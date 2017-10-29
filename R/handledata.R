mergeSAdata <- function(spatialdata, data, data.by, geo.by) {
  return(merge(spatialdata, data, by.x = geo.by, by.y = data.by, all.x = TRUE))
}
