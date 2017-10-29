plotSAcolorlist <- function(spatialdata, colorlist, legendcolors, labels, xlim, ylim, ls, SAlbw) {
  if (ls == 0) {
    par(mar=c(0,0,0,0))
  } else if (ls == 1) {
    par(mar=c(0,4,0,0))
  } else if (ls == 2) {
    par(mar=c(0,0,0,4))
  } else {
    stop(paste('Invalid option for ls. Only 1 (left), 2 (right) or 0 (none) are allowed.'))
  }

  plot(sf::st_geometry(spatialdata), col=colorlist, xlim=xlim, ylim=ylim,border=SAlbw)
  if (ls != 0) {
    plotlegend(legendcolors, ls, labels)
  }
}

plotSAcolors <- function(SAlevel,data,datacol,data.by,labels,ASGS=2016,region=NA,xlim=c(111,152),ylim=c(-49,-6),ls=1,SAlbw=NA) {
  spatialdata <- loadshapefile(SAlevel,ASGS)

  if(!is.na(region)) {
    lims <- handleregion(region)
    xlim <- lims$xlim
    ylim <- lims$ylim
  }
  merged <- mergeSAdata(spatialdata@data,data,data.by,SAlevel,ASGS)

  plotSAcolorlist(spatialdata,merged[, datacol],merged[, datacol],labels,xlim,ylim,ls,SAlbw)
}

heatmapSA <- function(data, datacol, level, data.by = NULL, geo = NULL, geo.type = "ASGS", geo.version = 2016, colpal=colorRampPalette(c('red','yellow','darkgreen')),region=NA,xlim=c(111,152),ylim=c(-49,-6),SAlbw=NA,ls=1,scale=NA,scalemax=NA,scalemin=NA,scalepoints=NA,N=1000,greyoutliers=TRUE) {

  if (is.null(geo)) {
    # Haven't opted to pass our own geo, load one up
    if (geo.type == "ASGS") {
      geo <- loadASGS(level, geo.version)
    } else {
      stop(paste("Unknown geography type ", geo.type, ".", sep = ""))
    }
  }

  if (is.null(data.by)) {
    # Assume we're using the ABS region code in the data frame too
    if (geo.type == "ASGS") {
      data.by <- getASGScode(level, geo.version)
    }
  }

  if (geo.type == "ASGS") {
    geo.by <- getASGScode(level, geo.version)
  }

  if(!is.na(region)) {
    lims <- handleregion(region)
    xlim <- lims$xlim
    ylim <- lims$ylim
  }

  # Handle scales
  scale <- handleheatmapscale(scale, scalemin, scalemax, scalepoints, data[, datacol])

  # Merge the data
  merged <- merge(geo, data, by.x = geo.by, by.y = data.by, all.x = TRUE)

  # Assign colours to each SA
  colors <- assigncolors(merged, datacol, scale, colpal, N, greyoutliers)

  # Assign labels to the color list
  labels <- createheatmaplabels(colors$cutpoints, scale)

  # Plot it!
  plotSAcolorlist(merged, colors$regioncolors, colors$colorlist, labels, xlim, ylim, ls, SAlbw)
}


