handleregion <- function(region) {
  # This switch overrides the xlim and ylim with coordinates for Australian cities
  if (region == 'SydneyCity') {
    coord <- CITYCOORDS['Sydney']
    xlim <- c(coord[1]-0.2,coord[1]+0.2)
    ylim <- c(coord[2]-0.2,coord[2]+0.2)
  } else if (region == 'MelbourneCity') {
    coord <- CITYCOORDS['Melbourne']
    xlim <- c(coord[1]-0.2,coord[1]+0.2)
    ylim <- c(coord[2]-0.2,coord[2]+0.2)
  } else if (region == 'BrisbaneCity') {
    coord <- CITYCOORDS['Brisbane']
    xlim <- c(coord[1]-0.2,coord[1]+0.2)
    ylim <- c(coord[2]-0.2,coord[2]+0.2)
  } else if (region == 'PerthCity') {
    coord <- CITYCOORDS['Perth']
    xlim <- c(coord[1]-0.2,coord[1]+0.2)
    ylim <- c(coord[2]-0.2,coord[2]+0.2)
  } else if (region == 'AdelaideCity') {
    coord <- CITYCOORDS['Adelaide']
    xlim <- c(coord[1]-0.2,coord[1]+0.2)
    ylim <- c(coord[2]-0.2,coord[2]+0.2)
  } else if (region == 'HobartCity') {
    coord <- CITYCOORDS['Hobart']
    xlim <- c(coord[1]-0.2,coord[1]+0.2)
    ylim <- c(coord[2]-0.2,coord[2]+0.2)
  } else if (region == 'CanberraCity') {
    coord <- CITYCOORDS['Canberra']
    xlim <- c(coord[1]-0.2,coord[1]+0.2)
    ylim <- c(coord[2]-0.2,coord[2]+0.2)
  } else if (region == 'DarwinCity') {
    coord <- CITYCOORDS['Darwin']
    xlim <- c(coord[1]-0.2,coord[1]+0.2)
    ylim <- c(coord[2]-0.2,coord[2]+0.2)


  } else if (region == 'GreaterSydney') {
    coord <- CITYCOORDS['Sydney']
    xlim <- c(coord[1]-0.6,coord[1]+0.6)
    ylim <- c(coord[2]-0.6,coord[2]+0.6)
  } else if (region == 'GreaterMelbourne') {
    coord <- CITYCOORDS['Melbourne']
    xlim <- c(coord[1]-0.6,coord[1]+0.6)
    ylim <- c(coord[2]-0.6,coord[2]+0.6)
  } else if (region == 'GreaterBrisbane') {
    coord <- CITYCOORDS['Brisbane']
    xlim <- c(coord[1]-0.6,coord[1]+0.6)
    ylim <- c(coord[2]-0.6,coord[2]+0.6)
  } else if (region == 'GreaterPerth') {
    coord <- CITYCOORDS['Perth']
    xlim <- c(coord[1]-0.6,coord[1]+0.6)
    ylim <- c(coord[2]-0.6,coord[2]+0.6)
  } else if (region == 'GreaterAdelaide') {
    coord <- CITYCOORDS['Adelaide']
    xlim <- c(coord[1]-0.6,coord[1]+0.6)
    ylim <- c(coord[2]-0.6,coord[2]+0.6)
  } else if (region == 'GreaterHobart') {
    coord <- CITYCOORDS['Hobart']
    xlim <- c(coord[1]-0.6,coord[1]+0.6)
    ylim <- c(coord[2]-0.6,coord[2]+0.6)
  } else if (region == 'GreaterCanberra') {
    coord <- CITYCOORDS['Canberra']
    xlim <- c(coord[1]-0.6,coord[1]+0.6)
    ylim <- c(coord[2]-0.6,coord[2]+0.6)
  } else if (region == 'GreaterDarwin') {
    coord <- CITYCOORDS['Darwin']
    xlim <- c(coord[1]-0.6,coord[1]+0.6)
    ylim <- c(coord[2]-0.6,coord[2]+0.6)


  } else if (region == 'Australia') {
    xlim <- c(111,152)
    ylim <- c(-49,-6)
  } else {
    stop(paste('Unknown region: ', region))
  }

  return(list("xlim" = xlim, "ylim" = ylim))
}
