removenull <- function(geo) {

  return(geo)
}

loadASGS <- function(SAlevel, ASGS) {
  # This is just to run a sanity check on SAlevel and ASGS; it errors out if invalid.
  getASGScode(SAlevel,ASGS)
  if (SAlevel == 'SA1') {
    if (ASGS == 2011 || ASGS == '2011') {
      geo <- sf::st_read(system.file("extdata", 'ASGS2011/SA1/SA1_2011_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    } else {
      geo <- sf::st_read(system.file("extdata", 'ASGS2016/SA1/SA1_2016_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    }
  } else if (SAlevel == 'SA2') {
    if (ASGS == 2011 || ASGS == '2011') {
      geo <- sf::st_read(system.file("extdata", 'ASGS2011/SA2/SA2_2011_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    } else {
      geo <- sf::st_read(system.file("extdata", 'ASGS2016/SA2/SA2_2016_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    }
  } else if (SAlevel == 'SA3') {
    if (ASGS == 2011 || ASGS == '2011') {
      geo <- sf::st_read(system.file("extdata", 'ASGS2011/SA3/SA3_2011_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    } else {
      geo <- sf::st_read(system.file("extdata", 'ASGS2016/SA3/SA3_2016_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    }
  } else if (SAlevel == 'SA4') {
    if (ASGS == 2011 || ASGS == '2011') {
      geo <- sf::st_read(system.file("extdata", 'ASGS2011/SA4/SA4_2011_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    } else {
      geo <- sf::st_read(system.file("extdata", 'ASGS2016/SA4/SA4_2016_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    }
  } else if (SAlevel == 'LGA') {
    if (ASGS == 2011 || ASGS == '2011') {
      geo <- sf::st_read(system.file("extdata", 'ASGS2011/LGA/LGA_2011_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    } else {
      geo <- sf::st_read(system.file("extdata", 'ASGS2016/LGA/LGA_2016_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    }
  } else if (SAlevel == 'GCCSA') {
    if (ASGS == 2011 || ASGS == '2011') {
      geo <- sf::st_read(system.file("extdata", 'ASGS2011/GCCSA/GCCSA_2011_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    } else {
      geo <- sf::st_read(system.file("extdata", 'ASGS2016/GCCSA/GCCSA_2016_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    }
  } else if (SAlevel == 'STE') {
    if (ASGS == 2011 || ASGS == '2011') {
      geo <- sf::st_read(system.file("extdata", 'ASGS2011/STE/STE_2011_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    } else {
      geo <- sf::st_read(system.file("extdata", 'ASGS2016/STE/STE_2016_AUST.shp', package = "absgeoutils"))
      return(removenull(geo))
    }
  }
}
