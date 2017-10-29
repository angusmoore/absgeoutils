getASGScode <- function(SAlevel,ASGS) {
  if (SAlevel == 'SA1') {
    code <- 'SA1_MAIN'
  } else if (SAlevel == 'SA2') {
    code <- 'SA2_MAIN'
  } else if (SAlevel == 'SA3') {
    code <- 'SA3_CODE'
  } else if (SAlevel == 'SA4') {
    code <- 'SA4_CODE'
  } else if (SAlevel == 'GCCSA') {
    code <- 'GCC_CODE'
  } else if (SAlevel == 'STE') {
    code <- 'STE_CODE'
  } else if (SAlevel == 'LGA') {
    code <- 'LGA_CODE'
  } else {
    stop(paste('Unsupported region definition ', SAlevel, '. Valid options are SA1, SA2, SA3, SA4, GCCSA, STE.'))
  }

  if (ASGS==2011 || ASGS=='2011') {
    code <- paste(code,'11',sep='')
  } else if (ASGS==2016 || ASGS=='2016') {
    code <- paste(code,'16',sep='')
  } else {
    stop(paste('Unsupported ASGS version ', ASGS, '. Only 2011 and 2016 are supported.'))
  }
}
