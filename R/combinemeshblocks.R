loadAUSmb <- function(MBcode = "MB_CODE11") {
  message("Loading mesh blocks...")
  ACT <- sf::st_read(system.file("extdata", "meshblocks/MB_2011_ACT.shp", package = "absgeoutils"), type = 6)
  NSW <- sf::st_read(system.file("extdata", "meshblocks/MB_2011_NSW.shp", package = "absgeoutils"), type = 6)
  NT <- sf::st_read(system.file("extdata", "meshblocks/MB_2011_NT.shp", package = "absgeoutils"), type = 6)
  OT <- sf::st_read(system.file("extdata", "meshblocks/MB_2011_OT.shp", package = "absgeoutils"), type = 6)
  QLD <- sf::st_read(system.file("extdata", "meshblocks/MB_2011_QLD.shp", package = "absgeoutils"), type = 6)
  SA <- sf::st_read(system.file("extdata", "meshblocks/MB_2011_SA.shp", package = "absgeoutils"), type = 6)
  TAS <- sf::st_read(system.file("extdata", "meshblocks/MB_2011_TAS.shp", package = "absgeoutils"), type = 6)
  VIC <- sf::st_read(system.file("extdata", "meshblocks/MB_2011_VIC.shp", package = "absgeoutils"), type = 6)
  WA <- sf::st_read(system.file("extdata", "meshblocks/MB_2011_WA.shp", package = "absgeoutils"), type = 6)
  message("Loading mesh blocks...done.")

  message("Merging mesh blocks...", appendLF = FALSE)
  AUS <- rbind(ACT, NSW, NT, OT, QLD, SA, TAS, VIC, WA)
  message("done.")

  # Read in population counts
  message("Adding population counts...", appendLF = FALSE)
  pop <- read.csv(system.file("extdata", "meshblocks/censuscounts_mb_2011_aust.csv", package = "absgeoutils"), header = TRUE)
  AUS <- merge(AUS, pop, by.x = MBcode, by.y = "Mesh_Block_ID", all.x = TRUE)
  message("done.")

  # drop empty geographies
  AUS <- AUS[!is.na(sf::st_dimension(AUS)),]
  return(AUS)
}
