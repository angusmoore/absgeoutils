loadAUSmb <- function(MBcode = "MB_CODE11") {
  message("Loading mesh blocks...")
  ACT <- sf::st_read("data-raw/meshblocks/MB_2011_ACT.shp", type = 6)
  NSW <- sf::st_read("data-raw/meshblocks/MB_2011_NSW.shp", type = 6)
  NT <- sf::st_read("data-raw/meshblocks/MB_2011_NT.shp", type = 6)
  OT <- sf::st_read("data-raw/meshblocks/MB_2011_OT.shp", type = 6)
  QLD <- sf::st_read("data-raw/meshblocks/MB_2011_QLD.shp", type = 6)
  SA <- sf::st_read("data-raw/meshblocks/MB_2011_SA.shp", type = 6)
  TAS <- sf::st_read("data-raw/meshblocks/MB_2011_TAS.shp", type = 6)
  VIC <- sf::st_read("data-raw/meshblocks/MB_2011_VIC.shp", type = 6)
  WA <- sf::st_read("data-raw/meshblocks/MB_2011_WA.shp", type = 6)
  message("Loading mesh blocks...done.")

  message("Merging mesh blocks...", appendLF = FALSE)
  AUS <- rbind(ACT, NSW, NT, OT, QLD, SA, TAS, VIC, WA)
  message("done.")

  # Read in population counts
  message("Adding population counts...", appendLF = FALSE)
  pop <- read.csv("data-raw/meshblocks/censuscounts_mb_2011_aust.csv", header = TRUE)
  AUS <- merge(AUS, pop, by.x = MBcode, by.y = "Mesh_Block_ID", all.x = TRUE)
  message("done.")

  # drop empty geographies
  AUS <- AUS[!is.na(sf::st_dimension(AUS)),]
  return(AUS)
}
