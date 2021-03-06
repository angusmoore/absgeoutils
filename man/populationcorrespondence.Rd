% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/population_correspondence.R
\name{populationcorrespondence}
\alias{populationcorrespondence}
\title{Construct a population-weighted correspondence betwene two digital boundary files}
\usage{
populationcorrespondence(from, to, from.ID, to.ID, MB,
  URcode = "Persons_Usually_Resident", filename = NULL)
}
\arguments{
\item{from}{The simple feature geography you wish to correspond from}

\item{to}{The simple feature geography you wish to correspond to}

\item{from.ID}{The column name that identifies the areas in the from geography}

\item{to.ID}{The column name that identifies the areas in the to geography}

\item{MB}{A (whole of Australia) mesh block simple feature geography}

\item{URcode}{The column name in MB that has the persons usually resident count}

\item{filename}{(optional) A csv filename to write the correspondence to when done}
}
\description{
Population weights are determined from Census Mesh Block files (which you supply)
}
\examples{
\dontrun{
  NSW <- read_sf("extdata/ASGC2006/MB_NSW_2006_census.shp")
  VIC <- read_sf("extdata/ASGC2006/MB_Vic_2006_census.shp")
  QLD <- read_sf("extdata/ASGC2006/MB_Qld_2006_census.shp")
  SA <- read_sf("extdata/ASGC2006/MB_SA_2006_census.shp")
  WA <- read_sf("extdata/ASGC2006/MB_WA_2006_census.shp")
  TAS <- read_sf("extdata/ASGC2006/MB_Tas_2006_census.shp")
  NT <- read_sf("extdata/ASGC2006/MB_NT_2006_census.shp")
  ACT <- read_sf("extdata/ASGC2006/MB_ACT_2006_census.shp")
  OT <- read_sf("extdata/ASGC2006/MB_OT_2006_census.shp")
  AUST_MB <- rbind(NSW, VIC, QLD, SA, WA, TAS, NT, ACT, OT)
  rm(list = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "OT"))
  ASGS11_SA2 <- read_sf("extdata/ASGS2011/SA2_2011_AUST.shp")
  ASGC06_SLA <- read_sf("extdata/ASGC2006/SLA06aAUST.shp")
  populationcorrespondence(ASGC06_SLA, ASGS11_SA2, "SLA_CODE06", "SA2_MAIN11", AUST_MB, "TURPOP2006")
}

}
