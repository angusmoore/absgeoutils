library(sf)
library(magrittr)

geo1 <- c("POLYGON((0 0,0 1,2 1,2 0,0 0))",
          "POLYGON((2 0,2 1,4 1,4 0,2 0))") %>%
  st_as_sfc() %>%
  st_sf(id1 = c(1,2), geoms = .)

geo2 <- c("POLYGON((0 0,0 1,1 1,1 0,0 0))",
          "POLYGON((1 0,1 1,3 1,3 0,1 0))",
          "POLYGON((3 0,3 1,4 1,4 0,3 0))") %>%
  st_as_sfc() %>%
  st_sf(id2 = c(1,2,3), geoms = .)

fakeMB <- c("POLYGON((0 0,0 1,1 1,1 0,0 0))",
            "POLYGON((1 0,1 1,2 1,2 0,1 0))",
            "POLYGON((2 0,2 1,3 1,3 0,2 0))",
            "POLYGON((3 0,3 1,4 1,4 0,3 0))") %>%
  st_as_sfc() %>%
  st_sf(MBID = c(11,12,13,14), geoms = .)

fakepops <- data.frame(MBID = c(11,12,13,14), Persons_Usually_Resident = c(20,5,50,10))
fakeMB <- merge(fakeMB, fakepops)

one_to_two <- data.frame(id1 = c(1,1,2,2), id2 = c(1,2,2,3), weight = c(0.5,0.5,0.5,0.5))
two_to_one <- data.frame(id2 = c(1,2,2,3), id1 = c(1,1,2,2), weight = c(1,0.5,0.5,1))

test_that(areacorrespondence(geo1, geo2, "id1", "id2"), equals(one_to_two))
test_that(areacorrespondence(geo2, geo1, "id2", "id1"), equals(two_to_one))

one_to_two <- data.frame(id1 = c(1,1,2,2), id2 = c(1,2,2,3), weight = c(20/25,5/25,50/60,10/60))
two_to_one <- data.frame(id2 = c(1,2,2,3), id1 = c(1,1,2,2), weight = c(20/20,5/55,50/55,10/10))

test_that(populationcorrespondence(geo1, geo2, "id1", "id2", MB = fakeMB, URcode = "Persons_Usually_Resident"), equals(one_to_two))
test_that(populationcorrespondence(geo2, geo1, "id2", "id1", MB = fakeMB, URcode = "Persons_Usually_Resident"), equals(two_to_one))
