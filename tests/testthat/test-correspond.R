somedata <- tibble(id = c(1,2,3), value = c(4,5,6))
correspondence <- tibble(oldid = c(1,2,2,3), newid = c(4,4,5,5), weight = c(1, 0.3, 0.7, 1))
should_be <- tibble(newid = c(4,5), value = c(5.5, 9.5))

expect_that(correspond(somedata, correspondence, "id", "oldid", "newid", "weight"), is_identical_to(should_be))

# Check that it repsects grouping
groupdata <- tibble(id = c(1,2,3,1,2,3), year = c(2000, 2000, 2000, 2001, 2001, 2001), value = c(4,5,6,7,8,9))
groupdata <- group_by(groupdata, year)
should_be <- tibble(year = c(2000,2000,2001,2001), newid = c(4,5,4,5), value = c(5.5,9.5,9.4,14.6))
should_be <- group_by(should_be, year)
expect_that(correspond(groupdata, correspondence, "id", "oldid", "newid", "weight"), is_identical_to(should_be))
