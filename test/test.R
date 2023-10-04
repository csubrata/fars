library(testthat)
library(fars)

# Test fars_read function to ensure it loads data into a dataframe
test_that("Confirm data loaded into a data frame", {
  fars_data <- fars_read("accident_2013.csv.bz2")
  expect_that(fars_data, is_a("data.frame"))

  fars_data <- fars_read("accident_2014.csv.bz2")
  expect_that(fars_data, is_a("data.frame"))

  fars_data <- fars_read("accident_2015.csv.bz2")
  expect_that(fars_data, is_a("data.frame"))
})

# Test error condition
test_that("Confirm 2016 data loading throws an error", {
  expect_error(fars_read("accident_2016.csv.bz2"), "file 'accident_2016.csv.bz2' does not exist", fixed=TRUE)
})

# Test make_filename function
test_that("Checkt if the make_filename function works as expected", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
  expect_equal(make_filename(2014), "accident_2014.csv.bz2")
  expect_equal(make_filename(2015), "accident_2015.csv.bz2")
  expect_equal(make_filename(2016), "accident_2016.csv.bz2")
})
