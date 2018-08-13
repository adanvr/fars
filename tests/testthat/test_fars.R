context("Read in data")

test_that("", {
  expect_error(fars_read(system.file("data","accident_2001.csv",package = "FARS")))
})
