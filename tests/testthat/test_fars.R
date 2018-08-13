context("Read in data")
file_path <- system.file("data","accident_2013.csv",package="FARS")
data_2013 <- fars_read(file_path)

test_that("", {
  expect_error(fars_read(system.file("data","accident_2001.csv",package = "FARS")))
})
