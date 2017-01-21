context("read-hobo-csv")

test_that("can read a single hobo csv file", {
  data <- read_hobo_csv(system.file("hobo", "10723440.csv", package = "poisutils"))
  expect_is(data, "tbl")
  expect_identical(colnames(data), c("Logger", "DateTime", "Temperature_degC", "FileRow", "FileName", "Directory"))
  expect_equal(data$Temperature_degC[1:2], c(17.106, 16.177))
})

test_that("can read a single hobo csv file converting to farenheit", {
  data <- read_hobo_csv(system.file("hobo", "10723440.csv", package = "poisutils"), temp_units = "degF")
  expect_identical(colnames(data), c("Logger", "DateTime", "Temperature_degF", "FileRow", "FileName", "Directory"))
  expect_equal(data$Temperature_degF[1:2], c(62.7908, 61.1186))
})
