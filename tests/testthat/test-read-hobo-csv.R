context("read-hobo-csv")

test_that("can read a single hobo csv file", {
  data <- read_hobo_csv(system.file("hobo", "10723440.csv", package = "poisutils"), quiet = TRUE)
  expect_is(data, "tbl")
  expect_identical(colnames(data), c("Logger", "DateTime", "Temperature_degC", "FileRow", "FileName", "Directory"))
  expect_identical(nrow(data), 11L)
  expect_identical(lubridate::tz(data$DateTime), "Etc/GMT+8")
  expect_equal(lubridate::hour(data$DateTime[1:2]), c(7L,7L))
  expect_equal(lubridate::minute(data$DateTime[1:2]), c(18L,23L))
  expect_equal(data$Temperature_degC[1:2], c(14.649, 17.106))
})

test_that("can read a single hobo csv file converting to farenheit and utc_offset 0", {
  data <- read_hobo_csv(system.file("hobo", "10723440.csv", package = "poisutils"), quiet = TRUE, units = "degF", tz = "UTC")
  expect_identical(colnames(data), c("Logger", "DateTime", "Temperature_degF", "FileRow", "FileName", "Directory"))
  expect_identical(nrow(data), 11L)
  expect_identical(lubridate::tz(data$DateTime), "UTC")
  expect_equal(lubridate::hour(data$DateTime[1:2]), c(15L,15L))
  expect_equal(lubridate::minute(data$DateTime[1:2]), c(18, 23L))
  expect_equal(data$Temperature_degF[1:2], c(58.3682, 62.7908))
})

test_that("can read multiple hobo csv file", {
  data <- read_hobo_csv(system.file("hobo", package = "poisutils"), quiet = TRUE, recursive = TRUE)
  expect_is(data, "tbl")
  expect_identical(colnames(data), c("Logger", "DateTime", "Temperature_degC", "FileRow", "FileName", "Directory"))
  expect_identical(nrow(data), 49L)
  expect_identical(unique(data$Logger), sort(c("10170918", "10171277", "10171279","10171286", "10328118", "10328122", "10723440", "10723450", "2391458")))
  expect_identical(lubridate::tz(data$DateTime), "Etc/GMT+8")
})
