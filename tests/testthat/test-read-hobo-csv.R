context("read-hobo-csv")

test_that("can read a single hobo csv file", {
  data <- read_hobo_csv(system.file("hobo", "10723440.csv", package = "poisutils"), quiet = TRUE)
  expect_is(data, "tbl")
  expect_identical(colnames(data), c("Logger", "DateTime_m8", "Temperature_degC", "FileRow", "FileName", "Directory"))
  expect_identical(nrow(data), 9L)
  expect_identical(lubridate::tz(data$DateTime_m8), "UTC")
  expect_equal(lubridate::hour(data$DateTime_m8[1:2]), c(7L,7L))
  expect_equal(lubridate::minute(data$DateTime_m8[1:2]), c(23L,28L))
  expect_equal(data$Temperature_degC[1:2], c(17.106, 16.177))
})

test_that("can read a single hobo csv file converting to farenheit and utc_offset 0", {
  data <- read_hobo_csv(system.file("hobo", "10723440.csv", package = "poisutils"), quiet = TRUE, units = "degF", utc_offset_hr = 0)
  expect_identical(colnames(data), c("Logger", "DateTime_p0", "Temperature_degF", "FileRow", "FileName", "Directory"))
  expect_identical(nrow(data), 9L)
  expect_identical(lubridate::tz(data$DateTime_p0), "UTC")
  expect_equal(lubridate::hour(data$DateTime_p0[1:2]), c(15L,15L))
  expect_equal(lubridate::minute(data$DateTime_p0[1:2]), c(23L,28L))
  expect_equal(data$Temperature_degF[1:2], c(62.7908, 61.1186))
})

test_that("can read a single stopped hobo csv file", {
  data <- read_hobo_csv(system.file("hobo", "10328118.csv", package = "poisutils"), quiet = TRUE)
  expect_identical(colnames(data), c("Logger", "DateTime_m8", "Temperature_degC", "FileRow", "FileName", "Directory"))
  expect_identical(nrow(data), 0L)
  expect_identical(lubridate::tz(data$DateTime_m8), "UTC")
})

test_that("can read a more complicated hobo csv file", {
  data <- read_hobo_csv(system.file("hobo", "10328122.csv", package = "poisutils"), quiet = TRUE)
  expect_identical(colnames(data), c("Logger", "DateTime_m8", "Temperature_degC", "FileRow", "FileName", "Directory"))
  expect_identical(nrow(data), 2L)
  expect_identical(lubridate::tz(data$DateTime_m8), "UTC")
})

test_that("can read an even more complicated hobo csv file", {
  data <- read_hobo_csv(system.file("hobo", "M3.csv", package = "poisutils"), quiet = TRUE)
  expect_identical(colnames(data), c("Logger", "DateTime_m8", "Temperature_degC", "FileRow", "FileName", "Directory"))
  expect_identical(nrow(data), 4L)
})

test_that("can read multiple hobo csv file", {
  data <- read_hobo_csv(system.file("hobo", package = "poisutils"), quiet = TRUE, recursive = TRUE)
  expect_is(data, "tbl")
  expect_identical(colnames(data), c("Logger", "DateTime_m8", "Temperature_degC", "FileRow", "FileName", "Directory"))
  expect_identical(nrow(data), 28L)
  expect_identical(unique(data$Logger), sort(c("10328122", "10723440", "10723450", "10171286")))
  expect_identical(lubridate::tz(data$DateTime_m8), "UTC")
  expect_equal(lubridate::hour(data$DateTime_m8[1:2]), c(17L,17L))
  expect_equal(lubridate::minute(data$DateTime_m8[1:2]), c(1L,16L))
  expect_equal(data$Temperature_degC[1:2], c(13.81, 12.558))
})

