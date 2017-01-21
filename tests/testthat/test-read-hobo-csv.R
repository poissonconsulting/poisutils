context("read-hobo-csv")

test_that("can read a single hobo csv file", {
  data <- read_hobo_csv(system.file("hobo", "10723440.csv", package = "poisutils"))
  expect_is(data, "tbl")
  expect_identical(colnames(data), c("Logger", "DateTime", "Temperature", "FileRow", "FileName", "Directory"))
  expect_equal(data$Temperature[1:2], c(17.106, 16.177))
})
