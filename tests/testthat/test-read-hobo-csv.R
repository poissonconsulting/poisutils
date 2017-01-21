context("read-hobo-csv")

test_that("can read a single hobo csv file", {
  read_hobo_csv(system.file("hobo", "10723440.csv", package = "poisutils"))
})
