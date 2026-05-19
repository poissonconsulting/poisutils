test_that("coverage_summary", {
  expect_error(coverage_summary(1), "`function_files` must be character.")
  expect_error(coverage_summary("", 1), "`test_files` must be character.")

  expect_no_error(coverage_summary("R/hex-to-dec.R", "tests/testthat/test-hex-to-dec.R"))
  expect_no_error(coverage_summary("R/hex-to-dec.R"))

  expect_error(coverage_summary("change.R"))
  expect_no_error(coverage_summary("R/change.R"))
  coverage_summary("R/change.R", test_file = "tests/testthat/test-change.R")
  coverage_summary("R/change.R", test_file = "tests/testthat/test-h2d-tag.R")
})
