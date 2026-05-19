# number of successes include those ran in the tests inside check_coverage()
test_that("check_coverage", {
  expect_error(check_coverage(1), "`function_name` must be a string \\(non-missing character scalar\\).")
  expect_error(check_coverage("", 1), "`test_file` must be a string \\(non-missing character scalar\\).")

  expect_no_error(check_coverage("hex-to-dec", "tests/testthat/test-hex-to-dec.R"))
  expect_no_error(check_coverage("hex-to-dec"))

  # will fail when running tests (too many functions in a script) but not if running manually
  expect_no_error(check_coverage("change"))
  expect_no_error(check_coverage("change.R", test_file = "tests/testthat/test-change.R"))
})

if (FALSE) {
  covr::file_coverage("R/check_coverage.R", "tests/testthat/test-check-coverage.R")
}
