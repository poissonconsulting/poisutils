test_that("cput works", {
  expect_identical(cput(1:2, allow_non_interactive = TRUE), 1:2)
})

