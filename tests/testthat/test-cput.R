test_that("cput works", {
  skip_on_os("linux")
  expect_identical(cput(1:2, allow_non_interactive = TRUE), 1:2)
})

