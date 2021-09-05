test_that("cput works", {
  skip_on_os("linux")
  expect_output(expect_identical(expect_invisible(cput(1:2)), 1:2), "1:2")
})

test_that("cput works from clipboard", {
  skip_on_os("linux")
  x <- data.frame(y = 2, z = 1)
  clipr::write_clip("colnames(x)", allow_non_interactive = TRUE)
  expect_output(expect_identical(expect_invisible(cput()), c("y", "z")), 'c\\("y", "z"\\)')
})

