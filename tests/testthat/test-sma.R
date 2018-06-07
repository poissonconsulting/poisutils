context("sma")

test_that("sma", {
  expect_equal(ps_sma(1:4, 0L), c(1, 2, 3, 4))
  expect_equal(ps_sma(1:4), c(1.5, 2, 3, 3.5))
  expect_equal(ps_sma(1:4, 2L), c(2.0, 2.5, 2.5, 3.0))
  expect_equal(ps_sma(c(1:3,NA)), c(1.5, 2.0, NA, NA))
  expect_equal(ps_sma(c(1:3,NA), na.rm = TRUE), c(1.5, 2.0, 2.5, 3.0))
})
