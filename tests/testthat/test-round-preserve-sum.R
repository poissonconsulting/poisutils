context("round-preserve-sum")

test_that("round_preserve_sum", {
  expect_identical(ps_round_preserve_sum(c(0.33, 0.33, 0.33), 1),
                   c(0.3, 0.3, 0.4))
})
