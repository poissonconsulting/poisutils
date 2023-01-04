test_that("change", {
  expect_equal(ps_nfold_change(3, c(3, 1, 9)), c(0, -2, 2))
  expect_equal(ps_prop_change(3, c(3, 1, 9)), c(0, -0.66666667, 2))
  expect_equal(
    ps_prop2nfold_change(ps_prop_change(3, c(3, 1, 9))),
    ps_nfold_change(3, c(3, 1, 9))
  )
})
