test_that("nfold_change", {
  expect_equal(nfold_change(3, c(3, 1, 9)), c(0, -2, 2))
  expect_equal(nfold_change(1, c(10, -1, 6)), c(9, 2, 5))
  expect_equal(nfold_change(-6, c(3, -1, 6)), c(3, -5, 2))
})

test_that("prop_change", {
  expect_equal(prop_change(3, c(3, 1, 9)), c(0, -0.66666667, 2))
  expect_equal(prop_change(1, c(10, -1, 6)), c(9, -2, 5))
  expect_equal(prop_change(-6, c(3, -1, 6)), c(-1.5, -0.83333333, -2))
})

test_that("foldchange", {
  expect_equal(fold_change(3, c(3, 1, 9)), c(1, .333333333, 3))
  expect_equal(fold_change(1, c(10, -1, 6)), c(10, -1, 6))
  expect_equal(fold_change(-6, c(3, -1, 6)), c(-0.5, 0.166666667, -1))
})

test_that("fold2nfold", {
  expect_equal(fold2nfold(fold_change(3, c(3, 1, 9))),
               nfold_change(3, c(3, 1, 9)))
  expect_equal(fold2nfold(fold_change(1, c(10, -1, 6))),
               nfold_change(1, c(10, -1, 6)))
  expect_equal(fold2nfold(fold_change(-6, c(3, -1, 6))),
               nfold_change(-6, c(3, -1, 6)))
})

test_that("prop2nfold", {
  expect_equal(prop2nfold(prop_change(3, c(3, 1, 9))),
               nfold_change(3, c(3, 1, 9)))
  expect_equal(prop2nfold(prop_change(1, c(10, -1, 6))),
               nfold_change(1, c(10, -1, 6)))
  expect_equal(prop2nfold(prop_change(-6, c(3, -1, 6))),
               nfold_change(-6, c(3, -1, 6)))
})

test_that("nfold2prop", {
  expect_equal(nfold2prop(nfold_change(3, c(3, 1, 9))),
               prop_change(3, c(3, 1, 9)))
  expect_equal(nfold2prop(nfold_change(1, c(10, -1, 6))),
               prop_change(1, c(10, -1, 6)))
  expect_equal(nfold2prop(nfold_change(-6, c(3, -1, 6))),
               prop_change(-6, c(3, -1, 6)))
})
