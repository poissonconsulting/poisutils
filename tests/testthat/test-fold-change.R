test_that("prop_change edge cases", {
  expect_equal(prop_change(numeric(0), numeric(0)), numeric(0))
  expect_equal(prop_change(numeric(0), 1), numeric(0))
  expect_equal(prop_change(1, numeric(0)), numeric(0))
  expect_equal(prop_change(1, numeric(0)), numeric(0))
  expect_equal(prop_change(1, NA_real_), NA_real_)
  expect_equal(prop_change(NA_real_, 1), NA_real_)
})

test_that("prop_change correct values", {
  expect_equal(prop_change(1, c(1, 2, 3, 0)), c(0, 1, 2, -1))
  expect_equal(prop_change(0, c(0, 1, 2)), c(0, Inf, Inf))
  expect_equal(prop_change(3, c(3, 1, 9, 0)), c(0, -2/3, 2, -1))
})

test_that("fold_change edge cases", {
  expect_equal(fold_change(numeric(0), numeric(0)), numeric(0))
  expect_equal(fold_change(numeric(0), 1), numeric(0))
  expect_equal(fold_change(1, numeric(0)), numeric(0))
  expect_equal(fold_change(1, numeric(0)), numeric(0))
  expect_equal(fold_change(1, NA_real_), NA_real_)
  expect_equal(fold_change(NA_real_, 1), NA_real_)
})

test_that("fold_change correct values", {
  expect_equal(fold_change(1, c(1, 2, 3, 0)), c(1, 2, 3, 0))
  expect_equal(fold_change(0, c(0, 1, 2)), c(0, Inf, Inf))
  expect_equal(fold_change(3, c(3, 1, 9, 0)), c(1, 1/3, 3, 0))
})

test_that("nfold_change edge cases", {
  expect_equal(nfold_change(numeric(0), numeric(0)), numeric(0))
  expect_equal(nfold_change(numeric(0), numeric(0)), numeric(0))
  expect_equal(nfold_change(numeric(0), 1), numeric(0))
  expect_equal(nfold_change(1, numeric(0)), numeric(0))
  expect_equal(nfold_change(1, numeric(0)), numeric(0))
  expect_equal(nfold_change(1, NA_real_), NA_real_)
  expect_equal(nfold_change(NA_real_, 1), NA_real_)
})

test_that("nfold_change correct values", {
  expect_equal(nfold_change(1, c(1, 2, 3, 0)), c(0, 1, 2, -Inf))
  expect_equal(nfold_change(0, c(0, 1, 2)), c(0, Inf, Inf))
  expect_equal(nfold_change(3, c(3, 1, 9, 0)), c(0, -2, 2, -Inf))
  expect_equal(nfold_change(4,2), nfold_change(2,4) * -1)
})

test_that("fold2nfold", {
  expect_equal(fold2nfold(fold_change(3, c(3, 1, 9))),
               nfold_change(3, c(3, 1, 9)))
  skip("need to work out how to deal with Inf values in fold and prop functions")
  expect_equal(fold2nfold(fold_change(0, c(10, 0, 6))),
               nfold_change(0, c(10, 0, 6)))
})

test_that("prop2nfold", {
  expect_equal(prop2nfold(prop_change(3, c(3, 1, 9))),
               nfold_change(3, c(3, 1, 9)))
  expect_equal(prop2nfold(prop_change(1, c(10, 0, 6))),
               nfold_change(1, c(10, 0, 6)))
  expect_equal(prop2nfold(prop_change(0, c(3, 0, 6))),
               nfold_change(0, c(3, 0, 6)))
})

test_that("nfold2prop", {
  expect_equal(nfold2prop(nfold_change(3, c(3, 1, 9))),
               prop_change(3, c(3, 1, 9)))
  expect_equal(nfold2prop(nfold_change(1, c(10, 0, 6))),
               prop_change(1, c(10, 0, 6)))
  expect_equal(nfold2prop(nfold_change(0, c(3, 0, 6))),
               prop_change(0, c(3, 0, 6)))
})
