test_that("change", {
  expect_equal(nfold_change(3, c(3, 1, 9)), c(0, -2, 2))
  expect_equal(fold_change(3, c(3, -6, 9)), c(1, -2, 3))
  expect_equal(prop_change(3, c(3, 1, 9)), c(0, -0.66666667, 2))
  expect_equal(prop2nfold(prop_change(3, c(3, 1, 9))),
               nfold_change(3, c(3, 1, 9)))
  expect_equal(prop2nfold(prop_change(-3, c(3, 1, 9))),
               nfold_change(-3, c(3, 1, 9)))
})

test_that("change", {
  expect_equal(fold2nfold(fold_change(3, c(3, -6, 9))),
                         nfold_change(3, c(3, -6, 9)))
  expect_equal(fold2nfold(fold_change(-3, c(3, -6, 9))),
               nfold_change(-3, c(3, -6, 9)))
  expect_equal(fold2nfold(fold_change(6, c(-3, 1, 9))),
               nfold_change(6, c(-3, 1, 9)))
  expect_equal(fold2nfold(fold_change(6, c(-3, 1, 9))),
               nfold_change(6, c(-3, 1, 9)))

  expect_equal(nfold2fold(nfold_change(6, c(-3, 1, 9))),
               fold_change(6, c(-3, 1, 9)))
  expect_equal(nfold2fold(nfold_change(-3, c(3, -6, 9))),
               fold_change(-3, c(3, -6, 9)))


  expect_equal(nfold2prop(nfold_change(3, c(3, 1, 9))),
                          prop_change(3, c(3, 1, 9)))
  expect_equal(nfold2prop(nfold_change(-3, c(3, 1, 9))),
               prop_change(-3, c(3, 1, 9)))
})


fold_change(3, c(1, 2, 9))
nfold_change(3, c(1, 6, 9))
nfold2fold(nfold_change(3, c(1, 6, 9)))

fold_change(6, 1)
nfold_change(6, 1)
nfold2fold(nfold_change(6, 1))

fold_change(3, 6)
nfold_change(3, 6)
nfold2fold(nfold_change(3, 6))

nfold_change(3, c(3, 1, 9))
prop_change(3, c(3, 1, 9))
nfold2prop(nfold_change(3, c(3, 1, 9)))

nfold_change(-3, 1)
prop_change(-3, 1)
nfold2prop(nfold_change(-3,  1))

nfold_change(3, 1)
prop_change(3, 1)
nfold2prop(nfold_change(3,  1))

nfold_change(6, 1)
prop_change(6, 1)
nfold2prop(nfold_change(6,  1))
