context("package")

test_that("package", {
  expect_error(error())
  user <- user()
  expect_is(user, "character")
  expect_identical(length(user), 1L)

  expect_false(is.named(user))
  names(user) <- user
  expect_true(is.named(user))
})
