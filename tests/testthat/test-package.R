context("package")

test_that("package", {
  expect_error(ps_error())
  expect_warning(ps_warning())
  user <- user()
  expect_is(user, "character")
  expect_identical(length(user), 1L)

  expect_false(is.named(user))
  names(user) <- user
  expect_true(is.named(user))

  dir <- file.path(tempdir(), "temp")
  expect_true(ps_create_dir(dir, ask = FALSE))
  expect_true(ps_create_dir(dir))
})
