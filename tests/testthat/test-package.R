test_that("syntactic", {
  expect_identical(
    is.syntactic(c("0", "x", "1x", "x y", "x1")),
    c(FALSE, TRUE, FALSE, FALSE, TRUE)
  )
})

test_that("package", {
  expect_error(ps_error())
  expect_warning(ps_warning())
  user <- user()
  expect_type(user, "character")
  expect_identical(length(user), 1L)

  expect_false(is.named(user))
  names(user) <- user
  expect_true(is.named(user))

  dir <- file.path(tempdir(), "temp")
  expect_true(ps_create_dir(dir, ask = FALSE))
  expect_true(ps_create_dir(dir))
})

test_that("ps_rename_object", {
  x <- 10
  expect_identical(x, 10)
  expect_error(y)
  expect_error(ps_rename_object(x + 1, "y"))
  ps_rename_object(x, "y")
  expect_identical(y, 10)
  expect_error(x)
})
