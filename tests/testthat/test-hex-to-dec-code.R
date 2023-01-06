test_that("default is to error", {
  expect_error(
    hex_to_dec_code(),
    regexp = 'argument "hex" is missing, with no default'
  )
})

test_that("code converted to correct value properly", {
  dec_code <- hex_to_dec_code("349EA72A50")
  expect_identical(
    dec_code,
    226000054864
  )
})

test_that("correct type of output value", {
  dec_code <- hex_to_dec_code("349EA72A50")
  expect_type(
    dec_code,
    "double"
  )
})

test_that("error when numeric value passed", {
  expect_error(
    hex_to_dec_code(12424543),
    regexp = "`hex` must be character or factor"
  )
})

test_that("pass when multiple values passed", {
  dec_codes <- hex_to_dec_code(c("AD14", "BC87"))
  expect_identical(
    dec_codes,
    c(44308, 48263)
  )
})
