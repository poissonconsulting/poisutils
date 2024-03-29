test_that("default is to error", {
  expect_error(
    hex_to_dec(),
    regexp = 'argument "hex" is missing, with no default'
  )
})

test_that("code converted to correct value properly", {
  dec_code <- hex_to_dec("349EA72A50")
  expect_identical(
    dec_code,
    "226000054864"
  )
})

test_that("correct type of output value", {
  dec_code <- hex_to_dec("349EA72A50")
  expect_type(
    dec_code,
    "character"
  )
})

test_that("error when numeric value passed", {
  expect_error(
    hex_to_dec(12424543),
    regexp = "`hex` must be character or factor"
  )
})

test_that("pass when multiple values passed", {
  dec_codes <- hex_to_dec(c("AD14", "BC87"))
  expect_identical(
    dec_codes,
    c("44308", "48263")
  )
})

test_that("pass when NA passed", {
  dec_codes <- hex_to_dec(NA_character_)
  expect_identical(
    dec_codes,
    NA_character_
  )
})

test_that("pass when multiple values passed with one being NA", {
  dec_codes <- hex_to_dec(c("AD14", "BC87", NA_character_))
  expect_identical(
    dec_codes,
    c("44308", "48263", NA_character_)
  )
})

test_that("returns NA when letters above F are passed (not hex letters)", {
  dec_code <- hex_to_dec("G1")
  expect_identical(
    dec_code,
    NA_character_
  )
})

test_that("passes when various values are passed", {
  dec_codes <- hex_to_dec(c("A1", NA_character_, "G1", "B1"))
  expect_identical(
    dec_codes,
    c("161", NA_character_, NA_character_, "177")
  )
})

test_that("passes when zero length character vector passed", {
  dec_codes <- hex_to_dec(character())
  expect_identical(
    dec_codes,
    character()
  )
})

test_that("errors when zero length numeric vector passed", {
  expect_error(
    hex_to_dec(numeric()),
    regexp = "`hex` must be character or factor\\."
  )
})
