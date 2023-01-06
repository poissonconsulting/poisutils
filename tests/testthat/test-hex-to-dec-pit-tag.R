test_that("default is to error", {
  expect_error(
    hex_to_dec_pit_tag(),
    regexp = 'argument "hex" is missing, with no default'
  )
})

test_that("default country number is appended to front of conversion", {
  dec_code <- hex_to_dec_pit_tag("349EA72A50")
  expect_identical(dec_code, "900226000054864")
})

test_that("output is 15 digits long when dec is 12 digits long", {
  dec_code <- hex_to_dec_pit_tag("349EA72A50")
  expect_identical(dec_code, "900226000054864")
  expect_identical(nchar(dec_code), 15L)
})

test_that("output is 15 digits long when dec is 11 digits long", {
  dec_code <- hex_to_dec_pit_tag("49EA72A50")
  expect_identical(dec_code, "900019841624656")
  expect_identical(nchar(dec_code), 15L)
})

test_that("output is 15 digits long when dec is less then 11 digits long", {
  dec_code <- hex_to_dec_pit_tag("A50")
  expect_identical(dec_code, "900000000002640")
  expect_identical(nchar(dec_code), 15L)
})

test_that("output is NA when dec outputted more then 12 digits long", {
  dec_code <- hex_to_dec_pit_tag("49EA72A5000")
  expect_identical(dec_code, NA_character_)
})

test_that("country code changes when passed", {
  dec_code <- hex_to_dec_pit_tag("349EA72A50", country_code = 800)
  expect_identical(dec_code, "800226000054864")
  expect_identical(nchar(dec_code), 15L)
})
