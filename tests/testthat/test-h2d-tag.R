test_that("default is to error", {
  expect_error(
    ps_h2d_tag(),
    regexp = 'argument "hex" is missing, with no default'
  )
})

test_that("default country number is appended to front of conversion", {
  dec_code <- ps_h2d_tag("349EA72A50")
  expect_identical(dec_code, "900226000054864")
})

test_that("output is 15 digits long when dec is 12 digits long", {
  dec_code <- ps_h2d_tag("349EA72A50")
  expect_identical(dec_code, "900226000054864")
  expect_identical(nchar(dec_code), 15L)
})

test_that("output is 15 digits long when dec is 11 digits long", {
  dec_code <- ps_h2d_tag("49EA72A50")
  expect_identical(dec_code, "900019841624656")
  expect_identical(nchar(dec_code), 15L)
})

test_that("output is NA when dec is less then 11 digits long", {
  dec_code <- ps_h2d_tag(c("A50", "11EA72A10"))
  expect_identical(dec_code, c(NA_character_, NA_character_))
})

test_that("output is NA when dec outputted more then 12 digits long", {
  dec_code <- ps_h2d_tag("49EA72A5000")
  expect_identical(dec_code, NA_character_)
})

test_that("country code changes when passed", {
  dec_code <- ps_h2d_tag("349EA72A50", country_code = 800)
  expect_identical(dec_code, "800226000054864")
  expect_identical(nchar(dec_code), 15L)
})

test_that("passes when multiple values passed", {
  dec_code <- ps_h2d_tag(c("349EA72A50", "349EA72A50"))
  expect_identical(dec_code, c("900226000054864", "900226000054864"))
})

test_that("returns NA when passed", {
  dec_code <- ps_h2d_tag(c("NA", "349EA72A50"))
  expect_identical(dec_code, c(NA_character_, "900226000054864"))
})

test_that("returns NA when passed two NA's", {
  dec_code <- ps_h2d_tag(c("NA", NA_character_))
  expect_identical(dec_code, c(NA_character_, NA_character_))
})

test_that("one fully numeric value and another alpha numeric value works", {
  dec_code <- ps_h2d_tag(c("1234538643", "349EA72A50"))
  expect_identical(dec_code, c("900078187300419", "900226000054864"))
})
