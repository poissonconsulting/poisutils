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
