#' Convert Hexidecimal to Decimal PIT Tags
#'
#' @param hex A string of the hex code.
#' @param country_code Number of the country code to add to the decimal code.
#'
#' @return A string.
#' @export
#' @details If the decimal code is 11 digits long a zero is padded to the front
#'  of the value before the country code is added. If the code is 10 or less
#'  digits long then NA will be returned. Any decimal codes 13 or more digits
#'  long will return NA. If the code is 12 digits long the country code is
#'  appended directly to the decimal value.
#'
#' @examples
#' ps_h2d_tag("349EA72A50")
#' ps_h2d_tag("349EA70")
#' ps_h2d_tag("349EA72A50000")
#' ps_h2d_tag("349EA72A50", country_code = 124)
ps_h2d_tag <- function(hex, country_code = 900) {
  chk::chk_whole_number(country_code)
  dec <- hex_to_dec(hex)
  vals <- vapply(
    dec, length_test_append_country, country_code,
    FUN.VALUE = "", USE.NAMES = FALSE
  )
  vals
}

length_test_append_country <- function(dec, country_code) {
  if (is.na(dec)) {
    return(NA_character_)
  }

  dec_length <- nchar(dec)
  if (dec_length >= 13 || dec_length < 11) {
    return(NA_character_)
  }
  if (dec_length == 11) {
    pit_tag_code <- stringr::str_pad(dec, 12, side = "left", pad = "0")
    pit_tag_code <- as.character(paste0(country_code, pit_tag_code))
    return(pit_tag_code)
  }
  pit_tag_code <- as.character(paste0(country_code, dec))
  pit_tag_code
}
