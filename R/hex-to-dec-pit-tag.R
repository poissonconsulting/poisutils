#' Convert Hexidecimal to Decimal PIT Tags
#'
#' @param hex A string of the hex code.
#' @param country_code Number of the country code to add to the decimal code.
#'
#' @return A string.
#' @export
#'
#' @examples
#' ps_hex_to_dec_pit_tag("349EA72A50")
#' ps_hex_to_dec_pit_tag("349EA70")
#' ps_hex_to_dec_pit_tag("349EA72A50000")
#' ps_hex_to_dec_pit_tag("349EA72A50", country_code = 124)
ps_hex_to_dec_pit_tag <- function(hex, country_code = 900) {
  chk::chk_whole_number(country_code)
  dec <- hex_to_dec_code(hex)
  vals <- vapply(dec, length_test_append_country, country_code, FUN.VALUE = "")
  vals
}

length_test_append_country <- function(dec, country_code) {
  if (is.na(dec)) {
    return(NA_character_)
  }

  dec_length <- nchar(dec)
  if (dec_length >= 13) {
    return(NA_character_)
  }
  if (dec_length <= 11) {
    pit_tag_code <- stringr::str_pad(dec, 12, side = "left", pad = "0")
    pit_tag_code <- as.character(paste0(country_code, pit_tag_code))
    return(pit_tag_code)
  }
  pit_tag_code <- as.character(paste0(country_code, dec))
  pit_tag_code
}
