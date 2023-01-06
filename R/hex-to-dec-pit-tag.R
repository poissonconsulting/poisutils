#' Convert Hexidecimal to Decimal PIT Tags
#'
#' @param hex A string of the hex code.
#' @param country_code Number of the country code to add to the decimal code.
#'
#' @return A string.
#' @export
#'
#' @examples
hex_to_dec_pit_tag <- function(hex, country_code = 900) {
  chk::chk_whole_number(country_code)

  dec <- hex_to_dec_code(hex)
  dec_length <- stringr::str_length(dec)

  # codes have to be 12 digits long
  if (dec_length == 12) {
    pit_tag_code <- as.character(paste0(country_code, dec))
    return(pit_tag_code)
  }

  if (dec_length >= 13) {
    return(NA_character_)
  }

  if (dec_length <= 11) {
    pit_tag_code <- stringr::str_pad(dec, 12, side = "left")
    pit_tag_code <- as.character(paste0(country_code, dec))
    return(pit_tag_code)
  }

  print("never here ... ?")

}
