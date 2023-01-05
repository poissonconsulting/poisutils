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

  # need to make code 12 digits long
  if (dec == 12) {
    pit_tag_code <- as.character(paste0(country_code, dec))
    return()
  }



  hex_to_dec_code


}
