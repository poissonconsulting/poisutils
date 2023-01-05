#' Hexadecimal to Decimal PIT Tag Converter
#'
#' @param hex A string of the hex code.
#'
#' @return An double.
#' @export
#'
#' @examples
#' hex_to_dec_code("349EA72A50")
#' hex_to_dec_code("14A5D0BE89")
hex_to_dec_code <- function(hex) {
  chk::chk_string(hex)
  dec <- as.numeric(Rmpfr::mpfr(hex, base = 16))
  dec
}
