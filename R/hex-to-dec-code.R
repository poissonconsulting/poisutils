#' Hexadecimal to Decimal Code Converter
#'
#' @param hex A string of the hex code.
#'
#' @return A double.
#' @export
#'
#' @examples
#' hex_to_dec_code("349EA72A50")
#' hex_to_dec_code("14A5D0BE89")
#' hex_to_dec_code(c("14D", "E67"))
hex_to_dec_code <- function(hex) {
  chk::chk_character_or_factor(hex)
  hex[is.na(hex)] <- "NaN"
  dec <- as.numeric(Rmpfr::mpfr(hex, base = 16))
  dec[is.nan(dec)] <- NA
  dec
}
