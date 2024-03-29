#' Hexadecimal to Decimal Code Converter
#'
#' @param hex A string of the hex code.
#'
#' @return The dec code as a character.
#' @export
#' @details The function will return NA if non HEX letters are present. The
#'  only letters that can be present in the HEX codes are A to F.
#'
#' @examples
#' hex_to_dec("349EA72A50")
#' hex_to_dec("14A5D0BE89")
#' hex_to_dec(c("14D", "E67"))
#' hex_to_dec(c("14D", "E67", NA))
hex_to_dec <- function(hex) {
  chk::chk_character_or_factor(hex)
  # replace any values non Hex letters with missing values
  hex[stringr::str_detect(hex, "[G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z]")] <-
    NA_character_
  # replace missing values with NaN due to function error handling
  hex[is.na(hex)] <- "NaN"
  dec <- as.numeric(Rmpfr::mpfr(hex, base = 16))
  # switch NaN's back to regular NA's
  dec[is.nan(dec)] <- NA_real_
  # convert to character before output
  dec <- as.character(dec)
  dec
}
