#' Punctuate
#'
#' @param x A vector to punctuate
#' @param qualifier A string to separate the last element.
#' @return A string.
#' @export
#'
#' @examples
#' ps_punctuate(1:3)
ps_punctuate <- function(x, qualifier = "or") {
  check_string(qualifier)
  if (is.logical(x) || is.integer(x) || is.numeric(x)) {
    x <- as.character(x)
  } else
    x <- paste0("'", as.character(x), "'")
  if (length(x) == 1)
    return(x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}

#' Plural
#'
#' @param x A string to pluralize.
#' @param n A count of the number of occurrences of x.
#' @param end A string of the last part of the pluralization
#' (useful for adding punctuation).
#' @return A string
#' @export
#'
#' @examples
#' ps_plural("column", 1)
#' ps_plural("column", 2)
#' ps_plural("column", 3, end = ".")
ps_plural <- function(x, n = 1L, end = "") {
  check_string(x)
  n <- check_count(n, coerce = TRUE)
  check_string(end)
  paste0(x, ifelse(n != 1L, "s", ""), end)
}
