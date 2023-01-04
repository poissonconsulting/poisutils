#' Is syntactic
#'
#' @param x A character of possible variable names.
#' @return A logical vector indicating whether a syntactically correct variable name.
#' @export
#' @examples
#' is.syntactic(c("0", "x", "1x", "x y", "x1"))
is.syntactic <- function(x) {
  x == make.names(x)
}
