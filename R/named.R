#' Test if named
#'
#' @param x the object to test
#' @return A logical scalar indicating whether named
#' @export
#' @examples
#' is.named(1)
is.named <- function(x) {
  is.character(names(x))
}
