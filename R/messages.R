#' Stop Function Execution
#'
#' Stops execution with an error message with call. = FALSE by default.
#'
#' @inheritParams base::stop
#' @export
error <- function(..., call. = FALSE) {
  stop(..., call. = call.)
}
