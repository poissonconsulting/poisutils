#' Error Message
#'
#' Stops execution with an error message.
#'
#' @inheritParams base::stop
#' @export
ps_error <- function(..., call. = FALSE) {
  stop(..., call. = call.)
}

#' Error Message
#'
#' Stops execution with an error message.
#'
#' @inheritParams base::stop
#' @export
error <- function(..., call. = FALSE) {
  .Deprecated("ps_error")
  ps_error(..., call. = call.)
}

#' Warning Message
#'
#' Generates a warning message.
#'
#' @inheritParams base::warning
#' @export
ps_warning <- function(..., call. = FALSE) {
  warning(..., call. = call.)
}

#' Diagnostic Message
#'
#' Generates a diagnostic message.
#'
#' @inheritParams base::message
#' @export
ps_message <- function(...) {
  message(...)
}

