#' Get user
#'
#' Gets current user as defined by operating system.
#'
#' @return A character scalar
#' @export
user <- function() {
  user <- Sys.info()["user"]
  names(user) <- NULL
  user
}
