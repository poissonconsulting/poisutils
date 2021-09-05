#' Write an Object to the Clipboard
#'
#' @param x An object.
#' @param allow_non_interactive A flag specifying whether to allow
#' writing to the clipboard in an non-interactive session.
#'
#' @return An invisible copy to the object.
#' @seealso [`dput()`]
#' @export
cput <- function(x, allow_non_interactive = FALSE) {
  chk_flag(allow_non_interactive)
  str <- capture.output(dput(x))
  write_clip(str, object_type = "character", allow_non_interactive = allow_non_interactive)
  invisible(x)
}
