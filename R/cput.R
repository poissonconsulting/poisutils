#' Write an Object to the Clipboard
#'
#' Writes an object (or the evaluated contents of the clipboard) to the clipboard.
#'
#' Useful for pasting relatively simple objects into examples or tests.
#'
#' @param x An object or NULL if R code in the clipboard is to be evaluated and then write
#' @param envir An environment in which the contents of the clipboard are to be evaluated if x is `NULL`.
#'
#' @return An invisible copy to the object.
#' @seealso [`dput()`]
#' @export
#' @examples
#' cput(data.frame(x = 1))
cput <- function(x = NULL, envir = parent.frame()) {
  chk_environment(envir)
  if(is.null(x)) {
    x <- read_clip(allow_non_interactive = TRUE)
    x <- str2lang(x)
    x <- eval(x, envir = envir)
  }
  str <- capture.output(dput(x))
  cat(str, sep = "\n")
  write_clip(str, object_type = "character", allow_non_interactive = TRUE)
  invisible(x)
}

str2lang <- function(text) {
  ex <- parse(text=text, keep.source=FALSE)
  ex[[1L]]
}
