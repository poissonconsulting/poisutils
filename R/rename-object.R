#' Rename Object
#'
#' @param x The object to rename.
#' @param new_name A string of the new name.
#' @param envir The environment.
#' @export
ps_rename_object <- function(x, new_name, envir = parent.frame()) {
  chk_string(new_name)
  if (!is.syntactic(new_name)) {
    ps_error("new_name '", new_name, "' is not syntactically valid")
  }

  x_name <- deparse(substitute(x))

  if (!is.syntactic(x_name)) ps_error("x must be a single R object")

  assign(new_name, x, envir = envir)
  rm(list = x_name, envir = envir)
  invisible(NULL)
}
