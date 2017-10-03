ps_rename_object <- function(x, new_name, envir = parent.frame()) {
  check_string(new_name)
  if (!is.syntactic(new_name)) ps_error("new_name '", new_name, "' is not syntactically valid")

  x_name <- deparse(substitute(x))

  if (!is.syntactic(x_name)) ps_error("x must be a single R object")

  assign(new_name, x, envir = envir)
  rm(list = x_name, envir = envir)
  invisible(NULL)
}
