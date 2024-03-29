#' Create Directory
#'
#' @param dir A string of the directory name
#' @param ask A flag indicating whether to ask before creating a new directory.
#' @return An invisible flag indicating whether the directory exists.
#' @export
ps_create_dir <- function(dir, ask = TRUE) {
  chk_string(dir)
  chk_flag(ask)

  if (dir.exists(dir)) {
    return(invisible(TRUE))
  }

  if (ask && !yesno("Create directory '", dir, "'?")) {
    return(invisible(FALSE))
  }

  dir.create(path = dir, showWarnings = FALSE, recursive = TRUE)

  invisible(TRUE)
}
