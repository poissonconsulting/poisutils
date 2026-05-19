#' Check the test coverage for multiple functions
#'
#' @param function_files A character vector of file names, which are assumed to
#' be in the `R/` folder.
#' @param test_files A character vector of length equal to that of
#' `function_files` specifying the test script(s) to use.
#' Defaults to `NULL`, which makes the function search for a script in the
#' `tests/testthat` folder.
#' Additionally, the script is assumed to have the same name as `function_files`
#' but start with `test-`, and all underscores are substituted for dashes.
#' @param return_all A flag indicating whether the function should return all
#' files (including those with 100% coverage) or only the files without complete
#' coverage (default).
#' @return A coverage report in the Viewer pane.
#' @export

coverage_summary <- function(function_files, test_files = NULL, ...,
                             return_all = FALSE) {
  chk::chk_character(function_files)
  chk::chk_null_or(test_files, vld = chk::vld_character)
  if (!is.null(test_files)) {
    chk::chk_length(test_files, length(function_files))
  }
  chk::chk_file(function_files)

  if (is.null(test_files)) {
    test_files <- gsub("_", "-", gsub("R/", "", function_files))
    test_files <- paste0("tests/testthat/test-", test_files)
  }

  out <- purrr::map2(function_files, test_files, function(.fun, .test) {
    if (!file.exists(.test)) {
      return(data.frame(`function` = .fun, test = .test, coverage = "no test file found"))
    }

    strings <- covr::file_coverage(.fun, .test) %>%
      # double-capturing output to circumvent the "test succeeded" from usethis
      # concatenates the outputs into a single character vector
      capture.output(type = "message") %>%
      capture.output(type = "output")

    strings <- strings[grepl("\\%", strings)]
    strings <- unlist(strsplit(strings, "\"", fixed = TRUE))
    strings <- strings[grepl("R/", strings)]
    percent <- gsub(".* ", "", strings)

    data.frame(`function` = .fun, test = .test, coverage = percent)
  }) %>%
    dplyr::bind_rows()

  if (return_all) {
    return(out)
  } else {
    out <- dplyr::filter(out, coverage != "100.00%")
  }

  if (nrow(out) > 0) {
    return(out)
  }
  message("All functions tested have 100.00% coverage.")
}
