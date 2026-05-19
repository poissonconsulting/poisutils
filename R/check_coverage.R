#' Check test coverage for a function
#'
#' @param function_name A string of the function name (with or without `()`)
#' whose script is in the `R/` folder.
#' @param test_file A string specifying the test script to use.
#' Defaults to `NULL`, which makes the function search for a script in the
#' `tests/testthat` folder.
#' Additionally, the script is assumed to have the same name as `function_name`
#' but start with `test-`, and all underscores are substituted for dashes.
#' @return A coverage report in the Viewer pane.
#' @export
check_coverage <- function(function_name, test_file = NULL) {
  chk::chk_string(function_name)
  chk::chk_null_or(test_file, vld = chk::vld_string)

  function_name <- gsub("\\.R", "", function_name)
  function_name <- gsub("\\(.*\\)", "", function_name)
  function_file <- paste0("R/", function_name, ".R")

  if (is.null(test_file)) {
    test_file <- paste0(
      "tests/testthat/test-",
      gsub("_", "-", function_name), ".R"
    )
  }
  covr::file_report(covr::file_coverage(source_files = function_file,
                                        test_files = test_file))
}
