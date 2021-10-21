#' n-Fold Change
#'
#' @param x A vector of the before values
#' @param y A vector of the after values
#'
#' @return A vector of the n-fold change
#' @export
#'
#' @examples
#' ps_nfold_change(3, c(3, 1, 9))
ps_nfold_change <- function(x, y) {
  lifecycle::deprecate_soft("0.0.0.9010",
                            what = "poisutils::ps_nfold_change()",
                            with = "poisutils::nfold_change()"
  )
  x <- y / x
  ifelse(x >= 1, x - 1, -x^-1+1)
}

#' Proportional Change
#'
#' @param x A vector of the before values
#' @param y A vector of the after values
#'
#' @return A vector of the proportional change
#' @export
#'
#' @examples
#' ps_prop_change(3, c(3, 1, 9))
ps_prop_change <- function(x, y) {
  lifecycle::deprecate_soft("0.0.0.9010",
                            what = "poisutils::ps_prop_change()",
                            with = "poisutils::prop_change()"
  )
  (y - x) / x
}

#' Proportional Change to n-Fold Change
#'
#' @param x A numeric vector of the proportional change
#'
#' @return A numeric vector of the n-fold change
#' @export
#'
#' @examples
#' ps_nfold_change(3, c(3, 1, 9))
#' ps_prop_change(3, c(3, 1, 9))
#' ps_prop2nfold_change(ps_prop_change(3, c(3, 1, 9)))
ps_prop2nfold_change <- function(x) {
  lifecycle::deprecate_soft("0.0.0.9010",
                 what = "poisutils::ps_prop2nfold_change()",
                 with = "poisutils::prop2nfold()"
  )
  chk_vector(x)
  check_values(x, 1)
  if(!length(x)) return(x)
  ifelse(x >= 0, x, -(x+1)^-1+1)
}
