#' n-Fold Change
#'
#' @param x A vector of the before values
#' @param y A vector of the after values
#'
#' @return A vector of the n-fold change
#' @export
#'
#' @examples
#' nfold_change(3, c(3, 1, 9))
nfold_change <- function(x, y) {
  chk_numeric(x)
  chk_numeric(y)
  chk_gte(x)
  chk_gte(y)
  if(!length(x)) return(numeric(0))
  if(!length(y)) return(numeric(0))

  x <- y / x
  ifelse(is.na(x) & x >= 1, x - 1, -x^-1 + 1)
}

#' Fold Change
#'
#' @param x A vector of the before values
#' @param y A vector of the after values
#'
#' @return A vector of the fold change
#' @export
#'
#' @examples
#' fold_change(3, c(3, 1, 9))
fold_change <- function(x, y) {
  chk_numeric(x)
  chk_numeric(y)
  chk_gte(x)
  chk_gte(y)

  y / x
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
#' prop_change(3, c(3, 1, 9))
prop_change <- function(x, y) {
  chk_numeric(x)
  chk_numeric(y)
  chk_gte(x)
  chk_gte(y)

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
#' nfold_change(3, c(3, 1, 9))
#' prop_change(3, c(3, 1, 9))
#' prop2nfold(prop_change(3, c(3, 1, 9)))
prop2nfold <- function(x) {
  chk_numeric(x)
  if(!length(x)) return(numeric(0))
  ifelse(is.na(x) & x >= 1, x, -(x+1)^-1+1)
}

#' n-Fold Change to Proportional Change
#'
#' @param x A numeric vector of the proportional change
#'
#' @return A numeric vector of the n-fold change
#' @export
#'
#' @examples
#' nfold_change(3, c(3, 1, 9))
#' prop_change(3, c(3, 1, 9))
#' nfold2prop(nfold_change(3, c(3, 1, 9)))
nfold2prop <- function(x) {
  chk_numeric(x)
  if (!length(x))
    return(numeric(0))
  ifelse(is.na(x) & x >= 1, x, -(x-1)^-1-1)
}

#' Fold Change to Proportional Change
#'
#' @param x A numeric vector of the proportional change
#'
#' @return A numeric vector of the n-fold change
#' @export
#'
#' @examples
#' fold_change(3, c(3, 1, 9))
#' prop_change(3, c(3, 1, 9))
#' fold2prop(fold_change(3, c(3, 1, 9)))
fold2prop <- function(x) {
  chk_numeric(x)
  chk_gte(x)
  if (!length(x))
    return(numeric(0))
  ifelse(is.na(x) & x >= 0, x - 1, x)
}

#' Fold to n-Fold Change
#'
#' @param x A vector of the n-fold values
#'
#' @return A vector of the fold change
#' @export
#'
#' @examples
#' fold2nfold(3)
fold2nfold <- function(x) {
  chk_numeric(x)
  chk_gte(x)
  if (!length(x))
    return(numeric(0))
  ifelse(is.na(x) & x >= 1, x - 1, -x^-1+1)
}

#' Fold to n-Fold Change
#'
#' @param x A vector of the n-fold values
#'
#' @return A vector of the fold change
#' @export
#'
#' @examples
#' nfold2fold(3)
nfold2fold <- function(x) {
  chk_numeric(x)
  if (!length(x))
    return(numeric(0))
  ifelse(is.na(x) & x >= 1, x + 1, -(x-1)^-1)

}






