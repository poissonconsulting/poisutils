#' Simple Moving Average
#'
#' Returns the simple moving average with a window of \code{2 * n}.
#'
#' @param x An integer or double vector.
#' @param n A positive integer of the window before and after each value.
#' @param na.rm A flag indicating whether to ignore missing values when calculating the mean.
#' @return A double vector.
#' @export
#' @examples
#' ps_sma(1:4)
ps_sma <- function(x, n = 1L, na.rm = FALSE) {
  checkor(check_vector(x, c(1, NA)), check_vector(x, c(1L, NA)))
  check_scalar(n, c(0L, length(x)))
  check_flag(na.rm)
  if(n == 0L) return(x)

  start <- apply(data.frame(1, 1:length(x) - n), 1, max)
  end <- apply(data.frame(length(x), 1:length(x) + n), 1, min)
  mapply(function(x, start, end) mean(x[start:end], na.rm = na.rm),
               start, end, MoreArgs = list(x = x))
}
