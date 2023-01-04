#' Round Values while Preserving their Rounded Sum
#'
#' @param x A numeric vector of values to round
#' @param digits A count of the number of decimals for rounding
#' @source <http://biostatmatt.com/archives/2902>
#' @examples
#' sum(c(0.33, 0.33, 0.33))
#' round(c(0.33, 0.33, 0.33), 1)
#' sum(round(c(0.33, 0.33, 0.33), 1))
#' ps_round_preserve_sum(c(0.33, 0.33, 0.33), 1)
#' sum(ps_round_preserve_sum(c(0.33, 0.33, 0.33), 1))
#' @export
ps_round_preserve_sum <- function(x, digits = 0L) {
  digits <- as.integer(digits)
  chk_whole_number(digits)
  chk_integer(digits)
  chk_gte(digits)
  up <- 10^digits
  x <- x * up
  y <- floor(x)
  indices <- utils::tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}
