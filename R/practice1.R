#' Plot the x^2 curve for a range
#'
#' @param x A number.
#' @param y A number.
#' @return The y = x^.
#' @examples myNewFunc(1:10)
#' @export myNewFunc

myNewFunc <- function(x) {
  y = x^2
  plot(y ~ x)
  list(x = x, y = y)
}
