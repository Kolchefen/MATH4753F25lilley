utils::globalVariables("x") # To supress no-binding note

#' My Normal Curve
#'
#'
#' @param mu the mean of the curve (peak)
#' @param sigma the average variation about the curve
#' @param a the probability to shade calculate up until a
#'
#' @returns Normal Curve with a shaded area
#' @export
#'
#' @examples myncurve(1,2,1)
myncurve = function(mu = 1, sigma=2, a=1){
  curve(stats::dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3*sigma, mu + 3*sigma))

  xcurve <- seq(mu - 3*sigma, a, length = 1000)
  ycurve <- stats::dnorm(xcurve, mean = mu, sd = sigma)  # Added stats::

  graphics::polygon(c(mu - 3*sigma, xcurve, a), c(0, ycurve, 0), col = "pink")

  prob = stats::pnorm(a, mean = mu, sd = sigma)
  list(mu = mu, sigma = sigma, area = round(prob, 4))
}

