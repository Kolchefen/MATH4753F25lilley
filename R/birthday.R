#' Determine the probability 2 or more people share the same birthday in a group of x number of people.
#'
#' @param x Size of group of people
#'
#' @returns Percentage of Probability
#' @export
#'
#' @examples birthday(20)
birthday <- function(x) {
  1-exp(lchoose(365,x) + lfactorial(x) - x*log(365))
}
# > usethis::use_vignette("Birthday")
