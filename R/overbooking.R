#' @importFrom stats pbinom uniroot
#' @importFrom graphics abline legend points
NULL # No idea why, but this is needed to make the imports work, something about the formatter and roxygen docs not working well together.
#' Calculate Optimal Number of Tickets to Sell
#'
#' Calculates the optimal number of tickets to sell to maximize seat utilization
#' while accounting for passenger no-shows, and creates visualization plots.
#'
#' @param N Number of seats available
#' @param gamma Acceptable overbooking probability threshold (default 0.02)
#' @param p Probability that a passenger shows up (default 0.95)
#'
#' @return A list with nd (discrete optimum), nc (continuous optimum), N, p, and gamma
#'
#' @export
ntickets <- function(N = 200, gamma = 0.02, p = 0.95) {
  # Define the objective function
  # We want P(X <= N) to be as close to (1 - gamma) as possible
  objective <- function(x) {
    abs(pbinom(N, size = x, prob = p) - (1 - gamma))
  }

  # Discrete solution: search over a reasonable range
  # Start from N and go up to about N/p + some buffer
  max_tickets <- ceiling(N / p * 1.1)
  x_range <- seq(N, max_tickets, by = 1)
  obj_values <- sapply(x_range, objective)

  # Find the discrete optimum
  nd <- x_range[which.min(obj_values)]

  # Continuous solution using optimization
  # Use uniroot to find where the derivative changes sign
  # The function pbinom(N, x, p) - (1 - gamma) changes from negative to positive
  func_for_root <- function(x) {
    pbinom(N, size = round(x), prob = p) - (1 - gamma)
  }

  # Find bounds where the function changes sign
  lower_bound <- N
  upper_bound <- max_tickets

  # Check if root exists in the interval
  if (func_for_root(lower_bound) * func_for_root(upper_bound) > 0) {
    # If same sign, use the discrete solution as continuous
    nc <- nd
  } else {
    # Find the root
    nc <- uniroot(func_for_root,
                  interval = c(lower_bound, upper_bound),
                  tol = 0.001)$root
  }

  # Create plots in separate windows
  x_range <- seq(N, ceiling(N / p * 1.1), by = 1)

  obj_values <- sapply(x_range, function(n) {
    abs(pbinom(N, size = n, prob = p) - (1 - gamma))
  })

  prob_values <- sapply(x_range, function(n) {
    pbinom(N, size = n, prob = p)
  })

  # Plot 1: Objective Function
  plot(x_range, obj_values,
       type = "b",
       pch = 21,
       bg = ifelse(x_range == nd, "red", "blue"),
       cex = ifelse(x_range == nd, 1.5, 1),
       xlab = "Number of Tickets Sold (n)",
       ylab = "Objective Function Value",
       main = "Overbooking Optimization \n P(X <= N) - (1 - gamma)",
       col = ifelse(x_range == nd, "red", "blue"))

  abline(v = nd, col = "red", lty = 2, lwd = 2)

  legend("right",
         legend = c(sprintf("Optimal: n = %d", nd),
                    sprintf("N = %d, p = %.3f, gamma = %.3f", N, p, gamma)),
         col = c("red", "black"),
         lty = c(2, 0),
         lwd = c(2, 0),
         bty = "n",
         cex = 0.9)

  # Plot 2: Probability Curve
  plot(x_range, prob_values,
       type = "l",
       lwd = 2,
       col = "blue",
       xlab = "Number of Tickets Sold (n)",
       ylab = "Probability",
       main = "Probability of N or Fewer Passengers Showing",
       ylim = c(min(prob_values) * 0.95, 1))

  abline(h = 1 - gamma, col = "darkgreen", lty = 2, lwd = 2)

  points(nd, pbinom(N, size = nd, prob = p),
         pch = 19, col = "red", cex = 1.5)

  abline(v = nd, col = "red", lty = 3, lwd = 1)

  legend("right",
         legend = c(sprintf("Target: 1 - gamma = %.4f", 1 - gamma),
                    sprintf("Optimal: n = %d", nd),
                    sprintf("P(X <= N) = %.4f", pbinom(N, size = nd, prob = p))),
         col = c("darkgreen", "red", "red"),
         lty = c(2, 0, 0),
         pch = c(NA, 19, NA),
         lwd = c(2, NA, NA),
         bty = "n",
         cex = 0.9)

  # Return results as a named list
  result <- list(
    nd = nd,
    nc = nc,
    N = N,
    p = p,
    gamma = gamma
  )

  # Print results
  cat("Optimal Ticket Sales Calculation\n")
  cat(sprintf("Number of seats (N): %d\n", N))
  cat(sprintf("Show-up probability (p): %.4f\n", p))
  cat(sprintf("Overbooking threshold (gamma): %.4f\n", gamma))
  cat(sprintf("\nOptimal tickets to sell (discrete): %d\n", nd))
  cat(sprintf("Optimal tickets to sell (continuous): %.4f\n", nc))

  invisible(result) # returns but doesnt print
}
