#' @importFrom graphics curve
#' @importFrom stats formula
#'
#' @title Quadratic Curve Fitter
#'
#' @description Fit a Quadratic Curve to its data frame scatterplot
#'
#' @param lm a linear model with quadratic vector
#' @param ylim The value to scale y
#' @param xlim The value to scale x
#' @param dataframe A dataframe that corresponds to the model
#'
#' @returns scatter plot
#' @export
#'
#' @examples
#' data(cars)
#' car.lm <- lm(dist ~ speed + I(speed^2), data = cars)
#' fitQuadCurve(car.lm, c(0, 150), c(0, 30), cars)
fitQuadCurve = function(lm, ylim, xlim, dataframe) {
  # Extract variable names from the model
  formula_vars <- all.vars(formula(lm))
  y_var <- formula_vars[1]  # Response variable
  x_var <- formula_vars[2]  # Predictor variable

  # Create the plot using the actual variable names
  plot(dataframe[[x_var]], dataframe[[y_var]],
       bg = "Blue",
       pch = 21,
       cex = 1.2,
       ylim = ylim,
       xlim = xlim,
       main = "Quadratic Prediction",
       xlab = x_var,
       ylab = y_var)

  # Define the quadratic function using the provided model
  myplot = function(x) {
    lm$coef[1] + lm$coef[2] * x + lm$coef[3] * x^2
  }

  # Add the quadratic curve to the plot
  curve(myplot, lwd = 2, col = "steelblue", add = TRUE)

  # Return the plotting function (optional)
  return(myplot)
}
