# app.R
library(shiny)
library(ggplot2)
library(gridExtra)

# --- helper functions ------------------------------------------------------

# Log-likelihoods (sum of log pdf/pmf)
ll_normal <- function(params, x) {
  mu <- params[1]; sigma <- params[2]
  if (sigma <= 0) return(-Inf)
  sum(dnorm(x, mean = mu, sd = sigma, log = TRUE))
}
ll_exponential <- function(lambda, x) {
  if (lambda <= 0) return(-Inf)
  sum(dexp(x, rate = lambda, log = TRUE))
}
ll_poisson <- function(lambda, x) {
  if (lambda <= 0) return(-Inf)
  sum(dpois(x, lambda = lambda, log = TRUE))
}
ll_binomial <- function(p, x, size) {
  if (p <= 0 || p >= 1) return(-Inf)
  sum(dbinom(x, size = size, prob = p, log = TRUE))
}
# Gamma parameterization: shape, rate
ll_gamma <- function(params, x) {
  shape <- params[1]; rate <- params[2]
  if (shape <= 0 || rate <= 0) return(-Inf)
  sum(dgamma(x, shape = shape, rate = rate, log = TRUE))
}

# MLE wrappers (analytic where available)
mle_analytic <- function(dist, x, binom_size = NULL) {
  switch(dist,
         "Normal" = {
           mu_hat <- mean(x)
           sigma_hat <- sqrt(sum((x - mu_hat)^2) / length(x)) # MLE denominator n
           list(mu = mu_hat, sigma = sigma_hat)
         },
         "Exponential" = {
           lambda_hat <- 1 / mean(x)
           list(lambda = lambda_hat)
         },
         "Poisson" = {
           lambda_hat <- mean(x)
           list(lambda = lambda_hat)
         },
         "Binomial" = {
           # x must be counts (0..n)
           if (is.null(binom_size)) stop("binom_size required for Binomial analytic MLE")
           p_hat <- sum(x) / (binom_size * length(x))
           list(p = p_hat)
         },
         "Gamma" = {
           NULL # no simple analytic closed form for both params
         })
}

# Numeric MLE via optim (we maximize log-likelihood using negative)
mle_numeric <- function(dist, x, binom_size = NULL) {
  switch(dist,
         "Normal" = {
           init <- c(mean(x), sd(x))
           res <- optim(par = init,
                        fn = function(p) -ll_normal(p, x),
                        method = "L-BFGS-B",
                        lower = c(-Inf, 1e-8))
           list(mu = res$par[1], sigma = res$par[2], optim = res)
         },
         "Exponential" = {
           init <- 1 / mean(x)
           res <- optim(par = init,
                        fn = function(p) -ll_exponential(p, x),
                        method = "L-BFGS-B",
                        lower = 1e-8)
           list(lambda = res$par[1], optim = res)
         },
         "Poisson" = {
           init <- mean(x) + 1e-6
           res <- optim(par = init,
                        fn = function(p) -ll_poisson(p, x),
                        method = "L-BFGS-B",
                        lower = 1e-8)
           list(lambda = res$par[1], optim = res)
         },
         "Binomial" = {
           if (is.null(binom_size)) stop("binom_size required for Binomial numeric MLE")
           init <- sum(x) / (binom_size * length(x))
           res <- optim(par = init,
                        fn = function(p) -ll_binomial(p, x, size = binom_size),
                        method = "L-BFGS-B",
                        lower = 1e-8, upper = 1-1e-8)
           list(p = res$par[1], optim = res)
         },
         "Gamma" = {
           # Use method-of-moments for init
           m <- mean(x); v <- var(x)
           shape_init <- (m^2) / v
           rate_init <- m / v
           res <- optim(par = c(shape_init, rate_init),
                        fn = function(p) -ll_gamma(p, x),
                        method = "L-BFGS-B",
                        lower = c(1e-8, 1e-8))
           list(shape = res$par[1], rate = res$par[2], optim = res)
         })
}

# Helper to produce likelihood plotting grids
make_ll_grid_1d <- function(dist, x, param_name, grid) {
  vals <- sapply(grid, function(g) {
    switch(dist,
           "Exponential" = ll_exponential(g, x),
           "Poisson" = ll_poisson(g, x),
           "Binomial" = ll_binomial(g, x, size = attr(x, "binom_size")),
           stop("Unsupported 1D"))
  })
  data.frame(param = grid, ll = vals)
}

# For 2D (Normal and Gamma) create surface grid
make_ll_grid_2d <- function(dist, x, grid1, grid2) {
  z <- matrix(NA, nrow = length(grid1), ncol = length(grid2))
  for (i in seq_along(grid1)) {
    for (j in seq_along(grid2)) {
      p <- c(grid1[i], grid2[j])
      z[i, j] <- switch(dist,
                        "Normal" = ll_normal(p, x),
                        "Gamma" = ll_gamma(p, x),
                        NA)
    }
  }
  expand.grid(param1 = grid1, param2 = grid2, ll = as.vector(z))
}

# --- UI --------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("MLE Demonstrations for 5 Univariate Distributions"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose distribution:",
                  choices = c("Normal", "Exponential", "Poisson", "Binomial", "Gamma"),
                  selected = "Normal"),
      conditionalPanel("input.dist == 'Normal'",
                       numericInput("normal_mu", "True μ (mean):", value = 0),
                       numericInput("normal_sigma", "True σ (sd):", value = 1, min = 1e-6)),
      conditionalPanel("input.dist == 'Exponential'",
                       numericInput("exp_lambda", "True λ (rate):", value = 1, min = 1e-6)),
      conditionalPanel("input.dist == 'Poisson'",
                       numericInput("poisson_lambda", "True λ:", value = 3, min = 0)),
      conditionalPanel("input.dist == 'Binomial'",
                       numericInput("binom_size", "n (trials per obs):", value = 10, min = 1, step = 1),
                       numericInput("binom_p", "True p (success prob):", value = 0.3, min = 0, max = 1, step = 0.01)),
      conditionalPanel("input.dist == 'Gamma'",
                       numericInput("gamma_shape", "True shape (k):", value = 2, min = 1e-6),
                       numericInput("gamma_rate", "True rate (θ^-1):", value = 1, min = 1e-6)),
      hr(),
      numericInput("n", "Sample size (number of observations):", value = 200, min = 1, step = 1),
      numericInput("seed", "Random seed (optional, integer):", value = 1234, step = 1),
      actionButton("gen", "Generate sample & Compute MLEs"),
      hr(),
      checkboxInput("show_numeric", "Show numeric MLEs (optim) where applicable", value = TRUE),
      width = 3
    ),
    mainPanel(
      h4("Results"),
      verbatimTextOutput("mle_table"),
      hr(),
      fluidRow(
        column(6, plotOutput("hist_plot", height = "320px")),
        column(6, plotOutput("ll_plot", height = "320px"))
      ),
      hr(),
      h5("Optimization details (when numeric MLE used):"),
      verbatimTextOutput("optim_info")
    )
  )
)

# --- Server ----------------------------------------------------------------
server <- function(input, output, session) {
  data_and_results <- eventReactive(input$gen, {
    set.seed(input$seed)
    n <- input$n
    dist <- input$dist
    x <- NULL
    binom_size <- NULL
    true_params <- list()
    if (dist == "Normal") {
      x <- rnorm(n, mean = input$normal_mu, sd = input$normal_sigma)
      true_params <- list(mu = input$normal_mu, sigma = input$normal_sigma)
    } else if (dist == "Exponential") {
      x <- rexp(n, rate = input$exp_lambda)
      true_params <- list(lambda = input$exp_lambda)
    } else if (dist == "Poisson") {
      x <- rpois(n, lambda = input$poisson_lambda)
      true_params <- list(lambda = input$poisson_lambda)
    } else if (dist == "Binomial") {
      binom_size <- as.integer(input$binom_size)
      # sample of counts (0..n)
      x <- rbinom(n, size = binom_size, prob = input$binom_p)
      attr(x, "binom_size") <- binom_size
      true_params <- list(size = binom_size, p = input$binom_p)
    } else if (dist == "Gamma") {
      x <- rgamma(n, shape = input$gamma_shape, rate = input$gamma_rate)
      true_params <- list(shape = input$gamma_shape, rate = input$gamma_rate)
    }

    analytic <- tryCatch(mle_analytic(dist, x, binom_size = binom_size), error = function(e) NULL)
    numeric <- tryCatch(mle_numeric(dist, x, binom_size = binom_size), error = function(e) list(error = e$message))

    list(dist = dist, x = x, n = n, true = true_params,
         analytic = analytic, numeric = numeric, binom_size = binom_size)
  })

  output$mle_table <- renderPrint({
    res <- data_and_results()
    if (is.null(res)) return("Press 'Generate sample & Compute MLEs' to begin.")
    cat("Distribution:", res$dist, "\n")
    cat("Sample size:", res$n, "\n\n")
    cat("True parameters:\n")
    print(res$true)
    cat("\nAnalytic MLEs (if available):\n")
    if (!is.null(res$analytic)) print(res$analytic) else cat("No closed-form analytic MLE for both params.\n")
    cat("\nNumeric (optim) MLEs / results:\n")
    # Show numeric nicely
    if (!is.null(res$numeric$error)) {
      cat("Numeric MLE error:", res$numeric$error, "\n")
    } else {
      # drop optim object when printing small
      show <- res$numeric
      if (!is.null(show$optim)) show$optim <- list(convergence = show$optim$convergence, value = show$optim$value)
      print(show)
    }
    cat("\nNotes:\n")
    cat("- Analytic MLEs shown where available (e.g., Normal, Exponential, Poisson, Binomial p).\n")
    cat("- For Gamma we use numerical optimization (optim) to obtain shape & rate.\n")
  })

  output$hist_plot <- renderPlot({
    res <- data_and_results()
    if (is.null(res)) return()
    x <- res$x
    dist <- res$dist
    df <- data.frame(x = x)
    p <- NULL

    if (dist %in% c("Normal", "Exponential", "Gamma")) {
      # continuous: histogram + fitted density overlays
      p <- ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.4, fill = "grey70", color = NA) +
        labs(x = "x", y = "Density", title = paste(dist, "sample and fitted density"))
      # add true density
      if (dist == "Normal") {
        p <- p + stat_function(fun = dnorm, args = list(mean = res$true$mu, sd = res$true$sigma),
                               size = 1, linetype = "dashed") +
          annotate("text", x = quantile(x, 0.95), y = max(density(x)$y)*0.9,
                   label = sprintf("True: μ=%.3g\nσ=%.3g", res$true$mu, res$true$sigma), hjust = 1)
        # fitted analytic
        if (!is.null(res$analytic)) {
          p <- p + stat_function(fun = dnorm, args = list(mean = res$analytic$mu, sd = res$analytic$sigma),
                                 size = 1, linetype = "solid")
        }
        # numeric
        if (input$show_numeric && !is.null(res$numeric$mu)) {
          p <- p + stat_function(fun = dnorm, args = list(mean = res$numeric$mu, sd = res$numeric$sigma),
                                 size = 1, linetype = "dotdash")
        }
      } else if (dist == "Exponential") {
        p <- p + stat_function(fun = dexp, args = list(rate = res$true$lambda), size = 1, linetype = "dashed") +
          annotate("text", x = quantile(x, 0.95), y = max(density(x)$y)*0.9,
                   label = sprintf("True: λ=%.3g", res$true$lambda), hjust = 1)
        if (!is.null(res$analytic)) {
          p <- p + stat_function(fun = dexp, args = list(rate = res$analytic$lambda), size = 1, linetype = "solid")
        }
        if (input$show_numeric && !is.null(res$numeric$lambda)) {
          p <- p + stat_function(fun = dexp, args = list(rate = res$numeric$lambda), size = 1, linetype = "dotdash")
        }
      } else if (dist == "Gamma") {
        p <- p + stat_function(fun = dgamma, args = list(shape = res$true$shape, rate = res$true$rate),
                               size = 1, linetype = "dashed") +
          annotate("text", x = quantile(x, 0.95), y = max(density(x)$y)*0.9,
                   label = sprintf("True: shape=%.3g\nrate=%.3g", res$true$shape, res$true$rate), hjust = 1)
        if (input$show_numeric && !is.null(res$numeric$shape)) {
          p <- p + stat_function(fun = dgamma, args = list(shape = res$numeric$shape, rate = res$numeric$rate),
                                 size = 1, linetype = "solid")
        }
      }
    } else if (dist %in% c("Poisson", "Binomial")) {
      # discrete: bar plot of counts + fitted pmf lines
      tab <- as.data.frame(table(x))
      tab$x <- as.numeric(as.character(tab$x))
      p <- ggplot(tab, aes(x = x, y = Freq / sum(Freq))) +
        geom_col(fill = "grey70", alpha = 0.8) +
        labs(x = "k", y = "Probability", title = paste(dist, "sample pmf and fitted pmf"))
      if (dist == "Poisson") {
        k <- min(tab$x):max(tab$x)
        true_pmf <- dpois(k, lambda = res$true$lambda)
        p <- p + geom_point(data = data.frame(k = k, p = true_pmf), aes(k, p), color = "blue", size = 2) +
          geom_line(data = data.frame(k = k, p = true_pmf), aes(k, p), color = "blue")
        if (!is.null(res$numeric$lambda)) {
          fit_pmf <- dpois(k, lambda = res$numeric$lambda)
          p <- p + geom_point(data = data.frame(k = k, p = fit_pmf), aes(k, p), color = "red", shape = 3, size = 2) +
            geom_line(data = data.frame(k = k, p = fit_pmf), aes(k, p), color = "red", linetype = "dashed")
        }
      } else {
        # Binomial
        size <- res$binom_size
        k <- min(tab$x):max(tab$x)
        true_pmf <- dbinom(k, size = size, prob = res$true$p)
        p <- p + geom_point(data = data.frame(k = k, p = true_pmf), aes(k, p), color = "blue", size = 2) +
          geom_line(data = data.frame(k = k, p = true_pmf), aes(k, p), color = "blue")
        if (input$show_numeric && !is.null(res$numeric$p)) {
          fit_pmf <- dbinom(k, size = size, prob = res$numeric$p)
          p <- p + geom_point(data = data.frame(k = k, p = fit_pmf), aes(k, p), color = "red", shape = 3, size = 2) +
            geom_line(data = data.frame(k = k, p = fit_pmf), aes(k, p), color = "red", linetype = "dashed")
        }
      }
    }

    p + theme_minimal()
  })

  output$ll_plot <- renderPlot({
    res <- data_and_results()
    if (is.null(res)) return()
    x <- res$x; dist <- res$dist
    # 1D likelihoods (Exponential, Poisson, Binomial) and 2D surface for Normal/Gamma
    if (dist == "Exponential") {
      grid <- seq(max(1e-6, 0.1 * (1/mean(x))), 5 * (1/mean(x) + 1e-6), length.out = 200)
      df <- make_ll_grid_1d(dist, x, "lambda", grid)
      ggplot(df, aes(x = param, y = ll)) +
        geom_line() + geom_vline(xintercept = if (!is.null(res$analytic)) res$analytic$lambda else NA, linetype = "dashed") +
        geom_vline(xintercept = if (input$show_numeric && !is.null(res$numeric$lambda)) res$numeric$lambda else NA, linetype = "dotted") +
        labs(x = "lambda", y = "Log-Likelihood", title = "Log-likelihood for Exponential (λ)") + theme_minimal()
    } else if (dist == "Poisson") {
      grid <- seq(max(1e-6, 0.1 * mean(x)), max(1, 2 * mean(x)), length.out = 200)
      df <- make_ll_grid_1d(dist, x, "lambda", grid)
      ggplot(df, aes(x = param, y = ll)) +
        geom_line() + geom_vline(xintercept = if (!is.null(res$analytic)) res$analytic$lambda else NA, linetype = "dashed") +
        geom_vline(xintercept = if (input$show_numeric && !is.null(res$numeric$lambda)) res$numeric$lambda else NA, linetype = "dotted") +
        labs(x = "lambda", y = "Log-Likelihood", title = "Log-likelihood for Poisson (λ)") + theme_minimal()
    } else if (dist == "Binomial") {
      grid <- seq(1e-3, 1 - 1e-3, length.out = 400)
      attr(x, "binom_size") <- res$binom_size
      df <- make_ll_grid_1d(dist, x, "p", grid)
      ggplot(df, aes(x = param, y = ll)) +
        geom_line() + geom_vline(xintercept = if (!is.null(res$analytic)) res$analytic$p else NA, linetype = "dashed") +
        geom_vline(xintercept = if (input$show_numeric && !is.null(res$numeric$p)) res$numeric$p else NA, linetype = "dotted") +
        labs(x = "p", y = "Log-Likelihood", title = "Log-likelihood for Binomial (p)") + theme_minimal()
    } else if (dist == "Normal") {
      # 2D surface over mu and sigma
      mu_range <- seq(mean(x) - 2 * sd(x), mean(x) + 2 * sd(x), length.out = 100)
      sigma_range <- seq(max(1e-3, sd(x) * 0.2), sd(x) * 2.5, length.out = 100)
      grid_df <- make_ll_grid_2d("Normal", x, mu_range, sigma_range)
      # convert to matrix for contour
      zmat <- matrix(grid_df$ll, nrow = length(mu_range), ncol = length(sigma_range))
      filled.contour(x = mu_range, y = sigma_range, z = zmat,
                     color.palette = terrain.colors,
                     xlab = "mu", ylab = "sigma",
                     main = "Log-likelihood surface (Normal)")
    } else if (dist == "Gamma") {
      # 2D surface over shape and rate
      m <- mean(x); v <- var(x)
      shape_init <- max(1e-3, (m^2) / v)
      rate_init <- max(1e-3, m / v)
      shape_range <- seq(max(1e-3, shape_init * 0.2), shape_init * 4 + 0.001, length.out = 80)
      rate_range <- seq(max(1e-3, rate_init * 0.2), rate_init * 4 + 0.001, length.out = 80)
      grid_df <- make_ll_grid_2d("Gamma", x, shape_range, rate_range)
      zmat <- matrix(grid_df$ll, nrow = length(shape_range), ncol = length(rate_range))
      filled.contour(x = shape_range, y = rate_range, z = zmat,
                     color.palette = terrain.colors,
                     xlab = "shape", ylab = "rate",
                     main = "Log-likelihood surface (Gamma)")
    }
  })

  output$optim_info <- renderPrint({
    res <- data_and_results()
    if (is.null(res)) return()
    if (input$show_numeric && !is.null(res$numeric$optim)) {
      print(res$numeric$optim)
    } else {
      cat("Numeric optimization not shown or not run.\n")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
