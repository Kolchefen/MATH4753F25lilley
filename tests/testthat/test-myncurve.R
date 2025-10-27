test_that("myncurve returns correct mu", {
  result <- myncurve(mu = 5, sigma = 2, a = 6)
  expect_equal(result$mu, 5)
})

test_that("myncurve returns correct sigma", {
  result <- myncurve(mu = 5, sigma = 2, a = 6)
  expect_equal(result$sigma, 2)
})

test_that("myncurve calculates correct area", {
  result <- myncurve(mu = 0, sigma = 1, a = 0)
  expect_equal(result$area, 0.5)

  # Another test
  result2 <- myncurve(mu = 10, sigma = 2, a = 10)
  expect_equal(result2$area, 0.5)  # At the mean, should be 0.5
})
