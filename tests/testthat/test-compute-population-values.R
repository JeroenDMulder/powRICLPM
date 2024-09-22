test_that("compute_Psi() works", {
  # Set lagged effects
  Phi <- matrix(c(.2, .15, .10, .3), ncol = 2, byrow = TRUE)

  # Compute residual (co)variances
  output <- compute_Psi(Phi = Phi, within_cor = 0.3)

  # Run tests
  expect_type(output, "double")
  expect_equal(dim(output), c(2, 2))
  expect_equal(eigen(output)$values > 0, c(TRUE, TRUE))
})

test_that("compute_RI_var() works", {
  expect_equal(compute_RI_var(0), 0)
  expect_equal(compute_RI_var(.99), 99)
})

test_that("compute_RI_cov() works", {
  expect_equal(compute_RI_cov(0.3, compute_RI_var(0.5)), 0.3)
  expect_equal(compute_RI_cov(0.5, compute_RI_var(0.20)), 0.125)
})
