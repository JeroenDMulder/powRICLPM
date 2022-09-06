test_that("lavaan model syntax creation works", {
  # Create valid create_lavaan() input
  cond <- data.frame(
    sample_size = 200,
    time_points = 3,
    ICC = 0.3,
    RI_cor = 0.3,
    reliability = 1,
    RI_var = 0.4285714,
    RI_cov = 0.1285714,
    ME_var = 0
  )
  Phi <- matrix(c(.5, .1, .4, .5), ncol = 2, byrow = TRUE)
  wSigma <- matrix(c(1, .3, .3, 1), ncol = 2, byrow = TRUE)
  Psi <- matrix(c(0.71, -0.037, -0.037, 0.47), ncol = 2, byrow = TRUE)
  constraints <- "none"
  estimate_ME <- FALSE
  skewness <- 0
  kurtosis <- 0
  alpha <- 0.05

  # Generate lavaan parameter table
  lav_out <- powRICLPM:::create_lavaan(cond, Phi, wSigma, Psi, constraints, estimate_ME, skewness, kurtosis, alpha)

  # Test
  expect_type(lav_out, "list")
  expect_equal(length(lav_out), 18)
  expect_equal(names(lav_out), c(
    "sample_size", "time_points", "ICC", "RI_var", "RI_cov", "pop_synt", "pop_tab",
    "est_synt", "est_tab", "estimate_ME", "skewness", "kurtosis", "alpha", "estimates", "uncertainty",
    "errors", "not_converged", "inadmissible"
  ))
})
