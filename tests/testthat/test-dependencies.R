# Test seed functionality with furrr package ----
test_that("setting a seed in furrr_options() works", {

  # Create valid powRICLPM() input
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)
  wSigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2, byrow = TRUE)

  out1 <- powRICLPM(
    target_power = 0.8,
    sample_size = 400,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    wSigma = wSigma,
    reps = 2,
    seed = 123456,
    bounds = FALSE
  )

  out2 <- powRICLPM(
    target_power = 0.8,
    sample_size = 400,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    wSigma = wSigma,
    reps = 2,
    seed = 123456, # Same seed
    bounds = TRUE
  )

  expect_equal(out1$conditions[[1]]$results$avg[1], out2$conditions[[1]]$results$avg[1])
})

# Test parameter order in lavaan package ----
test_that("lavaan() does not change order of parameters", {

  # Create valid powRICLPM() input
  time_points <- 3
  ICC <- 0.5
  RI_cor <- 0.3
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)
  wSigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2, byrow = TRUE)
  Psi <- compute_Psi(Phi = Phi, wSigma = wSigma)
  RI_var <- compute_RI_var(ICC)
  RI_cov <- compute_RI_cov(RI_cor = RI_cor, RI_var = RI_var)

  # Create lavaan syntax for generating data
  pop_synt <- create_lavaan(
    time_points = time_points,
    RI_var = RI_var,
    RI_cov = RI_cov,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    syntax = TRUE
  )

  # Create lavaan parameter table for population model
  pop_tab <- create_lavaan(
    time_points = time_points,
    RI_var = RI_var,
    RI_cov = RI_cov,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    estimation = FALSE
  )

  # Create lavaan syntax for estimating the model
  est_synt <- create_lavaan(
    time_points = time_points,
    RI_var = RI_var,
    RI_cov = RI_cov,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    estimation = TRUE,
    constraints = "none",
    syntax = TRUE
  )

  # Create lavaan syntax for estimating the model
  est_tab <- create_lavaan(
    time_points = time_points,
    RI_var = RI_var,
    RI_cov = RI_cov,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    estimation = TRUE,
    constraints = "none"
  )

  # Get index free parameters
  index_parameter_free <- est_tab$free == TRUE

  # Create parameter names from population model
  pop_par <- paste0(
    pop_tab$lhs[index_parameter_free],
    pop_tab$op[index_parameter_free],
    pop_tab$rhs[index_parameter_free]
  )

  # Generate data
  dat <- simulateData(pop_synt)

  # Fit model
  fit <- lavaan(est_synt, data = dat)

  # Create parameter names from fitted model
  fit_par <- paste0(
    lavaan::parameterestimates(fit)$lhs[index_parameter_free],
    lavaan::parameterestimates(fit)$op[index_parameter_free],
    lavaan::parameterestimates(fit)$rhs[index_parameter_free]
  )

  # Check if parameters are the same order
  expect_equal(pop_par, fit_par)
})

