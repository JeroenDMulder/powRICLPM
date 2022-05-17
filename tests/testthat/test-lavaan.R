test_that("lavaan model syntax creation works", {
  # Create valid create_lavaan() input
  Phi <- matrix(c(.5, .1, .4, .5), ncol = 2, byrow = TRUE)
  wSigma <- matrix(c(1, .3, .3, 1), ncol = 2, byrow = TRUE)
  Psi <- matrix(c(0.71, -0.037, -0.037, 0.47), ncol = 2, byrow = TRUE)
  ICC <- 0.5
  RI_cor <- 0.3
  RI_var <- compute_RI_var(ICC)
  RI_cov <- compute_RI_cov(RI_cor = RI_cor, RI_var = RI_var)
  constraints <- "stationarity"

  # Generate lavaan parameter table
  pop_tab <- create_lavaan(
    time_points = 3,
    RI_var = RI_var,
    RI_cov = RI_cov,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    estimation = FALSE,
    constraints = constraints
  )

  # Generate lavaan model syntax
  pop_synt <- create_lavaan(
    time_points = 3,
    RI_var = RI_var,
    RI_cov = RI_cov,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    syntax = TRUE,
    estimation = FALSE,
    constraints = constraints
  )

  # Create lavaan syntax for estimating a model
  est_synt <- create_lavaan(
    time_points = 3,
    RI_var = RI_var,
    RI_cov = RI_cov,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    syntax = TRUE,
    estimation = TRUE,
    constraints = constraints
  )

  # Create lavaan parameter table for estimating model
  est_tab <- create_lavaan(
    time_points = 3,
    RI_var = RI_var,
    RI_cov = RI_cov,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    estimation = TRUE,
    constraints = constraints
  )

  # Test parameter tables
  expect_type(pop_tab, "list")
  expect_type(est_tab, "list")

  expect_equal(dim(pop_tab), c(38, 6))
  expect_equal(dim(est_tab), c(44, 6))

  # Test syntax
  expect_type(pop_synt, "character")
  expect_equal(length(pop_synt), 1)

  expect_type(est_synt, "character")
  expect_equal(length(est_synt), 1)
})
