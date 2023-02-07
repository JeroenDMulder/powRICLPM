test_that("run() works", {

  # Create valid input
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)
  wSigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2, byrow = TRUE)
  Psi <- powRICLPM:::compute_Psi(Phi = Phi, wSigma = wSigma)

  # Run setup()
  setup1 <- powRICLPM:::setup(
    target_power = .8,
    sample_size = 300,
    time_points = 3,
    ICC = .5,
    RI_cor = .3,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    reliability = .85,
    skewness = 0,
    kurtosis = 0,
    estimate_ME = FALSE,
    alpha = .05,
    reps = 10,
    bootstrap_reps = 1000,
    seed = 1234,
    constraints = "none",
    bounds = FALSE,
    estimator = "ML",
    save_path = NULL
  )

  # Prepare progress bar
  progressr::with_progress({
    p <- progressr::progressor(along = setup1$conditions)

    # Run run_condition()
    out1 <- run_condition(
      condition = setup1$conditions[[1]],
      p = p,
      bounds = FALSE,
      estimator = "ML",
      reps = 10,
      bootstrap_reps = 1000,
      save_path = NULL
    )
  })

  # Test output
  expect_equal(length(out1), 18)
  expect_equal(names(out1), c(
    "sample_size", "time_points", "ICC", "RI_var",
    "RI_cov", "pop_synt", "pop_tab",
    "est_synt", "est_tab", "estimate_ME", "skewness", "kurtosis",
    "alpha", "estimates", "uncertainty", "errors",
    "not_converged", "inadmissible"
  ))
  expect_equal(length(out1$inadmissible), 10)
  expect_type(out1$estimates, "list")
  expect_type(out1$errors, "logical")
  expect_type(out1$not_converged, "logical")
  expect_type(out1$inadmissible, "logical")
  expect_equal(dim(out1$estimates), c(20, 10))
})

test_that("run() works for bivariate STARTS model", {

  # Create valid input
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)
  wSigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2, byrow = TRUE)
  Psi <- compute_Psi(Phi = Phi, wSigma = wSigma)

  # Run setup()
  setup2 <- setup(
    target_power = .8,
    sample_size = 300,
    time_points = 4,
    ICC = .5,
    RI_cor = .3,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    reliability = .85,
    skewness = 0,
    kurtosis = 0,
    estimate_ME = TRUE,
    alpha = .05,
    reps = 10,
    bootstrap_reps = 1000,
    seed = 1234,
    constraints = "within",
    bounds = FALSE,
    estimator = "ML",
    save_path = NULL
  )

  # Prepare progress bar
  progressr::with_progress({
    p <- progressr::progressor(along = setup2$conditions)

    # Run run_condition()
    out2 <- run_condition(
      condition = setup2$conditions[[1]],
      p = p,
      bounds = FALSE,
      estimator = "ML",
      reps = 10,
      bootstrap_reps = 1000,
      save_path = NULL
    )
  })

  # Test output
  expect_equal(length(out2), 18)
  expect_equal(names(out2), c(
    "sample_size", "time_points", "ICC", "RI_var",
    "RI_cov", "pop_synt", "pop_tab",
    "est_synt", "est_tab", "estimate_ME", "skewness", "kurtosis",
    "alpha", "estimates", "uncertainty", "errors",
    "not_converged", "inadmissible"
  ))
  expect_equal(length(out2$inadmissible), 10)
  expect_type(out2$estimates, "list")
  expect_type(out2$errors, "logical")
  expect_type(out2$not_converged, "logical")
  expect_type(out2$inadmissible, "logical")
  expect_equal(dim(out2$estimates), c(35, 10))
})
