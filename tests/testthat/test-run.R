test_that("run() works", {

  # Create valid input
  target_power <- 0.8
  sample_size <- 300
  time_points <- 3
  ICC <- 0.5
  RI_cor <- 0.3
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)
  wSigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2, byrow = TRUE)
  Psi <- compute_Psi(Phi = Phi, wSigma = wSigma)
  skewness <- 0
  kurtosis <- 0
  alpha <- 0.1
  reps <- 30
  seed <- 123456
  save_path <- NA
  parameter <- "wB2~wA1"
  constraints <- "none"
  bounds <- FALSE

  # Run setup()
  setup1 <- setup(
    target_power = target_power,
    sample_size = sample_size,
    time_points = time_points,
    ICC = ICC,
    RI_cor = RI_cor,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    skewness = 0,
    kurtosis = 0,
    alpha = alpha,
    reps = reps,
    seed = seed,
    save_path = save_path,
    parameter = "wB2~wA1",
    constraints = constraints,
    bounds = bounds
  )

  # Prepare progress bar
  progressr::with_progress({
    p <- progressr::progressor(along = setup1$conditions)

    # Run run_condition()
    out1 <- run_condition(
      object = setup1$conditions[[1]],
      progress = p,
      bounds = FALSE
    )
  })

  # Test output
  expect_equal(length(out1), 18)
  expect_equal(names(out1), c(
    "sample_size", "time_points", "ICC", "RI_var",
    "RI_cov", "reps", "pop_synt", "pop_tab",
    "est_synt", "est_tab", "skewness", "kurtosis",
    "alpha", "save_path", "results", "errors",
    "not_converged", "inadmissible"
  ))
  expect_equal(length(out1$inadmissible), reps)
  expect_type(out1$results, "list")
  expect_type(out1$errors, "logical")
  expect_type(out1$not_converged, "logical")
  expect_type(out1$inadmissible, "logical")
  expect_equal(dim(out1$results), c(20, 9))
})
