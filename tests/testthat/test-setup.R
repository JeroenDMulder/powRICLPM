test_that("setup() works", {

  # Create valid setup() input
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)
  wSigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2, byrow = TRUE)
  Psi <- powRICLPM:::compute_Psi(Phi = Phi, wSigma = wSigma)

  # Run setup()
  out1 <- setup(
    target_power = .8,
    sample_size = c(400, 500),
    time_points = c(3, 4),
    ICC = c(.3, .5),
    RI_cor = .3,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    reliability = 1,
    skewness = 0,
    kurtosis = 0,
    est_ME = FALSE,
    alpha = .05,
    reps = 30,
    bootstrap_reps = 1000,
    seed = 1234,
    constraints = "none",
    bounds = FALSE,
    estimator = "ML"
  )

  # Run general tests
  expect_type(out1, "list")
  expect_equal(length(out1), 2)
  expect_equal(names(out1), c("conditions", "session"))

  # Test "conditions" element
  expect_equal(length(out1$conditions), 8)
  expect_type(out1$conditions[[1]], "list")
  expect_equal(names(out1$conditions[[1]]), c("sample_size", "time_points", "ICC", "RI_var", "RI_cov", "pop_synt", "pop_tab", "est_synt", "est_tab", "est_ME", "skewness", "kurtosis", "alpha", "estimates", "uncertainty", "errors", "not_converged", "inadmissible"))

  # Test first element of "conditions" element
  expect_equal(out1$conditions[[1]]$sample_size, 400)
  expect_equal(out1$conditions[[1]]$time_points, 3)
  expect_equal(out1$conditions[[1]]$ICC, 0.3)
  expect_equal(out1$conditions[[1]]$RI_var, 0.43, tolerance = 0.01)
  expect_equal(out1$conditions[[1]]$RI_cov, 0.129, tolerance = 0.01)
  expect_type(out1$conditions[[1]]$pop_synt, "character")
  expect_s3_class(out1$conditions[[1]]$pop_tab, "data.frame")
  expect_type(out1$conditions[[1]]$est_synt, "character")
  expect_s3_class(out1$conditions[[1]]$est_tab, "data.frame")

  # Test "session" element
  expect_type(out1$session, "list")
  expect_equal(length(out1$session), 10)
  expect_equal(names(out1$session), c("Psi", "reliability", "est_ME", "reps", "bootstrap_reps", "target_power", "constraints", "bounds", "estimator", "version"))
  expect_type(out1$session$Psi, "double")
  expect_type(out1$session$reps, "double")
  expect_type(out1$session$target_power, "double")
  expect_type(out1$session$constraints, "character")
  expect_type(out1$session$bounds, "logical")
})


