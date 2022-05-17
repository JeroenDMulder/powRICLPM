test_that("setup() works", {

  # Create valid setup() input
  target_power <- 0.8
  sample_size <- c(300, 350, 400)
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
  out1 <- setup(
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

  # Run general tests
  expect_type(out1, "list")
  expect_equal(length(out1), 2)
  expect_equal(names(out1), c("conditions", "session"))

  # Test "conditions" element
  expect_equal(length(out1$conditions), length(sample_size))
  expect_type(out1$conditions[[1]], "list")
  expect_equal(names(out1$conditions[[1]]), c("sample_size", "time_points", "ICC", "RI_var", "RI_cov", "reps", "pop_synt", "pop_tab", "est_synt", "est_tab", "skewness", "kurtosis", "alpha", "save_path"))

  # Test first element of "conditions" element
  expect_equal(out1$conditions[[1]]$sample_size, 300)
  expect_equal(out1$conditions[[1]]$time_points, 3)
  expect_equal(out1$conditions[[1]]$ICC, 0.5)
  expect_equal(out1$conditions[[1]]$RI_var, 1)
  expect_equal(out1$conditions[[1]]$RI_cov, 0.3)
  expect_equal(out1$conditions[[1]]$reps, reps)
  expect_type(out1$conditions[[1]]$pop_synt, "character")
  expect_s3_class(out1$conditions[[1]]$pop_tab, "data.frame")
  expect_type(out1$conditions[[1]]$est_synt, "character")
  expect_s3_class(out1$conditions[[1]]$est_tab, "data.frame")

  # Test "session" element
  expect_type(out1$session, "list")
  expect_equal(length(out1$session), 7)
  expect_equal(names(out1$session), c("Psi", "reps", "target_power", "save_path", "parameter", "constraints", "bounds"))
  expect_type(out1$session$Psi, "double")
  expect_type(out1$session$reps, "double")
  expect_type(out1$session$target_power, "double")
  expect_equal(out1$session$save_path, NA)
  expect_type(out1$session$parameter, "character")
  expect_type(out1$session$constraints, "character")
  expect_type(out1$session$bounds, "logical")
})
