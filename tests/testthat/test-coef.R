test_that("coef.powRICLPM() works", {

  # Create valid powRICLPM() input
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)
  wSigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2, byrow = TRUE)

  # Base condition - Single experimental condition
  out1 <- powRICLPM(
    target_power = 0.8,
    sample_size = c(300, 400),
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    wSigma = wSigma,
    reps = 2,
    seed = 123456
  )

  # Execute coef_powRICLPM()
  x <- coef(out1, parameter = "wB2~wA1")

  # Run tests
  expect_s3_class(x, "data.frame")
  expect_equal(dim(x), c(2, 14))
  expect_equal(
    names(x),
    c(
      "sample_size", "time_points", "ICC", "errors",
      "not_converged", "inadmissible", "pv", "avg",
      "stdDev", "SEAvg", "mse", "acc", "coverage", "pwr"
    )
  )
})
