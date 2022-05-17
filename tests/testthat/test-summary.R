test_that("powRICLPM_summary() works", {

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

  expect_snapshot(summary(out1))
  expect_snapshot(summary(out1, parameter = "wB2~wA1"))
  expect_snapshot(summary(out1, names = TRUE))
})
