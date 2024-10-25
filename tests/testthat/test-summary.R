test_that("all columns from summary.powRICLMP(...) are named", {
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)
  out <- powRICLPM(
    target_power = 0.8,
    sample_size = c(500),
    time_points = c(3),
    ICC = c(0.4),
    reliability = c(1),
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = -0.42,
    reps = 2,
    seed = 1234
  )

  table_parameter <- summary(out, parameter = "wB2~wA1")
  expect_equal(colnames(table_parameter), c("Sample size", "Time points", "ICC", "Reliability", "Population", "Avg", "Bias", "Min", "SD", "SE Avg", "MSE", "Accuracy", "Cover", "Power", "Error", "Not converged", "Inadmissible"))

  table_condition <- summary(out, sample_size = 500, ICC = 0.4, time_points = 3, reliability = 1)
  expect_equal(colnames(table_condition), c("Population", "Avg", "Bias", "Min", "SD", "SE Avg", "MSE", "Accuracy", "Cover", "Power"))
})
