test_that("constraints in powRICLPM() work", {

  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)

  out_lagged <- powRICLPM(
    target_power = 0.8,
    sample_size = 300,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = 0.3,
    reps = 2,
    seed = 123456,
    constraints = "lagged"
  )

  # Cross-lagged effects
  expect_equal(
    out_lagged$conditions[[1]]$estimates$average[which(out_lagged$conditions[[1]]$estimates$parameter == "wB2~wA1")],
    out_lagged$conditions[[1]]$estimates$average[which(out_lagged$conditions[[1]]$estimates$parameter == "wB3~wA2")]
  )

  # Autoregressive effects
  expect_equal(
    out_lagged$conditions[[1]]$estimates$average[which(out_lagged$conditions[[1]]$estimates$parameter == "wA2~wA1")],
    out_lagged$conditions[[1]]$estimates$average[which(out_lagged$conditions[[1]]$estimates$parameter == "wA3~wA2")]
  )

  out_residuals <- powRICLPM(
    target_power = 0.8,
    sample_size = 300,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = 0.3,
    reps = 2,
    seed = 123456,
    constraints = "residuals"
  )

  expect_equal(
    out_residuals$conditions[[1]]$estimates$average[which(out_residuals$conditions[[1]]$estimates$parameter == "wA2~~wA2")],
    out_residuals$conditions[[1]]$estimates$average[which(out_residuals$conditions[[1]]$estimates$parameter == "wA3~~wA3")]
  )

  out_within <- powRICLPM(
    target_power = 0.8,
    sample_size = 300,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = 0.3,
    reps = 2,
    seed = 123456,
    constraints = "within"
  )

  # Cross-lagged effects
  expect_equal(
    out_within$conditions[[1]]$estimates$average[which(out_within$conditions[[1]]$estimates$parameter == "wB2~wA1")],
    out_within$conditions[[1]]$estimates$average[which(out_within$conditions[[1]]$estimates$parameter == "wB3~wA2")]
  )

  # Autoregressive effects
  expect_equal(
    out_within$conditions[[1]]$estimates$average[which(out_within$conditions[[1]]$estimates$parameter == "wA2~wA1")],
    out_within$conditions[[1]]$estimates$average[which(out_within$conditions[[1]]$estimates$parameter == "wA3~wA2")]
  )

  # Residual variances
  expect_equal(
    out_within$conditions[[1]]$estimates$average[which(out_within$conditions[[1]]$estimates$parameter == "wA2~~wA2")],
    out_within$conditions[[1]]$estimates$average[which(out_within$conditions[[1]]$estimates$parameter == "wA3~~wA3")]
  )

  out_stat <- powRICLPM(
    target_power = 0.8,
    sample_size = 300,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = 0.3,
    reps = 2,
    seed = 123456,
    constraints = "stationarity"
  )

  # Cross-lagged effects
  expect_equal(
    out_stat$conditions[[1]]$estimates$average[which(out_stat$conditions[[1]]$estimates$parameter == "wB2~wA1")],
    out_stat$conditions[[1]]$estimates$average[which(out_stat$conditions[[1]]$estimates$parameter == "wB3~wA2")],
    tolerance = 1e06
  )

  # Autoregressive effects
  expect_equal(
    out_stat$conditions[[1]]$estimates$average[which(out_stat$conditions[[1]]$estimates$parameter == "wA2~wA1")],
    out_stat$conditions[[1]]$estimates$average[which(out_stat$conditions[[1]]$estimates$parameter == "wA3~wA2")],
    tolerance = 1e06
  )

  out_ME <- powRICLPM(
    target_power = 0.8,
    sample_size = 1000,
    time_points = 4,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = 0.3,
    reps = 2,
    seed = 123456,
    constraints = "ME",
    estimate_ME = TRUE
  )

  expect_equal(
    out_ME$conditions[[1]]$estimates$average[which(out_ME$conditions[[1]]$estimates$parameter == "A1~~A1")],
    out_ME$conditions[[1]]$estimates$average[which(out_ME$conditions[[1]]$estimates$parameter == "A2~~A2")]
  )

  #expect_false(
  #  out_ME$conditions[[1]]$estimates$average[which(out_ME$conditions[[1]]$estimates$parameter == "A1~~A1")] ==
  #  out_ME$conditions[[1]]$estimates$average[which(out_ME$conditions[[1]]$estimates$parameter == "B2~~B2")]
  #)
})

