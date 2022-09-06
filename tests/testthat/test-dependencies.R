# Test seed functionality with furrr package ----
test_that("setting a seed in furrr_options() works", {

  # Create valid powRICLPM() input
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)

  out1 <- powRICLPM(
    target_power = 0.8,
    sample_size = 400,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = 0.3,
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
    within_cor = 0.3,
    reps = 2,
    seed = 123456, # Same seed
    bounds = TRUE
  )

  expect_equal(out1$conditions[[1]]$results$avg[1], out2$conditions[[1]]$results$avg[1])
})
