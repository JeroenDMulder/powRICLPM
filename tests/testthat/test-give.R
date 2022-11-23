test_that("give() works", {

  # Create valid powRICLPM() input
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)

  out1 <- powRICLPM(
    target_power = 0.8,
    sample_size = c(300, 400),
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = 0.3,
    reps = 2,
    seed = 123456
  )

  # Execute give()
  df_conditions <- give(out1, "conditions")
  df_problems <- give(out1, "estimation_problems")
  df_results <- give(out1, what = "results", parameter = "wB2~wA1")
  df_names <- give(out1, "names")

  # Run tests
  expect_error(give(1, "conditions"))
  expect_error(give(out1, "results"))

  expect_s3_class(df_conditions, "data.frame")
  expect_equal(dim(df_conditions), c(2, 3))

  expect_s3_class(df_problems, "data.frame")
  expect_equal(dim(df_problems), c(2, 6))
  expect_equal(
    names(df_problems),
    c(
      "sample_size", "time_points", "ICC",
      "errors", "not_converged", "inadmissible"
    )
  )

  expect_s3_class(df_results, "data.frame")
  expect_equal(dim(df_results), c(2, 12))
  expect_equal(
    names(df_results),
    c(
      "sample_size", "time_points", "ICC", "population_value", "average",
      "minimum", "SD", "SEAvg", "MSE", "accuracy", "coverage", "power"
    )
  )

  expect_type(df_names, "character")
  expect_equal(length(df_names), 20)
})
