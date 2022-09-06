test_that("give() works", {

  # Create valid powRICLPM() input
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)

  # Test 1
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
  x_conditions <- give(out1, "conditions")
  x_problems <- give(out1, "estimation_problems")
  x_results <- give(out1, what = "results", parameter = "wB2~wA1")
  x_names <- give(out1, "names")

  # Run tests
  expect_error(give(1, "conditions"))
  expect_error(give(out1, "results"))

  expect_s3_class(x_conditions, "data.frame")
  expect_equal(dim(x_conditions), c(2, 3))

  expect_s3_class(x_problems, "data.frame")
  expect_equal(dim(x_problems), c(2, 6))
  expect_equal(
    names(x_problems),
    c(
      "sample_size", "time_points", "ICC",
      "errors", "not_converged", "inadmissible"
    )
  )

  expect_s3_class(x_results, "data.frame")
  expect_equal(dim(x_results), c(2, 12))
  expect_equal(
    names(x_results),
    c(
      "sample_size", "time_points", "ICC", "PV", "Avg", "Min",
      "stdDev", "SEAvg", "MSE", "Acc", "Cov", "Pow"
    )
  )

  expect_type(x_names, "character")
  expect_equal(length(x_names), 20)
})
