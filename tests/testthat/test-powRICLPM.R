test_that("powRICLPM() works", {

  # Create valid powRICLPM() input
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)

  # Single experimental condition
  out1 <- powRICLPM(
    target_power = 0.8,
    sample_size = 1000,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = 0.3,
    reps = 2,
    seed = 123456
  )

  # Run tests
  expect_equal(class(out1), c("powRICLPM", "list"))
  expect_equal(names(out1), c("conditions", "session"))

  # Test "conditions" element
  expect_equal(length(out1$conditions), 1)
  expect_equal(
    c("estimates", "uncertainty", "errors", "not_converged", "inadmissible") %in% names(out1$conditions[[1]]),
    c(T, T, T, T, T)
  )
  expect_type(out1$conditions[[1]]$estimates, "list")
  expect_type(out1$conditions[[1]]$uncertainty, "list")
  expect_type(out1$conditions[[1]]$errors, "logical")
  expect_type(out1$conditions[[1]]$not_converged, "logical")
  expect_type(out1$conditions[[1]]$inadmissible, "logical")

  # Multiple experimental conditions
  out2 <- powRICLPM(
    target_power = 0.8,
    sample_size = c(100, 200),
    time_points = c(3, 4),
    ICC = c(0.4, 0.6),
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = 0.3,
    reps = 2,
    seed = 123456
  )

  # Run tests
  expect_equal(class(out2), c("powRICLPM", "list"))
  expect_equal(names(out2), c("conditions", "session"))

  # Test "conditions" element
  expect_equal(length(out2$conditions), 8)
  expect_equal(
    c("estimates", "uncertainty", "errors", "not_converged", "inadmissible") %in% names(out2$conditions[[6]]),
    c(T, T, T, T, T)
  )

  # Include measurement error (STARTS)
  out3 <- powRICLPM(
    target_power = 0.8,
    sample_size = c(200, 300),
    time_points = 4,
    ICC = .5,
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = 0.3,
    reliability = .85,
    estimate_ME = TRUE,
    reps = 2,
    seed = 1234
  )
  # Run tests
  expect_equal(out3$session$estimate_ME, TRUE)
  expect_equal(
    c("A1~~A1", "A2~~A2", "B1~~B1", "B2~~B2") %in% out3$conditions[[1]]$estimates$parameter,
    c(T, T, T, T)
  )
})

test_that("bounded estimation in powRICLPM() works", {
  # Create valid powRICLPM() input
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)
  wSigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2, byrow = TRUE)

  out1 <- powRICLPM(
    target_power = 0.8,
    sample_size = 21,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = 0.3,
    reps = 10,
    seed = 123456,
    bounds = FALSE
  )

  out2 <- powRICLPM(
    target_power = 0.8,
    sample_size = 21,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = 0.3,
    reps = 10,
    seed = 123456,
    bounds = TRUE
  )

  expect_equal(class(out2), c("powRICLPM", "list"))
  expect_equal(names(out2), c("conditions", "session"))

  # Test "conditions" element
  expect_equal(length(out2$conditions), 1)
  expect_equal(
    c("estimates", "uncertainty", "errors", "not_converged", "inadmissible") %in% names(out1$conditions[[1]]),
    c(T, T, T, T, T)
  )
  expect_type(out2$conditions[[1]]$estimates, "list")
  expect_type(out2$conditions[[1]]$uncertainty, "list")
  expect_type(out2$conditions[[1]]$errors, "logical")
  expect_type(out2$conditions[[1]]$not_converged, "logical")
  expect_type(out2$conditions[[1]]$inadmissible, "logical")

  # Test "session" element
  expect_true(out2$session$bounds)

  # Test that bounded estimation works
  expect_true(sum(out1$conditions[[1]]$not_converged) > sum(out2$conditions[[1]]$not_converged))
})
