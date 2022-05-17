test_that("powRICLPM() works", {

  # Create valid powRICLPM() input
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)
  wSigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2, byrow = TRUE)

  # Base condition - Single experimental condition
  out1 <- powRICLPM(
    target_power = 0.8,
    sample_size = 1000,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    wSigma = wSigma,
    reps = 2,
    seed = 123456
  )

  # Run tests
  expect_equal(class(out1), c("powRICLPM", "list"))
  expect_equal(names(out1), c("conditions", "session"))

  # Test "conditions" element
  expect_equal(length(out1$conditions), 1)
  expect_equal(
    c("results", "errors", "not_converged", "inadmissible") %in% names(out1$conditions[[1]]),
    c(T, T, T, T)
  )
  expect_type(out1$conditions[[1]]$results, "list")
  expect_type(out1$conditions[[1]]$errors, "logical")
  expect_type(out1$conditions[[1]]$not_converged, "logical")
  expect_type(out1$conditions[[1]]$inadmissible, "logical")

  # Test "sessions" element
  expect_equal(out1$session$reps, 2)
  expect_equal(out1$session$target_power, 0.8)
  expect_equal(out1$session$save_path, NA)
  expect_null(out1$session$parameter)
  expect_equal(out1$session$constraints, "none")
  expect_false(out1$session$bounds)

  # Multiple experimental conditions
  out2 <- powRICLPM(
    target_power = 0.8,
    sample_size = c(100, 200),
    time_points = c(3, 4),
    ICC = c(0.4, 0.6),
    RI_cor = 0.3,
    Phi = Phi,
    wSigma = wSigma,
    reps = 2,
    seed = 123456
  )

  # Run tests
  expect_equal(class(out2), c("powRICLPM", "list"))
  expect_equal(names(out2), c("conditions", "session"))

  # Test "conditions" element
  expect_equal(length(out2$conditions), 8)
  expect_equal(
    c("results", "errors", "not_converged", "inadmissible") %in% names(out2$conditions[[6]]),
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
    wSigma = wSigma,
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
    wSigma = wSigma,
    reps = 10,
    seed = 123456,
    bounds = TRUE
  )

  expect_equal(class(out2), c("powRICLPM", "list"))
  expect_equal(names(out2), c("conditions", "session"))

  # Test "conditions" element
  expect_equal(length(out2$conditions), 1)
  expect_equal(
    c("results", "errors", "not_converged", "inadmissible") %in% names(out1$conditions[[1]]),
    c(T, T, T, T)
  )
  expect_type(out2$conditions[[1]]$results, "list")
  expect_type(out2$conditions[[1]]$errors, "logical")
  expect_type(out2$conditions[[1]]$not_converged, "logical")
  expect_type(out2$conditions[[1]]$inadmissible, "logical")

  # Test "session" element
  expect_true(out2$session$bounds)

  # Test that bounded estimation works
  expect_true(sum(out1$conditions[[1]]$not_converged) > sum(out2$conditions[[1]]$not_converged))
})

test_that("constrained estimation model in powRICLPM() works", {
  # Create valid powRICLPM() input
  Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)
  wSigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2, byrow = TRUE)

  out1 <- powRICLPM(
    target_power = 0.8,
    sample_size = 300,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    wSigma = wSigma,
    reps = 2,
    seed = 123456,
    bounds = FALSE,
    constraints = "lagged"
  )

  expect_equal(
    out1$conditions[[1]]$results$avg[which(out1$conditions[[1]]$results$par == "wB2~wA1")],
    out1$conditions[[1]]$results$avg[which(out1$conditions[[1]]$results$par == "wB3~wA2")]
  )

  out2 <- powRICLPM(
    target_power = 0.8,
    sample_size = 300,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    wSigma = wSigma,
    reps = 2,
    seed = 123456,
    bounds = FALSE,
    constraints = "residuals"
  )

  expect_equal(
    out2$conditions[[1]]$results$avg[which(out2$conditions[[1]]$results$par == "wA2~~wA2")],
    out2$conditions[[1]]$results$avg[which(out2$conditions[[1]]$results$par == "wA3~~wA3")]
  )

  out3 <- powRICLPM(
    target_power = 0.8,
    sample_size = 300,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    wSigma = wSigma,
    reps = 2,
    seed = 123456,
    bounds = FALSE,
    constraints = "within"
  )

  expect_equal(
    out3$conditions[[1]]$results$avg[which(out3$conditions[[1]]$results$par == "wB2~wA1")],
    out3$conditions[[1]]$results$avg[which(out3$conditions[[1]]$results$par == "wB3~wA2")]
  )
  expect_equal(
    out3$conditions[[1]]$results$avg[which(out3$conditions[[1]]$results$par == "wA2~~wA2")],
    out3$conditions[[1]]$results$avg[which(out3$conditions[[1]]$results$par == "wA3~~wA3")]
  )

  out4 <- powRICLPM(
    target_power = 0.8,
    sample_size = 300,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = Phi,
    wSigma = wSigma,
    reps = 2,
    seed = 123456,
    bounds = FALSE,
    constraints = "stationarity"
  )

  expect_equal(
    out4$conditions[[1]]$results$avg[which(out4$conditions[[1]]$results$par == "wB2~wA1")],
    out4$conditions[[1]]$results$avg[which(out4$conditions[[1]]$results$par == "wB3~wA2")],
    tolerance = 1e06
  )
})
