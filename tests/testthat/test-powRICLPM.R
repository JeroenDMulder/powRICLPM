test_that("basic power analysis using lavaan runs", {
  out1 <- powRICLPM(
    target_power = 0.8,
    sample_size = 1000,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE),
    within_cor = 0.3,
    reps = 2,
    seed = 123456
  )

  expect_equal(class(out1), c("powRICLPM", "list"))
  expect_equal(names(out1), c("conditions", "session"))
  expect_equal(length(out1$conditions), 1)
  expect_equal(
    c(
      "sample_size", "time_points", "ICC", "reliability", "RI_var", "RI_cov",
      "pop_synt", "pop_tab", "est_synt", "est_tab", "estimate_ME", "skewness",
      "kurtosis", "significance_criterion", "estimates", "MCSEs", "reps",
      "condition_id", "estimation_information"
    ) %in% names(out1$conditions[[1]]),
    rep(TRUE, times = 19)
  )
  expect_type(out1$conditions[[1]]$estimates, "list")
  expect_type(out1$conditions[[1]]$MCSEs, "list")
  expect_type(out1$conditions[[1]]$estimation_information, "list")

  test_summary_condition <- summary(out1, sample_size = 1000, time_points = 3, ICC = 0.5, reliability = 1)

  expect_equal(
    test_summary_condition$Population,
    c(1.000, 1.000, 0.300, 0.400, 0.150, 0.200, 0.300, 0.400, 0.150, 0.200, 0.300, 1.000, 1.000, 0.300, 0.781, 0.781, 0.834, 0.834, 0.130, 0.130)
  )
})

test_that("basic power analysis with multiple experimental conditions works", {
  out1 <- powRICLPM(
    target_power = 0.8,
    sample_size = c(400, 500),
    time_points = c(3, 4),
    ICC = c(0.4, 0.6),
    RI_cor = 0.3,
    Phi = matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE),
    within_cor = 0.3,
    reps = 2,
    seed = 123456
  )

  expect_equal(class(out1), c("powRICLPM", "list"))
  expect_equal(names(out1), c("conditions", "session"))
  expect_equal(length(out1$conditions), 8)
  expect_equal(
    c(
      "sample_size", "time_points", "ICC", "reliability", "RI_var", "RI_cov",
      "pop_synt", "pop_tab", "est_synt", "est_tab", "estimate_ME", "skewness",
      "kurtosis", "significance_criterion", "estimates", "MCSEs", "reps",
      "condition_id", "estimation_information"
    ) %in% names(out1$conditions[[1]]),
    rep(TRUE, times = 19)
  )

})

test_that("power analysis with constraints works", {
  out1 <- powRICLPM(
    target_power = 0.8,
    sample_size = 1000,
    time_points = 3,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE),
    within_cor = 0.3,
    reps = 2,
    seed = 123456,
    constraints = "within"
  )
  test_summary_condition <- summary(out1, sample_size = 1000, time_points = 3, ICC = 0.5, reliability = 1)

  expect_equal(
    test_summary_condition$Population,
    c(1.000, 1.000, 0.300, 0.400, 0.150, 0.200, 0.300, 0.400, 0.150, 0.200, 0.300, 1.000, 1.000, 0.300, 0.781, 0.781, 0.834, 0.834, 0.130, 0.130)
  )
})


test_that("power analysis for the STARTS model works", {
  expect_warning({
    out <- powRICLPM(
      target_power = 0.8,
      sample_size = c(500),
      time_points = 4,
      ICC = .5,
      RI_cor = 0.3,
      Phi = matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE),
      within_cor = 0.3,
      reliability = .85,
      estimate_ME = TRUE,
      reps = 1,
      seed = 1234
    )
  })

  expect_equal(out$session$estimate_ME, TRUE)
  expect_equal(
    c("A1~~A1", "A2~~A2", "B1~~B1", "B2~~B2") %in% out$conditions[[1]]$estimates$parameter,
    c(T, T, T, T)
  )
})

test_that("bounded estimation for STARTS model in powRICLPM() works", {

  expect_warning({
    out1 <- powRICLPM(
      target_power = 0.8,
      sample_size = c(500),
      time_points = 4,
      ICC = .5,
      RI_cor = 0.3,
      Phi = matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE),
      within_cor = 0.3,
      reliability = .85,
      estimate_ME = TRUE,
      bounds = TRUE,
      reps = 1,
      seed = 1234
    )
  })

  expect_true(out1$session$bounds)
})

test_that("power analysis using Mplus works", {
  powRICLPM(
    sample_size = 1000,
    time_points = 4,
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = matrix(c(.5, .1, .4, .5), ncol = 2, byrow = TRUE),
    within_cor = 0.3,
    reps = 1000,
    seed = 123456,
    save_path = tempdir(),
    software = "Mplus"
  )

  if (.Platform$OS.type %in% c("windows", "mac")) {
    path <- file.path(tempdir(), "Condition1.inp") |>
      normalizePath(mustWork = FALSE)

    expect_true(file.exists(path))
  }

})

test_that("power analysis for the STARTS model using Mplus works", {
  out_unconstrained <- powRICLPM(
    target_power = 0.8,
    sample_size = c(2000),
    time_points = 8,
    ICC = .5,
    RI_cor = 0.3,
    Phi = matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE),
    within_cor = 0.3,
    reliability = .85,
    estimate_ME = TRUE,
    reps = 2,
    seed = 1234,
    software = "Mplus",
    save_path = tempdir()
  )

  out_constrained <- powRICLPM(
    target_power = 0.8,
    sample_size = c(2000),
    time_points = 8,
    ICC = .5,
    RI_cor = 0.3,
    Phi = matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE),
    within_cor = 0.3,
    reliability = .85,
    estimate_ME = TRUE,
    reps = 2,
    seed = 1234,
    software = "Mplus",
    constraints = "ME",
    save_path = tempdir()
  )

  expect_error(
    powRICLPM(
      target_power = 0.8,
      sample_size = c(2000),
      time_points = 10,
      ICC = .5,
      RI_cor = 0.3,
      Phi = matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE),
      within_cor = 0.3,
      reliability = .85,
      estimate_ME = TRUE,
      reps = 2,
      seed = 1234,
      software = "Mplus",
      bounds = TRUE,
      save_path = tempdir()
    )
  )

})
