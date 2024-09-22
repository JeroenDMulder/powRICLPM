test_that("icheck_plot_parameter() works", {

  out <- powRICLPM(
    target_power = 0.8,
    sample_size = 1000,
    time_points = c(3, 4),
    ICC = 0.5,
    RI_cor = 0.3,
    Phi = matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE),
    within_cor = 0.3,
    reps = 2,
    seed = 123456
  )

  expect_null(icheck_plot_parameter("wB2~wA1", out))
  expect_error(icheck_plot_parameter(object = out))
  expect_error(icheck_plot_parameter(c("wB2~wA1", "wA2~wB1"), out))
  expect_error(icheck_plot_parameter(12, out))
  expect_error(icheck_plot_parameter("wB4~wA3", out))
  expect_error(icheck_plot_parameter("wY2~wX1", out))
})

test_that("icheck_y() works", {
  expect_null(icheck_y("power"))
  expect_null(icheck_y("bias"))
  expect_error(icheck_y("sample_size"))
  expect_error(icheck_y(3))
})

test_that("icheck_plot_options() works", {
  expect_null(icheck_plot_options("time_points"))
  expect_error(icheck_plot_options("sample_size"))
  expect_error(icheck_plot_options(1))
  expect_error(icheck_plot_options(c("ICC", "time_points")))
})
