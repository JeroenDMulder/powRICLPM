test_that("icheck_target() works", {
  expect_null(icheck_target(0.8))
  expect_error(icheck_target(1.4))
  expect_error(icheck_target("a"))
})

test_that("icheck_T() works", {
  expect_null(icheck_T(c(3, 4), ME = FALSE), c(3, 4))
  expect_error(icheck_T(3.5, ME = FALSE))
  expect_error(icheck_T(c(2, 3), ME = FALSE))
  expect_warning(icheck_T(c(3:30), ME = FALSE))
  expect_error(icheck_T(c(3, 4), ME = TRUE))
})

test_that("icheck_ICC() works", {
  expect_null(icheck_ICC(c(0.5, 0.8)))
  expect_error(icheck_ICC(2))
  expect_error(icheck_ICC("0.5"))
})

test_that("icheck_cor() works", {
  expect_null(icheck_cor(0.4))
  expect_error(icheck_cor(1.3))
  expect_error(icheck_cor("a"))
  expect_error(icheck_cor(c(0.3, 0.4)))
})

test_that("icheck_Phi() works", {
  m1 <- matrix(c(.3, .2, .15, .2), ncol = 2, byrow = TRUE)
  m2 <- matrix(c(.8, .5, .4, .9), ncol = 2, byrow = TRUE)

  expect_null(icheck_Phi(m1))
  expect_error(icheck_Phi("m1"))
  expect_error(icheck_Phi(m2))
})

test_that("icheck_reliability() works", {
  expect_null(icheck_rel(c(.8, .9)))
  expect_null(icheck_rel(.8))
  expect_error(icheck_rel(8))
  expect_error(icheck_rel("a"))
  expect_error(icheck_rel(-.8))
})

test_that("icheck_moment() works", {
  expect_null(icheck_moment(0.3))
  expect_error(icheck_moment("a"))
  expect_error(icheck_moment(c(0.2, 0.5)))
})

test_that("icheck_significance_criterion() works", {
  expect_null(icheck_significance_criterion(0.05))
  expect_error(icheck_significance_criterion(c(0.05, 0.10)))
  expect_error(icheck_significance_criterion(-0.05))
  expect_error(icheck_significance_criterion("a"))
})

test_that("icheck_ME() works", {
  expect_null(icheck_ME(TRUE))
  expect_error(icheck_ME(c(T, F)))
  expect_error(icheck_ME("T"))
  expect_error(icheck_ME(1))
})

test_that("icheck_reps() works", {
  expect_null(icheck_reps(1000))
  expect_error(icheck_reps("1000"))
  expect_error(icheck_reps(1000.5))
  expect_error(icheck_reps(-1000))
})

test_that("icheck_seed() works", {
  expect_equal(icheck_seed(1234), 1234)
  expect_warning(icheck_seed(NA))
  expect_error(icheck_seed("1234"))
  expect_error(icheck_seed(1234.5))
})

test_that("icheck_constraints() works", {
  expect_null(icheck_constraints("lagged", ME = F))
  expect_null(icheck_constraints("ME", ME = T))
  expect_error(icheck_constraints(1, ME = F))
  expect_error(icheck_constraints("a", ME = F))
  expect_error(icheck_constraints(TRUE, ME = F))
  expect_error(icheck_constraints(c("none", "ME"), ME = F))
  expect_error(icheck_constraints("ME", ME = F))
})

test_that("icheck_estimator() works", {
  expect_equal(icheck_estimator(NA, skewness = 0, kurtosis = 1), "MLR")
  expect_equal(icheck_estimator(NA, 0, 0), "ML")
  expect_equal(icheck_estimator(NA, skewness = 1, kurtosis = 0), "MLR")
  expect_error(icheck_estimator("a", skewness = 0, kurtosis = 0))
  expect_error(icheck_estimator(1, skewness = 0, kurtosis = 0))
})

test_that("icheck_path() works", {
  expect_equal(icheck_path(tempdir(), "lavaan"), tempdir())
  expect_equal(icheck_path(tempdir(), "Mplus"), tempdir())
  expect_error(icheck_path(1, "lavaan"))
  expect_error(icheck_path("non_existing_path"))
  expect_equal(icheck_path(NULL, "Mplus"), getwd())
})

test_that("icheck_N() works", {
  expect_null(icheck_N(c(200, 300), 3, constraints = "none", ME = FALSE))
  expect_error(icheck_N(c(200.4, 300), 3, constraints = "none", ME = FALSE))
  expect_error(icheck_N(c(-200, 300), 3, constraints = "none", ME = FALSE))
  expect_error(icheck_N(10, 3, constraints = "none", ME = FALSE))
  expect_null(icheck_N(17, 3, constraints = "lagged", ME = FALSE))
  expect_error(icheck_N(20, 3, constraints = "none", ME = TRUE))
  expect_null(icheck_N(20, 3, constraints = "within", ME = TRUE))
})

test_that("icheck_bounds() works", {
  expect_null(icheck_bounds(TRUE, "none", "lavaan"))
  expect_error(icheck_bounds("TRUE", "none", "Mplus"))
  expect_error(icheck_bounds(TRUE, "lagged", "lavaan"))
})

test_that("icheck_software() works", {
  expect_null(icheck_software("lavaan", 0, 0))
  expect_null(icheck_software("Mplus", 0, 0))
  expect_error(icheck_software(c("lavaan", "Mplus"), 0, 0))
  expect_error(icheck_software(TRUE))
  expect_error(icheck_software("Mplus", 1, 1))
})

