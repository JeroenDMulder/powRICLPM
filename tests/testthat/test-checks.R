
# Test check_T() ----
test_that("check_T() works", {
  expect_equal(check_T(c(3, 4), estimate_ME = FALSE), c(3, 4))
  expect_error(check_T(3.5, estimate_ME = FALSE))
  expect_error(check_T(c(2, 3), estimate_ME = FALSE))
  expect_warning(check_T(c(3:30), estimate_ME = FALSE))
  expect_error(check_T(c(3, 4), estimate_ME = TRUE))
})

# Test is_PD() ----
test_that("is_PD() works", {
  # Create sample matrices
  m1 <- matrix(c(.4, .1, .2, .3), ncol = 2, byrow = TRUE)
  m2 <- matrix(c(.3, .4, .3, .2), ncol = 2, byrow = TRUE)
  m4 <- matrix(c(.3, .4, -.3, .2), ncol = 2, byrow = TRUE)

  expect_equal(is_PD(m1), TRUE)
  expect_equal(is_PD(m2), FALSE)
  expect_equal(is_PD(m4), FALSE)
})

# Test is_unit() ----
test_that("is_unit() works", {
  # Create sample matrices
  m3 <- matrix(c(1.2, .4, .3, 1.5), ncol = 2, byrow = TRUE)

  # Test
  expect_equal(is_unit(m3), FALSE)
})

# Test check_N() ----
test_that("check_N() works", {
  expect_equal(check_N(c(200, 300), 3, constraints = "none", estimate_ME = FALSE), c(200, 300))
  expect_error(check_N(c(200.4, 300), 3, constraints = "none", estimate_ME = FALSE))
  expect_error(check_N(c(-200, 300), 3, constraints = "none", estimate_ME = FALSE))
  expect_error(check_N(10, 3, constraints = "none", estimate_ME = FALSE))
  expect_equal(check_N(17, 3, constraints = "lagged", estimate_ME = FALSE), 17)
  expect_error(check_N(20, 3, constraints = "none", estimate_ME = TRUE))
  expect_equal(check_N(20, 3, constraints = "within", estimate_ME = TRUE), 20)
})

# Test check_ICC() ----
test_that("check_ICC() works", {
  expect_equal(check_ICC(0.5), 0.5)
  expect_error(check_ICC(2))
  expect_error(check_ICC("0.5"))
})

# Test check_within_cor() ----
test_that("check_within_cor() works", {
  expect_equal(check_within_cor(0.3), matrix(c(1, .3, .3, 1), ncol = 2, byrow = TRUE))
  expect_error(check_within_cor("0.3"))
  expect_error(check_within_cor(1.1))
})

# Test check_Phi() ----
test_that("check_Phi_intern() works", {
  # Create sample matrices
  m1 <- matrix(c(.3, .2, .15, .2), ncol = 2, byrow = TRUE)
  m2 <- matrix(c(.8, .5, .4, .9), ncol = 2, byrow = TRUE)

  expect_equal(check_Phi_intern(m1), m1)
  expect_error(check_Phi_intern("m1"))
  expect_error(check_Phi_intern(m2))
})

# Test check_skewness() and check_kurtosis() ----
test_that("check_skewness() works", {
  expect_equal(check_skewness(0.5), 0.5)
  expect_error(check_skewness(c(0, 0.1, 0.6, 0.45)))
  expect_error(check_skewness(c("0", 0.4)))
})

test_that("check_kurtosis() works", {
  expect_equal(check_kurtosis(-0.5), -0.5)
  expect_error(check_kurtosis(c(0, 0.1, 0.6, 0.45)))
  expect_error(check_kurtosis(c("0", 0.4)))
})

# Test check_alpha() ----
test_that("check_alpha() works", {
  expect_equal(check_alpha(0.05), 0.05)
  expect_error(check_alpha("0.05"))
  expect_error(check_alpha(-0.05))
})

# Test check_seed() ----
test_that("check_seed() works", {
  expect_equal(check_seed(1234), 1234)
  expect_warning(check_seed(NA))
  expect_error(check_seed("1234"))
  expect_error(check_seed(1234.5))
})

# Test check_reps() ----
test_that("check_reps() works", {
  expect_equal(check_reps(1000), 1000)
  expect_error(check_reps("1000"))
  expect_error(check_reps(1000.5))
  expect_error(check_reps(-1000))
})

# Test check_target() ----
test_that("check_target() works", {
  expect_equal(check_target(0.80), 0.80)
  expect_error(check_target("0.80"))
  expect_error(check_target(1.4))
})

# Test check_parameter_argument() ----
test_that("check_parameter_argument() works", {
  expect_error(check_parameter_argument(1))
  expect_error(check_parameter_argument(c("a", "b")))
})

# Test check_object() ----
test_that("check_object() works", {
  x <- c("test")
  class(x) <- c("powRICLPM", class(x))
  expect_null(check_object(x))
  expect_error(check_object("test"))
})

# Test check_bounds() ----
test_that("check_bounds() works", {
  expect_error(check_bounds("TRUE", "none"))
  expect_true(check_bounds(TRUE, "none"))
  expect_false(check_bounds(FALSE, "none"))
  expect_false(check_bounds(FALSE, "lagged"))
  expect_error(check_bounds(TRUE, "lagged"))
})

# Test check_reliability() ----
test_that("check_reliability() works", {
  expect_error(check_reliability(8))
  expect_error(check_reliability("a"))
  expect_error(check_reliability(-.8))
  expect_error(check_reliability(c(.8, .9)))
  expect_equal(check_reliability(.8), .8)
})

# Test check_est_ME() ----
test_that("check_estimate_ME() works", {
  expect_error(check_estimate_ME(1))
  expect_error(check_estimate_ME("ME"))
  expect_error(check_estimate_ME(c(TRUE, TRUE)))
  expect_equal(check_estimate_ME(TRUE), TRUE)
})

# Test check_constraints() ----
test_that("check_constraints() works", {
  expect_error(check_constraints(1, estimate_ME = F))
  expect_error(check_constraints("a", estimate_ME = F))
  expect_error(check_constraints(TRUE, estimate_ME = F))
  expect_error(check_constraints(c("none", "ME"), estimate_ME = F))
  expect_error(check_constraints("ME", estimate_ME = F))
  expect_equal(check_constraints("lagged", estimate_ME = F), "lagged")
  expect_equal(check_constraints("ME", estimate_ME = T), "ME")
})

# Test check_estimator() ----
test_that("check_estimator() works", {
  expect_error(check_estimator("a", skewness = 0, kurtosis = 0))
  expect_error(check_estimator(1, skewness = 0, kurtosis = 0))
  expect_equal(check_estimator(NA, skewness = 1, kurtosis = 0), "MLR")
  expect_equal(check_estimator(NA, skewness = 0, kurtosis = 1), "MLR")
  expect_equal(check_estimator(NA, 0, 0), "ML")
})
