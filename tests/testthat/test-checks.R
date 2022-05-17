
# Test check_T() ----
test_that("check_T() works", {
  expect_equal(check_T(c(3, 4)), c(3, 4))
  expect_error(check_T(3.5))
  expect_error(check_T(c(2, 3)))
  expect_warning(check_T(c(3:30)))
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
  expect_equal(check_N(c(200, 300), 3), c(200, 300))
  expect_error(check_N(c(200.4, 300), 3))
  expect_error(check_N(c(-200, 300), 3))
  expect_error(check_N(10, 3))
})

# Test check_ICC() ----
test_that("check_ICC() works", {
  expect_equal(check_ICC(0.5), 0.5)
  expect_error(check_ICC(2))
  expect_error(check_ICC("0.5"))
})

# Test check_wSigma() ----
test_that("check_wSigma() works", {
  # Create sample matrices
  m1 <- matrix(c(1, .3, .3, 1), ncol = 2, byrow = TRUE)
  m2 <- matrix(c(.3, .4, .3, .2), ncol = 2, byrow = TRUE)
  m3 <- matrix(c(1, 2, 2, 1), ncol = 2, byrow = TRUE)

  expect_equal(check_wSigma(m1), m1)
  expect_error(check_wSigma(m2))
  expect_error(check_wSigma("m2"))
  expect_error(check_wSigma(m3))
})

# Test check_Phi() ----
test_that("check_Phi() works", {
  # Create sample matrices
  m1 <- matrix(c(.3, .2, .15, .2), ncol = 2, byrow = TRUE)
  m2 <- matrix(c(.8, .5, .4, .9), ncol = 2, byrow = TRUE)

  expect_equal(check_Phi(m1), m1)
  expect_error(check_Phi("m1"))
  expect_error(check_Phi(m2))
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

# Test check_parameter() ----
test_that("check_parameter() works", {
  expect_equal(check_parameter("wB2~wA1"), "wB2~wA1")
  expect_error(check_parameter(1234))
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

# Test check_parameter() ----
test_that("check_parameter() works", {
  expect_error(check_parameter(1))
  expect_equal(check_parameter("wB2~wA1"), "wB2~wA1")
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
