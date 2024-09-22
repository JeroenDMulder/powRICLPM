test_that("is_PD() works", {
  # Create sample matrices
  m1 <- matrix(c(.4, .1, .2, .3), ncol = 2, byrow = TRUE)
  m2 <- matrix(c(.3, .4, .3, .2), ncol = 2, byrow = TRUE)
  m4 <- matrix(c(.3, .4, -.3, .2), ncol = 2, byrow = TRUE)

  expect_equal(is_PD(m1), TRUE)
  expect_equal(is_PD(m2), FALSE)
  expect_equal(is_PD(m4), FALSE)
})

test_that("is_unit() works", {
  # Create sample matrices
  m3 <- matrix(c(1.2, .4, .3, 1.5), ncol = 2, byrow = TRUE)

  expect_equal(is_unit(m3), FALSE)
})


