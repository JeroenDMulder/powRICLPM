
# Test check_parameter_given() ----
test_that("check_parameter_given() works", {
  expect_equal(check_parameter_given("wB2~wA1"), "wB2~wA1")
  expect_error(check_parameter_given(NULL))
})
