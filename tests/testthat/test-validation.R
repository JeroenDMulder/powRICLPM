if (FALSE) {
  test_that("Validation of results using Mplus", {

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
      reps = 1000,
      seed = 123456,
      save_path = "C:\\Users\\5879167\\surfdrive\\R packages\\powRICLPM_validation"
    )
  })
}
