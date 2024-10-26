# Run single powRICLPM analysis
# Phi <- matrix(c(0.4, 0.15, 0.2, 0.3), ncol = 2, byrow = TRUE)
# out_preliminary <- powRICLPM(
#   target_power = 0.8,
#   sample_size = c(500, 700),
#   time_points = c(3, 4),
#   ICC = c(0.4, 0.6),
#   reliability = c(1, 0.8),
#   RI_cor = 0.3,
#   Phi = Phi,
#   within_cor = 0.3,
#   reps = 20,
#   seed = 1234
# )

# target_power = 0.8
# search_lower = NULL
# search_upper = NULL
# search_step = 20
# sample_size = c(500, 700)
# time_points = c(3, 4)
# ICC = c(0.4, 0.6)
# RI_cor = 0.3
# Phi = matrix(c(.4, .1, .2, .3), ncol = 2, byrow = TRUE)
# within_cor = 0.3
# reliability = 1
# skewness = 0
# kurtosis = 0
# estimate_ME = FALSE
# significance_criterion = 0.05
# reps = 20
# bootstrap_reps = NULL
# seed = NA
# constraints = "stationarity"
# bounds = FALSE
# estimator = "ML"
# save_path = NULL
# software = "lavaan"
# options_furrr = NULL
# Psi <- powRICLPM:::compute_Psi(Phi, within_cor)
