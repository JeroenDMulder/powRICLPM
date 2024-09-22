#' Compute Monte Carlo Standard Error of Bias
#'
#' Compute Monte Carlo SE of bias based on Morris et al. (2019).
#'
#' @noRd
#' @references
#' Morris, T. P., White, Ian R., Crowther, Michael J. (2017). Using simulation studies to evaluate statistical methods. Statistics in Medicine, 38, 2074-2102. \url{https://doi.org/10.1002/sim.8086}
compute_MCSE_bias <- function(thetas_hat, thetas_bar) {

  # Compute sum of squared differences for each parameter
  sum_squared_diff <- rowSums((thetas_hat - thetas_bar)^2)

  # Number of replications
  reps <- ncol(thetas_hat)

  # Compute MC SE of bias
  out <- sqrt((1 / (reps * (reps - 1))) * sum_squared_diff)
  return(out)
}

#' Compute Monte Carlo Standard Error of MSE
#'
#' Compute Monte Carlo SE of mean square error based on Morris et al. (2019).
#'
#' @noRd
#' @references
#' Morris, T. P., White, Ian R., Crowther, Michael J. (2017). Using simulation studies to evaluate statistical methods. Statistics in Medicine, 38, 2074-2102. \url{https://doi.org/10.1002/sim.8086}
compute_MCSE_MSE <- function(thetas_hat, population_values, MSE) {

  # Compute sum of squared differences for each parameter
  squared_diff <- (thetas_hat - population_values)^2
  sum_squared_diff <- rowSums((squared_diff - MSE)^2)

  # Number of replications
  reps <- ncol(thetas_hat)

  # Compute MC SE of bias
  out <- sum_squared_diff / (reps * (reps - 1))
  return(out)
}

#' Compute Monte Carlo Standard Error of Coverage
#'
#' Compute Monte Carlo SE of coverage based on Morris et al. (2019).
#'
#' @noRd
#' @references
#' Morris, T. P., White, Ian R., Crowther, Michael J. (2017). Using simulation studies to evaluate statistical methods. Statistics in Medicine, 38, 2074-2102. \url{https://doi.org/10.1002/sim.8086}
compute_MCSE_coverage <- function(coverage, reps_completed) {
  out <- sqrt( (coverage * (1 - coverage)) / reps_completed )
  return(out)
}

#' Compute Monte Carlo Standard Error of Average Model SE
#'
#' Compute Monte Carlo SE of average model SE based on Morris et al. (2019).
#'
#' @noRd
#' @references
#' Morris, T. P., White, Ian R., Crowther, Michael J. (2017). Using simulation studies to evaluate statistical methods. Statistics in Medicine, 38, 2074-2102. \url{https://doi.org/10.1002/sim.8086}
compute_MCSE_SEAvg <- function(standard_variances, VEAvg, SEAvg, reps_completed) {

  # Compute variance of estimator variance
  var_var_theta <- rowSums((standard_variances - VEAvg)^2) / (reps_completed - 1)

  # Compute average ModeSE
  out <- sqrt( var_var_theta / (4 * reps_completed * SEAvg^2))
  return(out)
}

#' Compute Monte Carlo Standard Error of Empirical SE
#'
#' Compute Monte Carlo SE of empirical SE based on Morris et al. (2019).
#'
#' @noRd
#' @references
#' Morris, T. P., White, Ian R., Crowther, Michael J. (2017). Using simulation studies to evaluate statistical methods. Statistics in Medicine, 38, 2074-2102. \url{https://doi.org/10.1002/sim.8086}
compute_MCSE_EmpSE <- function(EmpSE, reps) {
  out <- EmpSE / (sqrt(2 * (reps - 1)))
  return(out)
}

#' Compute Monte Carlo Standard Error of Power
#'
#' Compute Monte Carlo SE of power based on Morris et al. (2019).
#'
#' @noRd
#' @references
#' Morris, T. P., White, Ian R., Crowther, Michael J. (2017). Using simulation studies to evaluate statistical methods. Statistics in Medicine, 38, 2074-2102. \url{https://doi.org/10.1002/sim.8086}
compute_MCSE_power <- function(power, reps) {
  out <- sqrt((power * (1 - power)) / reps)
  return(out)
}

