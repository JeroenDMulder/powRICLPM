#' Count number of estimated parameters
#'
#' \code{count_parameters()} counts the number of parameters that is estimated in the condition with most time points, taking imposed constraints and the potential estimation of measurement errors into account.
#'
#' @inheritParams powRICLPM
#' @param k An integer denoting the number of variables in the RI-CLPM.
#'
#' @noRd
count_parameters <- function(k, time_points, constraints, est_ME) {
  time_points_max <- max(time_points)

  # Default k-variate RI-CLPM
  n_parameters <- sum(
    factorial(1 + k) / (2 * (k - 1)) * (time_points_max + 1), # Number of (co)variances within- and between-level
    k^2 * (time_points_max - 1), # Number of lagged effects
    ifelse(est_ME, k * time_points_max, 0) # ME
  )

  # Influence of constraints
  if (constraints == "lagged" || constraints == "within" || constraints == "stationarity") {
    n_parameters <- n_parameters - (k^2 * (time_points_max - 2))
  }
  if (constraints == "residuals" || constraints == "within") {
    n_parameters <- n_parameters - ((k + sum((k - 1):1)) * (time_points_max - 2))
  }
  if (constraints == "stationarity" ) {
    n_parameters <- n_parameters - ((time_points_max - 1) * k)
  }
  if (constraints == "ME") {
    n_parameters <- n_parameters - ((time_points_max - 1) * k)
  }
  return(n_parameters)
}
