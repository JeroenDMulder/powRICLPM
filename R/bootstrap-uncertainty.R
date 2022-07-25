#' Quantify uncertainty around estimate using bootstrapping
#'
#' @param measure A numeric vector of estimates.
#' @param bootstrap_reps A numeric, denoting the number of bootstrap samples to draw.
#' @param converged_reps A numberic, denoting the number of valid estimates.
#'
#' @noRd
quantify_uncertainty <- function(measure, bootstrap_reps = 1000, converged_reps) {
  x <- vector(mode = "numeric", length = bootstrap_reps)
  n <- length(measure)
  for (r in 1:bootstrap_reps) {
    x[r] <- sum((sample(measure, n, replace = TRUE))) / converged_reps
  }
  stats::quantile(x[order(x)], probs = c(.025, .975), na.rm = TRUE)
}
