#' @title
#' Extract results from powRICLPM analysis
#'
#' @description
#' S3 method for class `powRICLPM`. \code{coef.powRICLPM} extracts the power analysis results across all experimental conditions, \emph{for a specific parameter}. See "Details" for a list of measures that are extracted.
#'
#' @param object A powRICLPM object.
#' @param parameter A character string denoting the parameter of interest.
#' @param ... Additional arguments not affecting the results extracted produced.
#'
#' @details
#' The below measures are extracted for each experimental condition:
#' \itemize{
#'   \item \code{errors}: The number replications that ended with a fatal error.
#'   \item \code{not_converged}: The number of replications that did not converge to a solution.
#'   \item \code{inadmissible}: The number of replications that resulted in inadmissible results for some parameters (e.g., negative variances).
#'   \item \code{avg}: The average parameter estimate over all replications.
#'   \item \code{stdDev}: The standard deviation of parameter estimates across replications.
#'   \item \code{SEAvg}: The average standard error across replications.
#'   \item \code{mse}: The mean square error.
#'   \item \code{acc}: The average width of the confidence interval.
#'   \item \code{cover}: The coverage rate of the confidence interval for the defined significance criterion (by default \code{alpha = .05}).
#'   \item \code{pwr}: The proportion of times the \emph{p}-value was lower than the significance criterion.
#' }
#'
#' @return
#' A \code{data.frame} with rows representing the experimental conditions, and columns representing the metrics.
#'
#' @method coef powRICLPM
#' @export
coef.powRICLPM <- function(object, ..., parameter = NULL) {

  # Argument validation
  check_object(object)
  if (!is.null(parameter)) {
    parameter <- check_parameter_summary(parameter, object)
  } else if (!is.null(object$session$parameter)) {
    parameter <- object$session$parameter
  } else {
    stop(rlang::format_error_bullets(c(
      "`coef()` must know which parameter to extract results for:",
      x = "You've not supplied a `parameter` argument, nor set `parameter` when running `powRICLPM()`."
    )))
  }

  # Combine sample sizes and simulated power across conditions
  d <- purrr::map_dfr(object$conditions, function(condition) {

    # Create data frame
    data.frame(
      sample_size = condition$sample_size,
      time_points = condition$time_points,
      ICC = condition$ICC,
      errors = sum(condition$errors),
      not_converged = sum(condition$not_converged),
      inadmissible = sum(condition$inadmissible),
      round(condition$results[condition$results$par == parameter, -1],
        digits = 3
      )
    )
  })
  return(d)
}
