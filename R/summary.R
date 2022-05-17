#' @title
#' Summarize powRICLPM analysis
#'
#' @description
#' S3 method for class `powRICLPM`. \code{summary.powRICLPM} summarizes and outputs the setup and results of the powRICLPM analysis either generally, for a specific parameter (when \code{parameter} argument is specified), or for a specific parameter in a specific experimental condition (when the \code{parameter}, \code{sample_size}, \code{time_points}, and \code{ICC} arguments are specified). Optionally, it can display the parameter names contained in \code{object}.
#'
#' @param object A powRICLPM object.
#' @param ... Additional arguments not affecting the summary produced.
#' @param parameter A character string denoting the parameter of interest.
#' @param sample_size An integer denoting the sample size of interest.
#' @param time_points An integer denoting the number of time points of interest.
#' @param ICC A numeric value denoting the proportion of variance at the between-unit level of interest.
#' @param names A logical denoting if parameter names contained in \code{object} should be printed.
#'
#' @details
#' \code{summary.powRICLPM} sets the \code{parameter} element in \code{session} with the value of the \code{parameter} argument, and returns the \code{object}. Saving this object might be useful as it renders the \code{parameter} argument for \code{\link{plot.powRICLPM}} and \code{\link{coef.powRICLPM}} superfluous.
#' \subsection{Parameter names}{When simulating the power across conditions with a varying number of time points, there are also a different numbers of parameters across the experimental conditions. By specifying the argument \code{names = TRUE}, this function returns the parameter names from the condition with the smallest number of parameters. As such, the returned parameter names are valid for each experimental condition.}
#'
#' @return
#' The powRICLPM \code{object} with the \code{parameter} element in \code{object$session} set to the value of the \code{parameter} argument.
#'
#' @examples
#' # Example - Same starting point as example from ?powRICLPM()
#' # Define population parameters for lagged effects and within-component correlations
#' Phi <- matrix(c(.4, .1, .2, .3), ncol = 2, byrow = TRUE)
#' wSigma <- matrix(c(1, .3, .3, 1), ncol = 2, byrow = TRUE)
#'
#' # Setup parallel computing (multicore, speeding up the analysis)
#' \dontrun{
#' library(furrr)
#' plan(multisession, workers = 4)
#'
#' # Run the analysis
#' out1 <- powRICLPM(
#'   target_power = 0.8,
#'   search_lower = 500,
#'   search_upper = 1000,
#'   search_step = 100,
#'   time_points = c(3, 4),
#'   ICC = c(0.4, 0.5, 0.6),
#'   RI_cor = 0.3,
#'   Phi = Phi,
#'   wSigma = wSigma,
#'   reps = 100,
#'   seed = 123456
#' )
#'
#' # General results (i.e., setup of powRICLPM analysis)
#' summary(out)
#'
#' # General results and recommendation for specific parameter
#' summary(out, parameter = "wB2~wA1")
#'
#' # Set parameter element of object
#' out_wB2wA1 <- summary(out, parameter = "wB2~wA1")
#' }
#' @method summary powRICLPM
#' @export
summary.powRICLPM <- function(object,
                              ...,
                              names = FALSE,
                              parameter = NULL,
                              sample_size = NULL,
                              time_points = NULL,
                              ICC = NULL) {

  # Argument validation ----
  check_object(object)
  if (!is.null(parameter)) {
    parameter <- check_parameter_summary(parameter, object)
    object$session$parameter <- parameter
  }

  # Summarize ----
  if (!is.null(sample_size) && !is.null(time_points) &&
    !is.null(ICC) && !is.null(parameter)) {
    summary_condition(object, sample_size, time_points, ICC)
  } else if (!is.null(parameter)) {
    summary_parameter(object, parameter)
  } else if (names) {
    condition_length <- purrr::map_int(object$conditions, function(condition) { # No. parameters per condition
      length(condition$results$par)
    })
    object$conditions[[which.min(condition_length)]]$results$par # Universal parameter names
  } else {
    summary_default(object)
  }
}

#' `powRICLPM` summary for specific experimental condition
#'
#' Print results from a specific experimental condition in the `powRICLPM` object.
#'
#' @inheritParams summary.powRICLPM
#'
#' @noRd
summary_condition <- function(object,
                              sample_size,
                              time_points,
                              ICC) {
  # Argument validation ----
  check_N_summary(object, sample_size)
  check_T_summary(object, time_points)
  check_ICC_summary(object, ICC)

  # Select specified condition
  condition <- purrr::keep(object$conditions, function(x) {
    x$sample_size == sample_size &&
      x$time_points == time_points &&
      x$ICC == ICC
  })[[1]]

  results_parameter <- condition$results[which(condition$results$par == object$session$parameter), ]

  cat("Experimental condition with:",
    sep = ""
  )
  cat("\n\n  Sample size:", sample_size)
  cat("\n  Time points:", time_points)
  cat("\n  Proportion random intercept variance:", ICC)
  cat("\n  Skewness:", condition$skewness)
  cat("\n  Kurtosis:", condition$kurtosis)
  cat("\n\nEstimation:")
  cat("\n\n  Significance criterion:", condition$alpha)
  cat("\n  Bounds:", object$session$bounds)
  cat("\n  Constraints:", object$session$constraints)
  cat("\n  Effective no. replications:", (condition$reps - sum(condition$not_converged)), "/", condition$reps)
  cat("\n\nResults for ", object$session$parameter, ":",
    sep = ""
  )
  cat("\n\n  Inadmissible results:", sum(condition$inadmissible))
  cat("\n  Average estimate:", results_parameter$avg)
  cat("\n  Standard deviation of estimates:", results_parameter$stdDev)
  cat("\n  Average standard error:", results_parameter$SEAvg)
  cat("\n  Mean square error:", results_parameter$mse)
  cat("\n  Coverage rate:", results_parameter$coverage)
  cat("\n  Accuracy:", results_parameter$acc)
  cat("\n  Power:", results_parameter$pwr)
}

#' Basic `powRICLPM` summary
#'
#' Output a basic summary of an `powRICLPM` object.
#'
#' @param object A `powRICLPM` object.
#'
#' @noRd
summary_default <- function(object) {
  print.powRICLPM(object)
  print_suggestion_parameter()
}

#' `powRICLPM` summary for specific parameter
#'
#' @param object A `powRICLPM` object.
#' @param parameter Character string denoting a parameter.
#'
#' @noRd
summary_parameter <- function(object, parameter) {
  n_recommendations <- length(
    purrr::keep(object$conditions, function(x) {
      x$results[x$results$par == parameter, "pwr"] >= object$session$target_power
    })
  )
  if (n_recommendations == 1) {
    candidate <- purrr::detect_index(object$conditions, function(condition) { # Detect condition that meets target_power
      condition$results[condition$results$par == parameter, "pwr"] >= object$session$target_power
    })
  }

  # Print ----
  print_basic(object)
  cat("  Targeted power:", object$session$target_power)
  cat("\n\nResults for", parameter, ":")
  cat("\n\n  No. conditions with power > targeted power: ", n_recommendations, "/", length(object$conditions))
  cat("\n")

  if (n_recommendations == 0) {
    cat("\nSuggested next steps:")
    cat("\n\n  - Increase `search_upper` and/or `time_points`, and rerun the analysis.")
  } else if (n_recommendations == 1) {
    cat(
      "\n  Simulated power: ", object$conditions[[candidate]]$results[object$conditions$results$par == parameter, "pwr"],
      "(with ICC = ", object$conditions[[candidate]]$ICC, ")"
    )
    cat("\n")
    cat("\n  Sample size: ", object$conditions[[candidate]]$sample_size)
    cat("\n  No. time points: ", object$conditions[[candidate]]$time_points)
    cat("\n")
    cat("\n  No. non-converged replications:", sum(object$conditions[[candidate]]$not_converged))
    cat("\n  No. replications with inadmissible estimates:", sum(object$conditions[[candidate]]$inadmissible))
    cat("\n")
    cat("\nSugested next step:")
    cat("\n\n  - For preliminary powRICLPM analyses, validate the recommendation by rerunning\n   the analysis with an increased number of replications (e.g., `reps = 1000`).")
  } else if (n_recommendations > 1) {
    cat("\nSuggested next steps:")
    cat("\n\n  - For preliminary powRICLPM analyses, validate the results by rerunning the\n   analysis (for a selection of conditions) with an increased number of\n   replications (e.g., `reps = 1000`).")
  }
  # Print suggestion that applies to every scenario
  cat("\n  - Use `plot()` to visualize results across all experimental conditions.")
}
