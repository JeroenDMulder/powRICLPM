#' Check \code{parameter} argument
#'
#' \code{check_parameter()} tests if a parameter was specified.
#'
#' @param x A character string.
#'
#' @noRd
icheck_plot_parameter <- function(parameter, object, arg = rlang::caller_arg(parameter), call = rlang::caller_env()) {
  if (is.null(parameter)) {
    cli::cli_abort(
      c(
        "No `parameter` was specified:",
        i = "`plot()` needs to know which specific parameter to create a plot for."
      )
    )
  }

  if (length(parameter) > 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a character vector of size 1:",
        "x" = "Your {.arg {arg}} is of length {length(sample_size)}."
      ),
      call = call
    )
  }

  if (!is.character(parameter)) {
    cli::cli_abort(
      c(
        "{.arg} must be a character string:",
        x = paste0("Your {.arg {arg}} is of type {typeof(x)}.")
      )
    )
  }

  # Check if available in all simulation conditions
  condition_lengths <- sapply(object$conditions, function(x) {
    length(x$estimates$parameter)
  })
  parameter_names <- object$conditions[[which.min(condition_lengths)]]$estimates$parameter

  if (!any(parameter == parameter_names)) {
    cli::cli_abort(
      c(
        x = "Your {.arg {arg}} is not available across all experimental conditions.",
        i = "Perhaps use `give(object, what = 'names')` to get an overview of parameter names in the `powRICLPM` object."
      )
    )
  }
}

#' Check \code{y} argument
#'
#' \code{check_y()} tests if a \code{y} represents a valid outcome for plotting \code{powRICLPM} results.
#'
#' @param y Character string, specifying which outcome is plotted on the y-axis.
#'
#' @noRd
icheck_y <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (length(x) > 1) {
    cli::cli_abort(
      c(
        "You can only plot a single outcome on the y-axis:",
        x = paste0("Your {.arg {arg}} contains {length(x)} outcomes.")
      )
    )
  }
  if (!any(x == c("power", "coverage", "accuracy", "MSE", "bias", "average", "SD", "SEAvg"))) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be 'power', 'coverage', 'accuracy', 'MSE', 'bias', 'average', 'SD', or 'SEAvg':",
        x = paste0("Your {.arg {arg}} is '", x, "'.")
      )
    )
  }
}


icheck_plot_options <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (length(x) > 1) {
    cli::cli_abort(
      c(
        "You can only set a single variable for {.arg {arg}}:",
        x = paste0("Your {.arg {arg}} contains {length(x)} variables")
      )
    )
  }
  if (!any(x == c("time_points", "ICC", "reliability"))) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be 'time_points', 'ICC', or 'reliability':",
        x = paste0("Your {.arg {arg}} is", x, ".")
      )
    )
  }
}

