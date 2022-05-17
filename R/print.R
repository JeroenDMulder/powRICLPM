#' @title
#' Print `powRICLPM` object
#'
#' @description
#' \code{print.powRICLPM} prints a textual summary of the power analysis \emph{design} used to create the `powRICLPM` object. Specifically, it prints the number of Monte Carlo replications that were run, the range of sample sizes, number of repeated measures, and proportions of between-unit variance, and if bounded estimation, constraints over time, or informative hypotheses were used.
#'
#' @param x A `powRICLPM` object.
#' @param ... Argument not in use.
#'
#' @method print powRICLPM
#' @export
print.powRICLPM <- function(x, ...) {
  print_basic(x)
  cat("\nSession info:")
  cat("\n\n  No. Monte Carlo replications: ", x$session$reps)
  cat("\n  Constraints: ", x$session$constraints)
  cat("\n  Bounds: ", x$session$bounds)
  invisible(x)
}

#' Print `powRICLPM` summary basics
#'
#' `print_basic()` outputs to screen the basic text contained in any summary of an `powRICLPM` object.
#'
#' @param x A `powRICLPM` object.
#'
#' @noRd
print_basic <- function(x) {
  # Collect info ----
  sample_size <- unique(purrr::map_dbl(x$conditions, function(condition) {
    condition$sample_size
  }))
  time_points <- unique(purrr::map_dbl(x$conditions, function(condition) {
    condition$time_points
  }))
  ICC <- unique(purrr::map_dbl(x$conditions, function(condition) {
    condition$ICC
  }))

  # Print ----
  cat("\npowRICLPM (", as.character(utils::packageVersion("powRICLPM")), ") simulated power for ", length(x$conditions), " experimental conditions:",
    sep = ""
  )
  cat("\n\n  No. sample sizes:", sample_size)
  cat("\n  No. time points:", time_points)
  cat("\n  No. random intercept variance proportions:", ICC)
  cat("\n")
  cat("\n  No. Monte Carlo replications:", x$session$reps)
  cat("\n")
}

#' Print parameter names within `powRICLM` object
#'
#' `print_names()` gathers and outputs the parameter names of the `powRICLPM` object `x`.
#'
#' @param x
#'
#' @noRd
print_names <- function(x) {
  condition_length <- purrr::map_int(x$conditions, function(condition) { # No. parameters per condition
    length(condition$results$par)
  })
  names_pars <- x$conditions[[which.min(condition_length)]]$results$par # Universal parameter names

  cat("\n\nSugested next steps:")
  cat("\n\n  - Specify the `parameter` argument of `summary()` to obtain a parameter-specific summary.")
  cat("\n  - Use `coef()` to obtain a table with metrics, across conditions, for a specific parameter.")
  cat("\n  - Use `plot()` to visualize results for a specific parameter across conditions.")
  cat("\n\nParameter names:")
  cat("\n", names_pars,
    sep = "\n  - "
  )
}

print_suggestion_parameter <- function() {
  cat("\n\nSugested next steps:")
  cat("\n\n  - Specify the `parameter` argument of `summary()` to obtain a parameter-specific summary.")
  cat("\n  - Specify `names = TRUE` as an argument in `summary()` to obtain names of parameters in the powRICLPM object.")
  cat("\n  - Use `coef()` to obtain a table with metrics, across conditions, for a specific parameter.")
  cat("\n  - Use `plot()` to visualize results for a specific parameter across conditions.")
}
