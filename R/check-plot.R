#' Check \code{parameter} argument
#'
#' \code{check_parameter()} tests if a parameter was specified.
#'
#' @param x A character string.
#'
#' @noRd
check_parameter_given <- function(parameter, object) {
  if (is.null(parameter)) {
    stop(rlang::format_error_bullets(c(
      "No `parameter` was specified:",
      i = "`plot()` needs to know which specific parameter to create a plot for."
    )))
  }
  return(parameter)
}

#' Check \code{y} argument
#'
#' \code{check_y()} tests if a \code{y} represents a valid outcome for plotting \code{powRICLPM} results.
#'
#' @param y Character string, specifying which outcome is plotted on the y-axis.
#'
#' @noRd
check_y <- function(y) {
  if (length(y) > 1) {
    stop(rlang::format_error_bullets(c(
      "You can only plot a single outcome on the y-axis:",
      x = paste0("You specified ", length(y), " outcomes.")
    )))
  }
  if (!y %in% c("power", "coverage", "accuracy", "MSE", "bias", "average", "SD", "SEAvg")) {
    stop(rlang::format_error_bullets(c(
      "`y` must be 'power', 'coverage', 'accuracy', 'MSE', 'bias', 'average', 'SD', or 'SEAvg':",
      x = paste0("Your `y` is '", y, "'.")
    )))
  }
}
