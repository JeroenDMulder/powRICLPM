#' Plot `powRICLPM` object
#'
#' Plot `powRICLPM` object and return `ggplot2` object.
#'
#' @param object A `powRICLPM` object.
#' @param parameter Character string.
#'
#' @noRd
#' @importFrom rlang .data
autoplot_powRICLPM <- function(object, parameter) {

  # Argument verification
  parameter <- check_parameter_summary(parameter, object = object)

  # Get performance table
  d <- coef.powRICLPM(object = object, parameter = parameter)

  # Create data (ggplot2-argument)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$sample_size, y = .data$pwr, color = factor(.data$time_points))) +
    ggplot2::geom_point(shape = 19) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = object$session$target_power, linetype = "dashed") +
    ggplot2::facet_wrap(ggplot2::vars(.data$ICC)) +
    ggplot2::scale_y_continuous(name = "Power", limits = c(0, 1)) +
    ggplot2::scale_x_continuous(name = "Sample size", breaks = d$sample_size) +
    ggplot2::labs(color = "Time points")
}

#' @title
#' Plot `powRICLPM` analysis results
#'
#' @description
#' \code{plot.powRICLPM} visualizes results from `powRICLPM` objects using \pkg{ggplot2}. The plots display the relation between sample size (x-axis) and power (y-axis), grouped by the number of time points, and wrapped by the proportion of between-unit variance.
#'
#' @param x A `powRICLPM` object.
#' @param y Argument not in use.
#' @param ... Argument not in use.
#' @param parameter A character string denoting the parameter of interest. Use the argument \code{names = TRUE} in \code{summary()} to get an overview of parameter names in the `powRICLPM` object.
#'
#' @details
#' \subsection{Manually creating plots}{\code{plot.powRICLPM()} creates the most obvious plot in the context of power analysis: The relation between power and sample size. However, \code{powRICLPM} computes several other metrics (e.g., accuracy, coverage rate, etc.) that research might want to plot. To this end, I suggest that user use \code{coef.powRICLPM} to collect all metrics, across all experimental conditions, for a specific parameter, into a data frame first. Then, user can use any plotting function they like to create their own manual plot. See "Details" of \code{\link{coef.powRICLPM}} for a list of measures that are computed by the function.}
#'
#' @seealso
#' \itemize{
#'   \code{\link{coef.powRICLPM}}: Extract performance measures for a specific parameter, across all experimental conditions. This function is used internally in \code{plot.powRICLPM}.
#' }
#'
#' @return
#' A `ggplot2` object.
#' @method plot powRICLPM
#' @export
plot.powRICLPM <- function(x, y, ..., parameter) {
  autoplot_powRICLPM(x, parameter = parameter)
}
