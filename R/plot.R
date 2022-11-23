#' Plot results from \code{powRICLPM} object
#'
#' @description
#' Visualizes (using \pkg{ggplot2}) the results from a \code{powRICLPM} analysis, for a specific parameter, across all experimental conditions. By default, sample size is plotted on the x-axis, power on the y-axis, and results are grouped by the number of time points and wrapped by the proportion of between-unit variance. Optionally, the \code{y} argument can be used to change the variable on the y-axis to other outcomes from the \code{powRICLPM} analysis.
#'
#' @param x A \code{powRICLPM} object.
#' @param y (optional) A \code{character} string, specifying which outcome is plotted on the y-axis (see "Details").
#' @param ... (don't use)
#' @param parameter Character string of length denoting the parameter to visualize the results for.
#'
#' @details
#' \subsection{y-axis options}{The following outcomes can be plotted on the y-axis:
#'
#' \itemize{
#'   \item \code{average}: The average estimate.
#'   \item \code{MSE}: The mean square error.
#'   \item \code{coverage}: The coverage rate
#'   \item \code{accuracy}: The average width of the confidence interval.
#'   \item \code{SD}: Standard deviation of parameter estimates.
#'   \item \code{SEAvg}: Average standard error.
#'   \item \code{bias}: The absolute difference between the average estimate and population value.
#' }
#' }
#'
#' @seealso
#' \itemize{
#'   \code{\link{give}}: Extract information (e.g., performance measures) for a specific parameter, across all experimental conditions. This function is used internally in \code{plot.powRICLPM}.
#' }
#'
#' @return A \code{ggplot2} object.
#'
#' @method plot powRICLPM
#' @export
#'
#' @examples
#' \dontshow{
#' load(system.file("extdata", "out_preliminary.Rds", package = "powRICLPM"))
#' }
#' # Visualize power for "wB2~wA1" across simulation conditions
#' plot(out_preliminary, parameter = "wB2~wA1")
#'
#' # Visualize bias for "wB2~wA1" across simulation conditions
#' plot(out_preliminary, y = "bias", parameter = "wB2~wA1")
#'
#' # Visualize coverage rate for "wB2~wA1" across simulation conditions
#' plot(out_preliminary, y = "coverage", parameter = "wB2~wA1")
#'
#' # Visualize MSE for autoregressive effect across simulation conditions
#' plot(out_preliminary, y = "MSE", parameter = "wA2~wA1")
#'
#' # Error: No parameter specified
#' try(plot(out_preliminary))
plot.powRICLPM <- function(x, y = "power", ..., parameter = NULL) {
  check_parameter_given(parameter)
  check_parameter_argument(parameter)
  check_parameter_available(parameter, x)
  check_y(y)

  if (y == "power") {
    plot_power(object = x, parameter = parameter)
  } else if (y == "bias") {
    plot_bias(object = x, parameter = parameter)
  } else {
    plot_default(object = x, y = y, parameter = parameter)
  }
}

#' Plot power results from \code{powRICLPM} object
#'
#' Plot the simulated power across the experimental conditions within the \code{powRICLPM} object, and return a \pkg{ggplot2} object.
#'
#' @param object A \code{powRICLPM} object.
#' @inheritParams plot.powRICPLM
#'
#' @noRd
#' @importFrom rlang .data
plot_power <- function(object, parameter) {

  # Get performance table
  d <- dplyr::full_join(
    give_results(object, parameter = parameter),
    give_uncertainty(object, parameter = parameter),
    by = c("sample_size", "time_points", "ICC")
  )

  # Create data (ggplot2-argument)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$sample_size, y = .data$power, color = factor(.data$time_points))) +
    ggplot2::geom_point(shape = 19) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = object$session$target, linetype = "dashed") +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = .data$power2.5, ymax = .data$power97.5),
      position = ggplot2::position_dodge(2)
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$ICC)) +
    ggplot2::scale_y_continuous(name = "Power", limits = c(0, 1)) +
    ggplot2::scale_x_continuous(name = "Sample size", breaks = d$sample_size) +
    ggplot2::labs(color = "Time points") +
    ggplot2::theme_bw()
}

#' Plot bias results from \code{powRICLPM} object
#'
#' Plot the simulated bias across the experimental conditions within the \code{powRICLPM} object, and return a \pkg{ggplot2} object.
#'
#' @param object A \code{powRICLPM} object.
#' @inheritParams plot.powRICPLM
#'
#' @noRd
#' @importFrom rlang .data
plot_bias <- function(object, parameter) {

  # Get performance table
  d <- dplyr::mutate(
    .data = give_results(object, parameter = parameter),
    bias = .data$average - .data$population_value
  )

  # Create data (ggplot2-argument)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$sample_size, y = .data$bias, color = factor(.data$time_points))) +
    ggplot2::geom_point(shape = 19) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::facet_wrap(ggplot2::vars(.data$ICC)) +
    ggplot2::scale_y_continuous(name = "Bias") +
    ggplot2::scale_x_continuous(name = "Sample size", breaks = d$sample_size) +
    ggplot2::labs(color = "Time points") +
    ggplot2::theme_bw()
}

#' Plot results from \code{powRICLPM} object
#'
#' Plot outcomes (other than power and bias) across the experimental conditions from a \code{powRICLPM} analysis, and return a \pkg{ggplot2} object.
#'
#' @param object A \code{powRICLPM} object.
#' @inheritParams plot.powRICPLM
#'
#' @noRd
#' @importFrom rlang .data
plot_default <- function(object, y, parameter) {

  # Get performance table
  d <- give_results(object, parameter = parameter)

  ggplot2::ggplot(d,
    ggplot2::aes(
      x = .data$sample_size,
      y = .data[[y]],
      color = factor(.data$time_points)
    )
  ) +
    ggplot2::geom_point(shape = 19) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(.data$ICC)) +
    ggplot2::scale_y_continuous(name = y) +
    ggplot2::scale_x_continuous(name = "sample size", breaks = d$sample_size) +
    ggplot2::labs(color = "time points") +
    ggplot2::theme_bw()
}
