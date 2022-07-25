#' Plot results from \code{powRICLPM} object
#'
#' \code{autoplot_powRICLPM()} plots the simulated power across the experimental conditions within the \code{powRICLPM} object, and returns a \pkg{ggplot2} object.
#'
#' @param object A \code{powRICLPM} object.
#' @param parameter Character string.
#'
#' @noRd
#' @importFrom rlang .data
autoplot_powRICLPM <- function(object, parameter) {
  check_parameter_given(parameter)
  check_parameter_argument(parameter)
  check_parameter_available(parameter, object)

  # Get performance table
  d <- dplyr::full_join(
    give_results(object, parameter = parameter),
    give_uncertainty(object, parameter = parameter),
    by = c("sample_size", "time_points", "ICC")
  )

  # Create data (ggplot2-argument)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$sample_size, y = .data$Pow, color = factor(.data$time_points))) +
    ggplot2::geom_point(shape = 19) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = object$session$target_power, linetype = "dashed") +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = .data$Pow2.5, ymax = .data$Pow97.5),
      position = ggplot2::position_dodge(2)
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$ICC)) +
    ggplot2::scale_y_continuous(name = "Power", limits = c(0, 1)) +
    ggplot2::scale_x_continuous(name = "Sample size", breaks = d$sample_size) +
    ggplot2::labs(color = "Time points") +
    ggplot2::theme_bw()
}

#' @title
#' Plot results from \code{powRICLPM} object
#'
#' @description
#' \code{plot.powRICLPM()} visualizes (using \pkg{ggplot2}) the estimated power across all experimental conditions within the \code{powRICLPM} object, for a specific parameter. The plots display the relation between sample size (x-axis) and power (y-axis), grouped by the number of time points, and wrapped by the proportion of between-unit variance.
#'
#' @param x A \code{powRICLPM} object.
#' @param y (don't use)
#' @param ... (don't use)
#' @param parameter Character string of length denoting the parameter to visualize the results for.
#'
#' @details
#' \subsection{Manually creating plots}{\code{plot.powRICLPM()} creates arguably the most obvious plot in the context of power analysis: The relation between power and sample size. However, \code{powRICLPM} computes several other metrics (e.g., accuracy, coverage rate, etc.) that researchers might want to plot. To this end, I suggest that users use \code{give()} to collect other metrics, across all experimental conditions, for a specific parameter, into a data frame first. Then, users can use any plotting function they like to create their own manual plot. See "Details" of \code{\link{give}()} for a list of measures that are computed by the function.}
#'
#' @seealso
#' \itemize{
#'   \code{\link{give}}: Extract information (e.g., performance measures) for a specific parameter, across all experimental conditions. This function is used internally in \code{plot.powRICLPM}.
#' }
#'
#' @return
#' A \code{ggplot2} object.
#' @method plot powRICLPM
#' @export
plot.powRICLPM <- function(x, y, ..., parameter = NULL) {
  autoplot_powRICLPM(x, parameter = parameter)
}
