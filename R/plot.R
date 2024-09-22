#' Plot Results From \code{powRICLPM} Object
#'
#' @description
#' Visualizes (using \pkg{ggplot2}) the results from a \code{powRICLPM} analysis, for a specific parameter, across all experimental conditions. By default, sample size is plotted on the x-axis, power on the y-axis, with results colored by the number of time points, wrapped by the proportion of between-unit variance, and shaped by the reliability. Optionally, other variables can be mapped to the y-axis, x-axis, color, shape, and facets.
#'
#' @param x A \code{powRICLPM} object.
#' @param y (optional) A \code{character} string, specifying which outcome is plotted on the y-axis (see "Details").
#' @param ... (don't use)
#' @param parameter Character string of length 1, denoting the parameter to visualize the results for.
#' @param color_by Character string of length 1, denoting what variable to map to color (see "Details").
#' @param shape_by Character string of length 1, denoting what variable to map to point shapes (see "Details").
#' @param facet_by Character string of length 1, denoting what variable to facet by (see "Details").
#'
#' @details
#' \subsection{Mapping Options}{The following outcomes can be plotted on the y-axis:
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
#'
#' The following variables can be mapped to color, shape, and facet:
#'
#' \itemize{
#'    \item \code{sample_size}: Sample size.
#'    \item \code{time_points}: Time points.
#'    \item \code{ICC}: Intraclass correlation (ICC).
#'    \item \code{reliability}: Item-reliablity.
#' }
#' }
#'
#' @seealso
#' \itemize{
#'   \code{\link{give}}: Extract information (e.g., performance measures) for a specific parameter, across all experimental conditions. This function is used internally by \code{plot.powRICLPM}.
#' }
#'
#' @return A \code{ggplot2} object.
#'
#' @method plot powRICLPM
#' @export
#'
#' @examples
#' \dontshow{
#' load(system.file("extdata", "out_preliminary.RData", package = "powRICLPM"))
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
plot.powRICLPM <- function(
    x,
    y = "power",
    ...,
    parameter = NULL,
    color_by = "time_points",
    shape_by = "reliability",
    facet_by = "ICC"
) {

  icheck_plot_parameter(parameter, x)
  icheck_y(y)
  do.call(icheck_plot_options, list(color_by, shape_by, facet_by))

  # Get performance table
  d <- merge(
    give_powRICLPM_results(x, parameter = parameter),
    give_powRICLPM_MCSE_parameter(x, parameter = parameter),
    by = c("sample_size", "time_points", "ICC", "reliability")
  )

  # Compute upper and lower bound of y-variable
  d$lb <- d[, y] - 1.96 * d[, paste0("MCSE_", y)]
  d$ub <- d[, y] + 1.96 * d[, paste0("MCSE_", y)]

  # Select relevant columns
  d <- d[, c(y, "sample_size", color_by, shape_by, facet_by, "lb", "ub")]

  # Ensure inputs are factors for proper handling in ggplot2
  d[[facet_by]] <- as.factor(d[[facet_by]])
  d[[color_by]] <- as.factor(d[[color_by]])
  d[[shape_by]] <- as.factor(d[[shape_by]])

  # Create plot
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$sample_size, y = !!ggplot2::sym(y), color = !!ggplot2::sym(color_by), shape = !!ggplot2::sym(shape_by), fill = !!ggplot2::sym(color_by))) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lb, ymax = .data$ub), alpha = 0.2, color = NA) +
    ggplot2::facet_wrap(as.formula(paste('~', facet_by)), scales = "fixed") +
    ggplot2::labs(x = "sample size", y = y) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 8)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(title = color_by, nrow = 1),
      shape = ggplot2::guide_legend(title = shape_by, nrow = 1)
    )

  # Add dashed horizontal line if y is "power"
  if (y == "power") {
    p <- p + ggplot2::geom_hline(yintercept = x$session$target_power, linetype = "dashed")
  }

  # Print plot
  print(p)
  return(p)
}
