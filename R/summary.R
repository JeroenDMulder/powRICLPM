#' Summarize setup and results from \code{powRICLPM} object
#'
#' @description
#' S3 method for class \code{powRICLPM}. \code{summary.powRICLPM} summarizes and outputs the setup and results of the \code{powRICLPM} analysis. Depending on the arguments that are set, \code{summary.powRICLPM} provides a different summary (see "Details").
#'
#' @param object A \code{powRICLPM} object.
#' @param ... (don't use) Additional arguments not affecting the summary produced.
#' @param parameter Character string of length denoting the parameter to visualize the results for.
#' @param sample_size (optional) An \code{integer}, denoting the sample size of the experimental condition of interest.
#' @param time_points (optional) An \code{integer}, denoting the number of time points of the experimental condition of interest.
#' @param ICC (optional) A \code{double}, denoting the proportion of variance at the between-unit level of the experimental condition of interest.
#'
#' @return No return value, called for side effects.
#'
#' @details
#' \code{summary.powRICLPM} provides a different summary of the \code{powRICLPM} object, depending on the additional arguments that are set:
#' \itemize{
#'   \item When \code{sample_size = ...}, \code{time_points = ...}, and \code{ICC = ...} are set: Estimation information and results for all parameters of the experimental condition denoted by \code{sample_size}, \code{time_points}, and \code{ICC}.
#'   \item When \code{parameter = "..."} is set: Estimation information and results for a specific parameter across all experimental conditions.
#'   \item No additional arguments: Characteristics of the different experimental conditions are summarized, as well as session info (information that applies to each conditions, such the number of replications, etc.).
#' }
#'
#' @examples
#' \dontshow{
#' load(system.file("extdata", "out_preliminary.RData", package = "powRICLPM"))
#' }
#'
#' # Get setup of powRICLPM analysis and convergence issues
#' summary(out_preliminary)
#'
#' # Performance measures for "wB2~wA1" parameter across experimental conditions
#' summary(out_preliminary, parameter = "wB2~wA1")
#'
#' # Performance measures for all parameters, for specific experimental condition
#' summary(out_preliminary, sample_size = 600, time_points = 4, ICC = .4)
#'
#' @method summary powRICLPM
#' @export
summary.powRICLPM <- function(object,
                              ...,
                              parameter = NULL,
                              sample_size = NULL,
                              time_points = NULL,
                              ICC = NULL) {

  # Argument validation ----
  check_object(object)
  if (!is.null(parameter)) {
    parameter <- check_parameter_argument(parameter)
    parameter <- check_parameter_available(parameter, object)
  }

  # Summarize ----
  if (!is.null(sample_size) && !is.null(time_points) && !is.null(ICC)) {
    summary_condition(object, sample_size, time_points, ICC)
  } else if (!is.null(parameter)) {
    summary_parameter(object, parameter)
  } else {
    print.powRICLPM(object)
  }
}

#' \code{powRICLPM} summary for specific experimental condition
#'
#' Print results for all parameters from a specific experimental condition in the \code{powRICLPM} object.
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

  # Collect information ----
  condition <- purrr::keep(object$conditions, function(x) {
    x$sample_size == sample_size && x$time_points == time_points && x$ICC == ICC
  })[[1]]

  df_c <- data.frame(
    c("Skewness:", "Kurtosis:", "Constraints:", "Bounds:", "Estimate ME:", "Significance criterion:"),
    c(
      condition$skewness, condition$kurtosis, object$session$constraints, object$session$bounds,
      object$session$estimate_ME, condition$alpha
    )
  )
  df_e <- data.frame(
    c("Errors:", "Nonconvergence:", "Inadmissible results:"),
    c(sum(condition$errors), sum(condition$not_converged), sum(condition$inadmissible))
  )

  # Print ----
  cat("\nResults:")
  print(
    knitr::kable(
      condition$estimates,
      format = "simple",
      digits = 3,
      col.names = c("Parameter", "Population value", "Avg", "Min", "SD", "SE Avg", "MSE", "Accuracy", "Coverage", "Power"),
      align = "lrrrrrrrr",
      escape = FALSE
    )
  )
  cat("\nEstimation problems:")
  print(
    knitr::kable(
      df_e,
      align = "lr",
      col.names = NULL,
      format = "simple"
    )
  )
  cat("\nCondition info:")
  print(
    knitr::kable(
      df_c,
      align = "lr",
      col.names = NULL,
      format = "simple"
    )
  )
}

#' \code{powRICLPM} summary for specific parameter
#'
#' @param object A \code{powRICLPM} object.
#' @param parameter Character string, denoting a parameter.
#'
#' @noRd
summary_parameter <- function(object, parameter) {
  df <- dplyr::full_join(
    give(object, "estimation_problems"),
    give(object, what = "results", parameter = parameter),
    by = c("sample_size", "time_points", "ICC")
  )
  cat("Results for ", parameter, " across conditions:", sep = "")
  print(
    knitr::kable(
      df,
      format = "simple",
      col.names = c(
        "N", "T", "ICC",
        "Err", "NC", "IR",
        "PV", "Avg", "Min", "SD", "SE Avg", "MSE", "Accuracy", "Coverage", "Power"
      )
    )
  )
  invisible(df)
}
