#' Summarize Results from \code{powRICLPM} Object
#'
#' @description
#' S3 method for class \code{powRICLPM}. \code{summary.powRICLPM} summarizes the setup and results of the \code{powRICLPM} analysis. Depending on the arguments that are set, \code{summary.powRICLPM} provides a different summary (see "Details").
#'
#' @param object A \code{powRICLPM} object.
#' @param ... (don't use)
#' @param parameter Character string of length 1 denoting the parameter to visualize the results for.
#' @param sample_size (optional) An \code{integer}, denoting the sample size of the experimental condition of interest.
#' @param time_points (optional) An \code{integer}, denoting the number of time points of the experimental condition of interest.
#' @param ICC (optional) A \code{double}, denoting the proportion of variance at the between-unit level of the experimental condition of interest.
#' @param reliability (optional) An \code{integer}, denoting the reliability of the indicators of the experimental condition of interest.
#'
#' @return No return value, called for side effects.
#'
#' @details
#' \code{summary.powRICLPM} provides a different summary of the \code{powRICLPM} object, depending on the additional arguments that are set:
#' \itemize{
#'   \item When \code{sample_size = ...}, \code{time_points = ...}, \code{ICC = ...}, and \code{reliability} are set: Estimation information and results for all parameters across experimental conditions.
#'   \item When \code{parameter = "..."} is set: Estimation information and results for a specific parameter across all experimental conditions.
#'   \item No additional arguments: Characteristics of the different experimental conditions are summarized, as well as session info (information that applies to all conditions, such the number of replications, etc.).
#' }
#' \subsection{Interpretation Output}{Depending on the arguments that you set, \code{summary()} prints a table with different analysis outcomes in the columns and where each row refers to a different experimental condition. The following information is available:
#'  \itemize{
#'   \item \code{Sample size}, \code{Time points}, \code{ICC}, \code{Reliability}: The experimental condition that the row refers to.
#'   \item \code{Population}: The true value of the parameter.
#'   \item \code{Avg}: The average (across replications) parameter estimate.
#'   \item \code{Bias}: The difference between the population value and the average parameter estimate.
#'   \item \code{Min}: The lowest (across replications) parameter estimate.
#'   \item \code{SD}: The standard deviation of the parameter estimate over replications.
#'   \item \code{SEAvg}: The average (across replications) standard error of the parameter estimate.
#'   \item \code{MSE}: The parameter mean square error, combining a parameter's bias and efficiency.
#'   \item \code{Accuracy}: The average (across replications) width of the confidence interval.
#'   \item \code{Cover}: The coverage rate, representing the proportion of times (across replications) the true parameter estimate fell in the confidence interval.
#'   \item \code{Power}: The proportion of times (across replications) the confidence interval did not contain zero.
#'   \item \code{Error}: The number of replications that failed to run (i.e., \code{lavaan()} produced an error).
#'   \item \code{Not converged}: The number of replications that did not converge to a solution.
#'   \item \code{Inadmissible}: The number of replications that converged to an inadmissible solution (e.g., a variance estimated to be lower than zero).
#'   }
#' }
#'
#' @examples
#' \dontshow{load(system.file("extdata", "out_preliminary.RData", package = "powRICLPM"))}
#' # Get setup of powRICLPM analysis and convergence issues
#' summary(out_preliminary)
#'
#' # Performance measures for "wB2~wA1" parameter across experimental conditions
#' summary(out_preliminary, parameter = "wB2~wA1")
#'
#' # Performance measures for all parameters, for specific experimental condition
#' summary(out_preliminary, sample_size = 700, time_points = 4, ICC = .3, reliability = 1)
#'
#' @method summary powRICLPM
#' @export
summary.powRICLPM <- function(
    object,
    ...,
    parameter = NULL,
    sample_size = NULL,
    time_points = NULL,
    ICC = NULL,
    reliability = NULL
  ) {

  # Argument validation
  icheck_object_summary(object)
  if (!is.null(parameter)) {icheck_parameter_summary(parameter, object)}
  if (!is.null(sample_size)) {icheck_sample_size_summary(sample_size, object)}
  if (!is.null(time_points)) {icheck_time_points_summary(time_points, object)}
  if (!is.null(ICC)) {icheck_ICC_summary(ICC, object)}
  if (!is.null(reliability)) {icheck_reliability_summary(reliability, object)}


  # Summarize
  if (!is.null(sample_size) && !is.null(time_points) && !is.null(ICC)) {

    # Collect information for print.summary.powRICLPM.condition()
    condition <- Filter(function(x) {
      x$sample_size == sample_size &&
        x$time_points == time_points &&
        x$ICC == ICC &&
        x$reliability == reliability
    }, object$conditions)[[1]]

    ## Simulation results
    results <- condition$estimates[, -1]
    results <- round(results, digits = 3)
    colnames(results) <- c("Population", "Avg", "Bias", "Min", "SD", "SE Avg", "MSE", "Accuracy", "Cover", "Power")
    rownames(results) <- condition$estimates$parameter

    ## Summary of analysis
    summary_replications <- matrix(c(
      object$session$reps,
      condition$estimation_information$n_completed,
      condition$estimation_information$n_nonconvergence,
      condition$estimation_information$n_inadmissible
    ), ncol = 1)
    colnames(summary_replications) <- c("Number of replications")
    rownames(summary_replications) <- c("Requested:", "Completed:", "Convergence issues:", "Inadmissible results:")

    ## Summary of condition
    summary_condition <- matrix(c(
      condition$skewness,
      condition$kurtosis,
      object$session$constraints,
      object$session$bounds,
      object$session$estimate_ME,
      condition$significance_criterion
    ), ncol = 1)
    colnames(summary_condition) <- c("Value")
    rownames(summary_condition) <- c("Skewness:", "Kurtosis:", "Constraints:", "Bounds:", "Estimated measurement error:", "Significance criterion:")

    summary_list <- list(
      summary_condition = summary_condition,
      results = results,
      replications = summary_replications
    )

    print.summary.powRICLPM.condition(summary_list)
    invisible(results)

  } else if (!is.null(parameter)) {

    # Collect information for print.summary.powRICLPM.parameter()
    parameter_df <- give_powRICLPM_results(object, parameter)
    replications_df <- give_powRICLPM_estimation_problems(object)
    parameter_summary <- merge(parameter_df, replications_df, by = c("sample_size","time_points", "ICC", "reliability"))
    colnames(parameter_summary) <- c("Sample size", "Time points", "ICC", "Reliability", "Population", "Avg","Bias", "Min", "SD", "SE Avg", "MSE", "Accuracy", "Cover", "Power", "Error", "Not converged", "Inadmissible")
    print.summary.powRICLPM.parameter(parameter_summary, parameter = parameter)
    invisible(parameter_summary)

  } else {

    # Collect information for print.summary.powRICLPM()
    ## Summary of analysis
    replications_df <- give_powRICLPM_estimation_problems(object)
    colnames(replications_df) <- c("Sample size", "Time points", "ICC", "Reliability", "Error", "Not converged", "Inadmissible")
    version <- utils::packageVersion("powRICLPM")
    print.summary.powRICLPM(replications_df, powRICLPM_version = version)
  }
}





