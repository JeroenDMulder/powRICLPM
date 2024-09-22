#' Extract Information From \code{powRICLPM} Object
#'
#' Extract information stored within a \code{powRICLPM} object (internally used by \code{\link{print.powRICLPM}} and \code{\link{summary.powRICLPM}}). See "Details" for which pieces of information can be extracted. The information is presented by condition (i.e., sample size, number of time points, and ICC).
#'
#' @param from A \code{powRICLPM} object
#' @param what A character string, denoting the information to extract, either "conditions", "estimation_problems", "results", or "names".
#' @param parameter (optional) When \code{what = "results"}, a character string denoting the parameter to extract the results for.
#'
#' @details
#' The following information can be extracted from the \code{powRICLPM} object:
#'
#' \itemize{
#'   \item \code{conditions}: A \code{data.frame} with the different experimental conditions per row, where each condition is defined by a unique combination of sample size, number of time points and ICC.
#'   \item \code{estimation_problems}: The proportion of fatal errors, inadmissible values, or non-converged estimations (columns) per experimental conditions (row).
#'   \item \code{results}: The average estimate (\code{average}), minimum estimate (\code{minimum}), standard deviation of parameter estimates (\code{SD}), the average standard error (\code{SEavg}), the mean square error (\code{MSE}), the average width of the confidence interval (\code{accuracy}), the coverage rate (\code{coverage}), and the proportion of times the \emph{p}-value was lower than the significance criterion (\code{power}). It requires setting the \code{parameter = "..."} argument.
#'   \item \code{names}: The parameter names in the condition with the least parameters (i.e., parameter names that apply to each experimental condition).
#' }
#'
#' @return A \code{data.frame}.
#' @export
#'
#' @examples
#' \dontshow{
#' load(system.file("extdata", "out_preliminary.RData", package = "powRICLPM"))
#' }
#' # Return data frame with number of estimation problems per experimental condition
#' give(out_preliminary, "estimation_problems")
#'
#' # Return data frame with performance measures for "wB2~wA1" per experimental condition
#' give(out_preliminary, "results", parameter = "wB2~wA1")
#'
#' # Return character vector with parameter names
#' give(out_preliminary, "names")
give <- function(from, what, parameter = NULL) {

  # Input checking
  icheck_object_summary(from)
  icheck_what_give(what)

  # Call to relevant give_*() based on `what` argument
  if (what == "conditions" || what == "sample_size" || what == "time_points" ||
      what == "ICC" || what == "reliability") {
    give_powRICLPM_conditions(object = from)
  } else if (what == "estimation_problems") {
    give_powRICLPM_estimation_problems(object = from)
  } else if (what == "results") {
      give_powRICLPM_results(object = from, parameter = parameter)
  } else if (what == "names") {
      give_powRICLPM_parameter_names(object = from)
  } else if (what == "uncertainty") {
      give_powRICLPM_MCSE_parameter(object = from, parameter = parameter)
  }
}

give_powRICLPM_conditions <- function(object) {

  # Combine sample sizes and simulated power across conditions
  d <- do.call(rbind, lapply(object$conditions, function(condition) {
    data.frame(
      sample_size = condition$sample_size,
      time_points = condition$time_points,
      ICC = condition$ICC,
      reliability = condition$reliability,
      stringsAsFactors = FALSE
    )
  }))
  return(d)
}

give_powRICLPM_estimation_problems <- function(object) {

  # Combine sample sizes and simulated power across conditions
  d <- do.call(rbind, lapply(object$conditions, function(condition) {
    data.frame(
      sample_size = condition$sample_size,
      time_points = condition$time_points,
      ICC = condition$ICC,
      reliability = condition$reliability,
      errors = condition$estimation_information$n_error,
      not_converged = condition$estimation_information$n_nonconvergence,
      inadmissible = condition$estimation_information$n_inadmissible,
      stringsAsFactors = FALSE
    )
  }))
  return(d)
}

give_powRICLPM_results <- function(object, parameter = NULL) {

  # Combine simulation results across experimental conditions
  d <- do.call(rbind, lapply(object$conditions, function(condition) {

    # Extract and round estimates per condition
    estimates <- condition$estimates[condition$estimates$parameter == parameter, -1]
    estimates <- round(estimates, digits = 3)

    # Combine extracted info in data frame
    data.frame(
      sample_size = condition$sample_size,
      time_points = condition$time_points,
      ICC = condition$ICC,
      reliability = condition$reliability,
      estimates
    )
  }))
  return(d)
}

give_powRICLPM_parameter_names <- function(object) {

  # Determine length of parameter vector per condition
  condition_lengths <- sapply(object$conditions, function(condition) {
    length(condition$estimates$parameter)
  })

  # Find index of condition with minimum length
  min_length_index <- which.min(condition_lengths)

  # Extract parameter vector from condition with minimum length
  out <- object$conditions[[min_length_index]]$estimates$parameter
  return(out)
}


give_powRICLPM_MCSE_parameter <- function(object, parameter) {

  # Combine data frames across conditions
  d <- do.call(rbind, lapply(object$conditions, function(condition) {

    # Extract rows from uncertainty where parameter matches
    uncertainty_filtered <- condition$MCSEs[which(condition$estimates$parameter == parameter), ]

    # Create data frame
    data.frame(
      sample_size = condition$sample_size,
      time_points = condition$time_points,
      ICC = condition$ICC,
      reliability = condition$reliability,
      uncertainty_filtered,
      stringsAsFactors = FALSE
    )
  }))
  return(d)
}
