#' Extract information from \code{powRICLPM} object
#'
#' Extract information stored within a \code{powRICLPM} object (internally used by \code{\link{print.powRICLPM}} and \code{\link{summary.powRICLPM}}). See "Details" for which pieces of information can be extracted. The information is presented by condition (i.e., sample size, number of time points, and ICC).
#'
#' @param from A \code{powRICLPM} object
#' @param what A character string denoting the information to extract, either "conditions", "estimation_problems", "results", or "names".
#' @param parameter (optional) When \code{what = "results"}, a character string denoting the parameter to extract the results for.
#'
#' @details
#' The following information can be extracted from the \code{powRICLPM} object:
#'
#' \itemize{
#'   \item \code{conditions}: A \code{data.frame} with the different experimental conditions per row, where each condition is defined by a unique combination of sample size, number of time points and ICC.
#'   \item \code{estimation_problems}: The proportion of fatal errors, inadmissible values, or non-converged estimations (columns) per experimental conditions (row).
#'   \item \code{results}: The average estimate (\code{Avg}), minimum estimate (\code{Min}), standard deviation of parameter estimates (\code{stdDev}), the average standard error (\code{SEavg}), the mean square error (\code{MSE}), the average width of the confidence interval (\code{Acc}), the coverage rate (\code{Cov}), and the proportion of times the \emph{p}-value was lower than the significance criterion (\code{Pow}). It requires setting the \code{parameter = "..."} argument.
#'   \item \code{names}: The parameter names in the condition with the least parameters (i.e., parameter names that apply to each experimental condition).
#' }
#'
#' @return A \code{data.frame}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Return data frame with number of estimation problems per experimental condition
#' give(out1, "estimation_problems")
#'
#' # Return data frame with performance measures for "wB2~wA1" per experimental condition
#' give(out1, "results", parameter = "wB2~wA1")
#' }
give <- function(from, what, parameter = NULL) {
  check_object(from)
  check_give(what)
  if (what == "conditions" || what == "sample_size" ||
    what == "time_points" || what == "ICC") {
    give_conditions(object = from)
  } else if (what == "estimation_problems") {
    give_estimation_problems(object = from)
  } else if (what == "results") {
    give_results(object = from, parameter = parameter)
  } else if (what == "names") {
    give_names(object = from)
  } else if (what == "uncertainty") {
    give_uncertainty(object = from)
  } else {
    stop(rlang::format_error_bullets(c(
      "`what` must be 'conditions', 'estimation_problems', 'results', or 'names':",
      x = paste0("Your `what` is ", what, ".")
    )))
  }
}

#' Extract condition information from \code{powRICLPM} object
#'
#' \code{give_conditions()} extracts the sample size, number of time points, and ICC from each experimental condition, returns it in a data frame with the conditions in rows.
#'
#' @param object A \code{powRICLPM} object.
#'
#' @noRd
give_conditions <- function(object) {
  # Combine sample sizes and simulated power across conditions
  d <- purrr::map_dfr(object$conditions, function(condition) {
    # Create data frame
    data.frame(
      sample_size = condition$sample_size,
      time_points = condition$time_points,
      ICC = condition$ICC
    )
  })
  return(d)
}

#' Extract estimation information from \code{powRICLPM} object
#'
#' \code{give_estimation_problems()} extracts the proportion of fatal estimation errors, solutions with inadmissible results, and converges problems from each experimental condition, returns it in a data frame with the conditions in rows.
#'
#' @param object A \code{powRICLPM} object.
#'
#' @noRd
give_estimation_problems <- function(object) {
  # Combine sample sizes and simulated power across conditions
  d <- purrr::map_dfr(object$conditions, function(condition) {
    # Create data frame
    data.frame(
      sample_size = condition$sample_size,
      time_points = condition$time_points,
      ICC = condition$ICC,
      errors = sum(condition$errors),
      not_converged = sum(condition$not_converged),
      inadmissible = sum(condition$inadmissible)
    )
  })
  return(d)
}

#' Extract results from \code{powRICLPM} object
#'
#' \code{give_results()} extracts the results (power analysis dependent variables) from each experimental condition, returns it in a data frame with the conditions in rows.
#'
#' @param object A \code{powRICLPM} object.
#' @inheritParams powRICLPM
#'
#' @noRd
give_results <- function(object, parameter = NULL) {
  check_give_results(object = object, parameter = parameter)
  # Combine sample sizes and simulated power across conditions
  d <- purrr::map_dfr(object$conditions, function(condition) {
    data.frame(
      sample_size = condition$sample_size,
      time_points = condition$time_points,
      ICC = condition$ICC,
      round(condition$estimates[condition$estimates$Par == parameter, -1],
        digits = 3
      )
    )
  })
  return(d)
}

#' Extract parameter names from \code{powRICLPM} object
#'
#' \code{give_names()} extracts and returns the parameter names of the condition with the least number of parameters.
#'
#' @param object A \code{powRICLPM} object.
#'
#' @noRd
give_names <- function(object) {
  condition_length <- purrr::map_int(object$conditions, function(condition) {
    length(condition$estimates$Par)
  })
  object$conditions[[which.min(condition_length)]]$estimates$Par
}



give_uncertainty <- function(object, parameter) {
  d <- purrr::map_dfr(object$conditions, function(condition) {
    data.frame(
      sample_size = condition$sample_size,
      time_points = condition$time_points,
      ICC = condition$ICC,
      condition$uncertainty[which(condition$uncertainty$par == parameter), -1]
    )
  })
  return(d)
}
