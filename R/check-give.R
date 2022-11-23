#' Check \code{what} argument
#'
#' \code{check_give} checks the \code{what} argument of \code{give()} by testing if it is a character string of length 1.
#'
#' @param what A character string of length 1.
#'
#' @noRd
check_give <- function(what) {
  if (!is.character(what)) {
    stop(rlang::format_error_bullets(c(
      "`what` must be a character string:",
      x = paste0("Your `what` is a `", typeof(what), "`.")
    )))
  }
  if (length(what) > 1) {
    stop(rlang::format_error_bullets(c(
      "`what` must be of length 1:",
      x = paste0("Your `what` contains ", length(what), " elements.")
    )))
  }
}

#' Check arguments for \code{give(what = "results")}
#'
#' \code{check_give_results} checks that the \code{parameter} argument is correctly specified when results are extracted using \code{give(what = "results")}.
#'
#' @param object A \code{powRICLPM} object.
#' @param parameter A character string.
#'
#' @noRd
check_give_results <- function(object, parameter) {
  if (!is.null(parameter)) {
    check_parameter_argument(parameter)
    check_parameter_available(parameter, object)
  } else if (!is.null(object$session$parameter)) {
    parameter <- object$session$parameter
  } else {
    stop(rlang::format_error_bullets(c(
      "`give(what = 'results')` must know which parameter to extract results for:",
      x = "You've not supplied a `parameter` argument, nor set `parameter` when running `powRICLPM()`."
    )))
  }
}


