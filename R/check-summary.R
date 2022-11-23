#' Check \code{sample_size} in \code{summary.powRICLPM()}
#'
#' \code{check_N_summary()} tests if the specified sample size \code{n} is in the \code{powRICLPM} object.
#'
#' @param object A \code{powRICLPM} object.
#' @param n An integer.
#'
#' @noRd
check_N_summary <- function(object, sample_size) {
  if (length(sample_size) > 1) {
    stop(rlang::format_error_bullets(c(
      "`sample_size` must be a single number:",
      x = paste0("Your `sample_size` is of length ", length(sample_size), ".")
    )))
  }
  sample_size_unique <- unique(purrr::map_dbl(object$condition, function(x) {
    x$sample_size
  }))
  if (!sample_size %in% sample_size_unique) {
    stop(rlang::format_error_bullets(c(
      "`sample_size` must refer to an experimental condition in the `powRICLPM` object with that sample size:",
      i = "The sample size you've indicated is not included in any experimental condition.",
      x = "Perhaps you meant any of the following sample sizes?",
      sample_size_unique
    )))
  }
}

#' Check \code{time_points} in \code{summary.powRICLPM()}
#'
#' \code{check_T_summary()} tests if the specified sample size \code{time_points} is in the \code{powRICLPM} object.
#'
#' @param object A \code{powRICLPM} object.
#' @param time_points An integer.
#'
#' @noRd
check_T_summary <- function(object, time_points) {
  if (length(time_points) > 1) {
    stop(rlang::format_error_bullets(c(
      "`time_points` must be a single number:",
      x = paste0("Your `time_points` is of length ", length(time_points), ".")
    )))
  }
  t_unique <- unique(purrr::map_dbl(object$condition, function(x) {
    x$time_points
  }))
  if (!time_points %in% t_unique) {
    stop(rlang::format_error_bullets(c(
      "`time_points` must refer to an experimental condition in the `powRICLPM` object with that sample size:",
      i = "The `time_points` you've indicated is not included in any experimental condition.",
      x = "Perhaps you meant any of the following number of time points?",
      t_unique
    )))
  }
}

#' Check \code{ICC} in \code{summary.powRICLPM()}
#'
#' \code{check_ICC_summary()} tests if the specified sample size \code{ICC} is in the \code{powRICLPM} object.
#'
#' @param object A \code{powRICLPM} object.
#' @param ICC A double.
#'
#' @noRd
check_ICC_summary <- function(object, ICC) {
  if (length(ICC) > 1) {
    stop(rlang::format_error_bullets(c(
      "`ICC` must be a single number:",
      x = paste0("Your `ICC` is of length ", length(ICC), ".")
    )))
  }
  ICC_unique <- unique(purrr::map_dbl(object$condition, function(x) {
    x$ICC
  }))
  if (!ICC %in% ICC_unique) {
    stop(rlang::format_error_bullets(c(
      "`ICC` must refer to an experimental condition in the `powRICLPM` object with that sample size:",
      i = "The `ICC` you've indicated is not included in any experimental condition.",
      x = "Perhaps you meant any of the following ICCs?",
      ICC_unique
    )))
  }
}
