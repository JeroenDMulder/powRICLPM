icheck_object_summary <- function(object, arg = rlang::caller_arg(object), call = rlang::caller_env()) {
  if (!inherits(object, "powRICLPM")) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be of class 'powRICLPM'."
      ),
      call = call
    )
  }
}


icheck_sample_size_summary <- function(sample_size, object, arg = rlang::caller_arg(sample_size), call = rlang::caller_env()) {

  if (length(sample_size) > 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single number:",
        "x" = "Your {.arg {arg}} is of length {length(sample_size)}."
      ),
      call = call
    )
  }

  sample_sizes <- lapply(object$conditions, function(x) {x$sample_size})

  if (!any(sample_size == sample_sizes)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must refer to an experimental condition in the {.cls {class(object)}} object with that sample size:",
        "i" = "The sample size you've indicated is not included in any experimental condition.",
        "x" = "Perhaps you meant any of the following sample sizes?",
        paste(unique(sample_sizes), collapse = ", ")
      ),
      call = call
    )
  }
}


icheck_parameter_summary <- function(x, object, parameter, arg = rlang::caller_arg(x), call = rlang::caller_env()) {

  if (length(x) > 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a character string of length 1:",
        "x" = "Your {.arg {arg}} is of length {length(sample_size)}."
      ),
      call = call
    )
  }

  if (!is.character(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a character string:",
        "x" = "Your {.arg {arg}} is of type {typeof(arg)}"
      ),
      call = call
    )
  }

  number_of_parameters <- lapply(object$conditions, function(x) {length(x$estimates$parameter)})
  names_parameters <- object$conditions[[which.min(number_of_parameters)]]$estimates$parameter

  if (!any(x == names_parameters)) {
    cli::cli_abort(
      c(
        "Your {.arg {arg}} is not available across all experimental conditions.",
        "i" = "Perhaps use `give(object, what = 'names')` to get an overview of parameter names in the `powRICLPM` object."
      ),
      call = call
    )
  }
}



icheck_time_points_summary <- function(time_points, object, arg = rlang::caller_arg(time_points), call = rlang::caller_env()) {

  if (length(time_points) > 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single number:",
        "x" = "Your {.arg {arg}} is of length {length(time_points)}."
      ),
      call = call
    )
  }

  time_points_object <- lapply(object$conditions, function(x) {x$time_points})

  if (!any(time_points == time_points_object)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must refer to an experimental condition in the {.cls {class(object)}} object with that number of time points:",
        "i" = "The {.arg {arg}} you've indicated is not included in any experimental condition.",
        "x" = "Perhaps you meant any of the following number of time points?",
        paste(unique(time_points_object), collapse = ", ")
      ),
      call = call
    )
  }
}


icheck_ICC_summary <- function(ICC, object, arg = rlang::caller_arg(ICC), call = rlang::caller_env()) {
  if (length(ICC) > 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single number:",
        "x" = "Your {.arg {arg}} is of length {length(ICC)}."
      ),
      call = call
    )
  }

  ICCs <- lapply(object$conditions, function(x) {x$ICC})

  if (!any(ICC == ICCs)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must refer to an experimental condition in the {.cls {class(object)}} object with that ICC:",
        "i" = "The {.arg {arg}} you've indicated is not included in any experimental condition.",
        "x" = "Perhaps you meant any of the following ICCs?",
        paste(unique(ICCs), collapse = ", ")
      ),
      call = call
    )
  }
}


icheck_reliability_summary <- function(reliability, object, arg = rlang::caller_arg(reliability), call = rlang::caller_env()) {

  if (length(reliability) > 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single number:",
        "x" = "Your {.arg {arg}} is of length {length(reliability)}."
      ),
      call = call
    )
  }

  reliabilities <- lapply(object$conditions, function(x) {x$reliability})

  if (!any(reliability == reliabilities)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must refer to an experimental condition in the {.cls {class(object)}} object with that reliability:",
        "i" = "The reliability you've indicated is not included in any experimental condition.",
        "x" = "Perhaps you meant any of the following reliabilities?",
        paste(unique(reliabilities), collapse = ", ")
      ),
      call = call
    )
  }
}


