#' Check if matrix is positive definite
#'
#' `is_PD()` determines if matrix `x` is positive definite by checking if the eigenvalues are non-negative. The function also handles complex eigenvalues.
#'
#' @param x A square matrix.
#'
#' @noRd
is_PD <- function(x) {
  e <- eigen(x, only.values = TRUE)$values
  if (is.complex(e)) {
    if (any(Re(e) < 0) || any(Im(e) < 0)) {
      return(FALSE)
    }
  } else {
    if (any(e < 0)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Check if the implied process is stationary
#'
#' `is_unit()` checks if the process implied by the regression matrix `x` is stationary, by testing if its eigenvalues lie within the unit circle.
#'
#' @param x A square matrix.
#'
#' @noRd
is_unit <- function(x) {
  e <- eigen(x, only.values = TRUE)$values
  if (is.complex(e)) {
    if (any(sqrt((Re(e)^2) + (Im(e)^2)) > 1)) {
      return(FALSE)
    }
  } else {
    if (any(e >= 1)) {
      return(FALSE)
    }
  }
  return(TRUE)
}


#' Check `time_points` argument
#'
#' `check_T()` checks if the `time_points` argument represents (a) valid number(s) of repeated measures.
#'
#' @param x An integer vector.
#'
#' @noRd
check_T <- function(x) {
  if (!all(is.numeric(x))) {
    stop(rlang::format_error_bullets(c(
      "`time_points` must be an integer vector:",
      x = "Not all elements are numeric."
    )))
  }
  if (!all(x %% 1 == 0)) {
    stop(rlang::format_error_bullets(c(
      "`time_points` must be an integer vector:",
      x = "Not all elements are integers."
    )))
  }
  if (any(x < 3)) {
    stop(rlang::format_error_bullets(c(
      "Elements in `time_points` should be larger than 2:",
      i = "The RI-CLPM is not identified with fewer than 3 time points.",
      x = "You've supplied a number of time points smaller than 3."
    )))
  }
  if (any(x > 20)) {
    warning(rlang::format_error_bullets(c(
      "You've supplied (a) large number(s) of time points:",
      i = "This can lead to computational problems in estimation.",
      " " = "You want to consider methods for intensive longitudinal data."
    )))
  }
  return(x)
}

#' Check `sample_size` argument
#'
#' `check_N()` checks if the `sample_size` argument represents (a) valid sample size(s).
#'
#' @param n An integer vector representing sample size(s).
#' @param t An integer vector representing the number(s) of time points.
#'
#' @noRd
check_N <- function(n, t) {
  if (!all(is.numeric(n))) {
    stop(rlang::format_error_bullets(c(
      "`sample_size` must be an integer vector:",
      x = "Not all elements are numeric."
    )))
  }

  if (!all(n %% 1 == 0)) {
    stop(rlang::format_error_bullets(c(
      "`sample_size` must be an integer vector:",
      x = "Not all elements are integers."
    )))
  }
  if (!all(n > 0)) {
    stop(rlang::format_error_bullets(c(
      "`sample_size` must only contain positive integers:",
      x = "You've supplied negative integers."
    )))
  }

  k <- 2 # Number of variables (might be variable in new releases)
  t_max <- max(t)

  n_parameters <- sum(
    factorial(1 + k) / (2 * (k - 1)) * (t_max + 1),
    k^2 * (t_max - 1)
  )

  if (!all(n > n_parameters)) {
    stop(rlang::format_error_bullets(c(
      "The sample size must be larger than the number of parameters estimated (for all experimental conditions):",
      i = paste0("The most estimated parameters in an experimental condition is ", n_parameters, "."),
      x = paste0("The smallest sample size is ", min(n), ".")
    )))
  }
  return(n)
}

#' Check `ICC` argument
#'
#' `check_ICC()` checks if the `ICC` argument represents (a) valid intraclass correlation(s).
#'
#' @param x A numeric vector.
#'
#' @noRd
check_ICC <- function(x) {
  if (!all(is.double(x))) {
    stop(rlang::format_error_bullets(c(
      "`ICC` should be of a double vector:",
      x = "Not all ICC's are doubles."
    )))
  }
  if (!all(x < 1 | x > 0)) {
    stop(rlang::format_error_bullets(c(
      "Elements in `ICC` must be between 0 and 1:",
      x = "Some ICC's are smaller than 0 or larger than 1."
    )))
  }
  if (!all(x < .99)) {
    stop(rlang::format_error_bullets(c(
      "Elements in `ICC` are very close to 1:",
      i = "This can lead to problems with estimation.",
      " " = "Use a maximum of .99."
    )))
  }
  return(x)
}

#' Check `RIcor` argument
#'
#' `check_RIcor()` tests if the `RIcor` argument represents a valid correlation.
#'
#' @param x A double.
#'
#' @noRd
check_RIcor <- function(x) {
  if (!is.double(x)) {
    stop(rlang::format_error_bullets(c(
      "`RI_cor` must be a double:",
      x = paste0("You've provided a type `", typeof(x), "`.")
    )))
  }
  if (x < -1 || x > 1) {
    stop(rlang::format_error_bullets(c(
      "`RI_cor` must be between -1 and 1:",
      i = "Correlations cannot be smaller than -1 or larger than 1.",
      x = ifelse(x < -1,
        "You've provided a values smaller than -1.",
        "You've provided a value larger than 1."
      )
    )))
  }
  if (length(x) > 1) {
    stop(rlang::format_error_bullets(c(
      "`RI_cor` should a double of length 1:",
      x = "You've provided multiple values for `RI_cor`."
    )))
  }
  return(x)
}

#' Check `wSigma` argument
#'
#' `check_wSigma()` tests if `wSigma` represents a valid correlation matrix.
#'
#' @param x A square matrix
#'
#' @noRd
check_wSigma <- function(x) {
  if (!is.matrix(x)) {
    stop(rlang::format_error_bullets(c(
      "`wSigma` must be a square matrix:",
      x = paste0("You've provided a `", typeof(x), "`.")
    )))
  }
  if (all(diag(x) != 1)) {
    stop(rlang::format_error_bullets(c(
      "`wSigma` must be a correlation matrix:",
      i = "Correlation matrices have 1's on the diagonal.",
      x = paste0("Your matrix has on the diagonal the values: ", diag(x)[1], " and ", diag(x)[2])
    )))
  }
  if (!is_PD(x)) {
    stop(rlang::format_error_bullets(c(
      "`wSigma` must be a correlation matrix:",
      i = "Correlation matrices must be positive definite (i.e., eigenvalues smaller than 0).",
      x = "Your `wSigma` is not positive definite. Perhaps try out smaller correlations?"
    )))
  }
  return(x)
}

#' Check `Phi` argument
#'
#' `check_Phi()` tests if `Phi` represents a valid regression matrix for the within-components of the RI-CLPM.
#'
#' @param x A square matrix.
#'
#' @noRd
check_Phi <- function(x) {
  if (!is.matrix(x)) {
    stop(rlang::format_error_bullets(c(
      "`Phi` must be a matrix:",
      x = paste0("Your `Phi` is a `", typeof(x), "`.")
    )))
  }
  if (!is_unit(x)) {
    stop(rlang::format_error_bullets(c(
      "`Phi` must specify a stationary process:",
      i = "This is checked by testing if its eigenvalues lie within the unit circle.",
      x = "The eigenvalues of `Phi` are not within unit circle. Try out smaller lagged effects?"
    )))
  }
  return(x)
}

#' Check computed `Psi` argument
#'
#' `check_Psi()` checks if `Psi` represents a valid variance-covariance matrix by checking if it is positive definite using `is_PD()`.
#'
#' @param x
#'
#' @noRd
check_Psi <- function(x) {
  if (!is_PD(x)) {
    stop(rlang::format_error_bullets(c(
      "The residual variance-covariance matrix for within-components `Psi` must be positive definite:",
      i = "`Psi` is computed from the specified `Phi` and `wSigma` arguments.",
      x = "`Psi` is not positive definite. Try smaller values for `Phi` and `wSigma`?"
    )))
  }
  return(x)
}

#' Check `skewness` argument
#'
#' `check_skewness()` tests if `skewness` is a numeric value of length 1.
#'
#' @param x A numeric value.
#'
#' @noRd
check_skewness <- function(x) {
  if (!is.numeric(x)) {
    stop(rlang::format_error_bullets(c(
      "`skewness` must be of a numeric vector:",
      x = paste0("Your `skewness` is a `", typeof(x), "`.")
    )))
  }
  if (length(x) != 1) {
    stop(rlang::format_error_bullets(c(
      "`skewness` should be of length 1:",
      i = "This version of `powRICLPM` only accepts a single skewness value.",
      x = paste0("You've supplied a `skewness` of length ", length(x), ".")
    )))
  }
  return(x)
}

#' Check `kurtosis` argument
#'
#' `check_kurtosis()` tests if `kurtosis` is a numeric value of length 1.
#'
#' @param x A numeric value.
#'
#' @noRd
check_kurtosis <- function(x, t) {
  if (!is.numeric(x)) {
    stop(rlang::format_error_bullets(c(
      "`kurtosis` must be of a numeric vector:",
      x = paste0("Your `kurtosis` is a `", typeof(x), "`.")
    )))
  }
  if (length(x) != 1) {
    stop(rlang::format_error_bullets(c(
      "`kurtosis` should be of length 1:",
      i = "This version of `powRICLPM` only accepts a single kurtosis value.",
      x = paste0("You've supplied a `kurtosis` of length ", length(x), ".")
    )))
  }
  return(x)
}

#' Check `alpha` argument
#'
#' `check_alpha()` tests if the `alpha` argument is numeric value representing a valid significance criterion.
#'
#' @param x A double.
#'
#' @noRd
check_alpha <- function(x) {
  if (!is.double(x)) {
    stop(rlang::format_error_bullets(c(
      "`alpha` must be a double:",
      x = paste0("You've supplied an `alpha` of type `", typeof(x), "`.")
    )))
  }
  if (1 < x || x < 0) {
    stop(rlang::format_error_bullets(c(
      "`alpha` must be between 0 and 1:",
      x = paste0("Your `alpha` is ", x, ".")
    )))
  }
  return(x)
}

#' Check `seed` argument
#'
#' `check_seed()` checks if a seed is specified. If yes, then it tests if `seed` is an integer. If not, it randomly generates a seed.
#'
#' @param seed An integer.
#'
#' @noRd
check_seed <- function(seed) {
  if (is.na(seed)) {
    seed <- floor(stats::runif(1, -1000000, 1000000))
    warning(rlang::format_error_bullets(c(
      "No seed was specified:",
      i = paste0("A seed was randomly created: ", seed)
    )))
    return(seed)
  }
  if (!is.numeric(seed)) {
    stop(rlang::format_error_bullets(c(
      "`seed` must be `numeric`:",
      x = paste0("You've supplied a seed that is a `", typeof(seed), "`.")
    )))
  } else if (seed %% 1 != 0) {
    stop(rlang::format_error_bullets(c(
      "`seed` should be an `integer`:",
      x = "Your `seed` is not a 'whole' number."
    )))
  }
  return(seed)
}

#' Check `parameter` argument
#'
#' `check_parameter()` tests if the specified parameter is a character string.
#'
#' @param x A character string.
#'
#' @noRd
check_parameter <- function(x) {
  if (!is.null(x)) {
    if (!is.character(x)) {
      stop(rlang::format_error_bullets(c(
        "`parameter` must be a character string:",
        x = paste0("You've supplied a `parameter` of type `", typeof(x), "`.")
      )))
    }
  }
  return(x)
}

#' Check `parameter` argument given `powRICLPM` object
#'
#' `check_parameter_summary()` tests if
#'
#' @param parameter A character string.
#' @param object An `powRICLPM` object.
#'
#' @noRd
check_parameter_summary <- function(parameter, object) {
  check_parameter(parameter)
  condition_length <- purrr::map_int(object$conditions, function(x) {
    length(x$results$par)
  })

  # Check condition with least parameters because these names apply to every condition
  names_pars <- object$conditions[[which.min(condition_length)]]$results$par
  if (!parameter %in% names_pars) {
    stop(rlang::format_error_bullets(c(
      "`parameter` is not valid:",
      i = "`parameter` should be a parameter name in the experimental condition with the least amount of parameters.",
      x = "Perhaps use `summary(object, names = TRUE)` to get an overview of parameter names in the `powRICLPM` object."
    )))
  }
  return(parameter)
}

#' Check `sample_size` in `summary.powRICLPM()`
#'
#' `check_N_summary()` tests if the specified sample size `n` is in the `powRICLPM` object.
#'
#' @param object A `powRICLPM` object.
#' @param n An integer.
#'
#' @noRd
check_N_summary <- function(object, n) {
  n_unique <- unique(purrr::map_dbl(object$condition, function(x) {
    x$sample_size
  }))
  if (!n %in% n_unique) {
    stop(rlang::format_error_bullets(c(
      "`sample_size` must refer to an experimental condition in the `powRICLPM` object with that sample size:",
      i = "The sample size you've indicated is not included in any experimental condition.",
      x = "Perhaps you meant any of the following sample sizes?",
      n_unique
    )))
  }
}

#' Check `time_points` in `summary.powRICLPM()`
#'
#' `check_T_summary()` tests if the specified sample size `time_points` is in the `powRICLPM` object.
#'
#' @param object A `powRICLPM` object.
#' @param time_points An integer.
#'
#' @noRd
check_T_summary <- function(object, time_points) {
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

#' Check `ICC` in `summary.powRICLPM()`
#'
#' `check_ICC_summary()` tests if the specified sample size `ICC` is in the `powRICLPM` object.
#'
#' @param object A `powRICLPM` object.
#' @param ICC A double.
#'
#' @noRd
check_ICC_summary <- function(object, ICC) {
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

#' Check `reps` argument
#'
#' `check_reps()` tests if the `reps` argument is an integer.
#'
#' @param x An integer greater than 0.
#'
#' @noRd
check_reps <- function(x) {
  if (!is.numeric(x)) {
    stop(rlang::format_error_bullets(c(
      "`reps` must be an integer:",
      x = paste0("Your `reps` is a `", typeof(x), "`.")
    )))
  }
  if (x %% 1 != 0) {
    stop(rlang::format_error_bullets(c(
      "`reps` must be an integer:",
      x = paste0("Your `reps` is not a 'whole' number.")
    )))
  }
  if (x < 0) {
    stop(rlang::format_error_bullets(c(
      "`reps` must be a positive integer:",
      x = paste0("Your `reps` is: ", x)
    )))
  }
  return(x)
}

#' Check `target` argument
#'
#' `check_target()` tests if the `target` argument is an integer.
#'
#' @param x A double between 0 and 1.
#'
#' @noRd
check_target <- function(x) {
  if (!is.double(x)) {
    stop(rlang::format_error_bullets(c(
      "`target` must be of type `numeric`:",
      x = paste0("Your `target` is a `", typeof(x), "`.")
    )))
  }
  if (x >= 1 || x <= 0) {
    stop(rlang::format_error_bullets(c(
      "`target` must be between 0 and 1:",
      x = paste0("Your `target` is: ", x)
    )))
  }
  return(x)
}

#' Check `object` argument
#'
#' `check_object()` tests if the `object` argument is an `powRICLPM` object.
#'
#' @param x An object.
#'
#' @noRd
check_object <- function(x) {
  if (!"powRICLPM" %in% class(x)) {
    stop(rlang::format_error_bullets(c(
      "`object` must be `powRICLPM` object:",
      x = paste0("Your `object` is a `", typeof(x), "`.")
    )))
  }
}

#' Check `save_path` argument
#'
#' `check_save()` tests if the `save_path` argument points to an existing folder.
#'
#' @param x A character string.
#'
#' @noRd
check_save <- function(x) {
  if (is.na(x)) {
    return(x)
  } else if (!is.character(x)) {
    stop(rlang::format_error_bullets(c(
      "`save_path` must be a character string:",
      x = paste0("Your `save_path` is a `", typeof(x), "`.")
    )))
  } else if (!dir.exists(x)) {
    stop(rlang::format_error_bullets(c(
      "`save_path` must point to an existing folder:",
      x = "The directory in `save_path` does not exist. Create this folder or change `save_path`."
    )))
  } else {
    return(x)
  }
}

#' Check `bounds` argument
#'
#' `check_bounds()` tests if `bounds` is a logical, and whether bounded estimation can be used (i.e., no constraints are imposed).
#'
#' @param bounds A logical.
#' @param constraints A character string.
#'
#' @noRd
check_bounds <- function(bounds, constraints) {
  if (!is.logical(bounds)) {
    stop(rlang::format_error_bullets(c(
      "`bounds` should be a `logical`:",
      x = paste0("Your `bounds` is a `", typeof(bounds), "`.")
    )))
  }
  if (bounds && constraints != "none") {
    stop(rlang::format_error_bullets(c(
      "Bounded estimation can only be used without constraints on the estimation model:",
      x = paste0("You've placed the following constraints on the estimation model: ", constraints)
    )))
  }
  return(bounds)
}
