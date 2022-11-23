#' Check if matrix is positive definite
#'
#' \code{is_PD()} determines if matrix \code{x} is positive definite by checking if the eigenvalues are non-negative. The function also handles complex eigenvalues.
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
#' \code{is_unit()} checks if the process implied by the regression matrix \code{x} is stationary, by testing if its eigenvalues lie within the unit circle.
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


#' Check \code{time_points} argument
#'
#' \code{check_T()} checks if the \code{time_points} argument represents (a) valid number(s) of repeated measures.
#'
#' @inheritParams powRICLPM
#'
#' @noRd
check_T <- function(time_points, estimate_ME) {
  if (!all(is.numeric(time_points))) {
    stop(rlang::format_error_bullets(c(
      "`time_points` must be an integer vector:",
      x = "Not all elements are numeric."
    )))
  }
  if (!all(time_points %% 1 == 0)) {
    stop(rlang::format_error_bullets(c(
      "`time_points` must be an integer vector:",
      x = "Not all elements are integers."
    )))
  }
  if (any(time_points < 3) && !estimate_ME) {
    stop(rlang::format_error_bullets(c(
      "Elements in `time_points` should be larger than 2:",
      i = "The RI-CLPM is not identified with fewer than 3 time points.",
      x = "You've supplied a number of time points smaller than 3."
    )))
  }
  if (any(time_points < 4) && estimate_ME) {
    stop(rlang::format_error_bullets(c(
      "Elements in `time_points` should be larger than 3:",
      i = "If you want to estimate measurement errors, you should have at least 4 waves of data.",
      x = "You've supplied a number of time points smaller than 3."
    )))
  }
  if (any(time_points > 20)) {
    warning(rlang::format_error_bullets(c(
      "You've supplied (a) large number(s) of time points:",
      i = "This can lead to computational problems in estimation.",
      " " = "You might want to consider methods for intensive longitudinal data."
    )))
  }
  return(time_points)
}

#' Check \code{sample_size} argument
#'
#' \code{check_N()} checks if the \code{sample_size} argument represents (a) valid sample size(s) (given constraints).
#'
#' @inheritParams powRICLPM
#'
#' @noRd
check_N <- function(sample_size, time_points, constraints = "none", estimate_ME = FALSE) {
  if (!all(is.numeric(sample_size))) {
    stop(rlang::format_error_bullets(c(
      "`sample_size` must be an integer vector:",
      x = "Not all elements are numeric."
    )))
  }

  if (!all(sample_size %% 1 == 0)) {
    stop(rlang::format_error_bullets(c(
      "`sample_size` must be an integer vector:",
      x = "Not all elements are integers."
    )))
  }
  if (!all(sample_size > 0)) {
    stop(rlang::format_error_bullets(c(
      "`sample_size` must only contain positive integers:",
      x = "You've supplied negative integers."
    )))
  }

  n_parameters <- count_parameters(2, time_points, constraints, estimate_ME)

  if (!all(sample_size > n_parameters)) {
    stop(rlang::format_error_bullets(c(
      "The sample size must be larger than the number of parameters estimated (for all experimental conditions):",
      i = paste0("The highest number of estimated parameters in an experimental condition is ", n_parameters, "."),
      x = paste0("The smallest sample size you have specified is ", min(sample_size), ".")
    )))
  }
  return(sample_size)
}

#' Check \code{ICC} argument
#'
#' \code{check_ICC()} checks if the \code{ICC} argument represents (a) valid intraclass correlation(s).
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

#' Check \code{RIcor} argument
#'
#' \code{check_RIcor()} tests if the \code{RIcor} argument represents a valid correlation.
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

#' Check \code{within_cor} argument
#'
#' \code{check_within_cor()} tests if \code{within_cor} represents a valid correlation.
#'
#' @param x A correlation
#'
#' @return A correlation matrix including \code{x}.
#'
#' @noRd
check_within_cor <- function(x) {
  if (!is.double(x)) {
    stop(rlang::format_error_bullets(c(
      "`within_cor` must be a `double`:",
      x = paste0("You've provided a `", typeof(x), "`.")
    )))
  }
  if (x > 1 || x < 0) {
    stop(rlang::format_error_bullets(c(
      "`within_cor` must be between 0 and 1:",
      i = "A correlation is always between 0 and 1.",
      x = paste0("Your `within_cor` is ", x, ".")
    )))
  }
  return(matrix(c(1, x, x, 1), ncol = 2, byrow = TRUE))
}

#' Check \code{Phi} argument
#'
#' \code{check_Phi()} tests if \code{Phi} represents a valid regression matrix for the within-components of the RI-CLPM.
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

#' Check computed \code{Psi}
#'
#' \code{check_Psi()} checks if \code{Psi} represents a valid variance-covariance matrix by checking if it is positive definite using \code{is_PD()}.
#'
#' @param x
#'
#' @noRd
check_Psi <- function(x) {
  if (!is_PD(x)) {
    stop(rlang::format_error_bullets(c(
      "The residual variance-covariance matrix for within-components `Psi` must be positive definite:",
      i = "`Psi` is computed from the specified `Phi` and `within_cor` arguments.",
      x = "`Psi` is not positive definite. Try smaller values for `Phi` and `within_cor`?"
    )))
  }
  return(x)
}

#' Check \code{skewness} argument
#'
#' \code{check_skewness()} tests if \code{skewness} is a numeric value of length 1.
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

#' Check \code{kurtosis} argument
#'
#' \code{check_kurtosis()} tests if \code{kurtosis} is a numeric value of length 1.
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

#' Check \code{alpha} argument
#'
#' \code{check_alpha()} tests if the \code{alpha} argument is numeric value representing a valid significance criterion.
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

#' Check \code{seed} argument
#'
#' \code{check_seed()} checks if a seed is specified. If yes, then it tests if \code{seed} is an integer. If not, it randomly generates a seed.
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

#' Check \code{parameter} argument
#'
#' \code{check_parameter()} tests if the specified parameter is a character string of length 1.
#'
#' @param x A character string.
#'
#' @noRd
check_parameter_argument <- function(x) {
  if (!is.null(x)) {
    if (length(x) > 1) {
      stop(rlang::format_error_bullets(c(
        "`parameter` must be a character string of length 1:",
        x = paste0("Your `parameter` is of length ", length(x), ".")
      )))
    }
    if (!is.character(x)) {
      stop(rlang::format_error_bullets(c(
        "`parameter` must be a character string:",
        x = paste0("You've supplied a `parameter` of type `", typeof(x), "`.")
      )))
    }
  }
  return(x)
}

#' Check \code{parameter} argument given \code{powRICLPM} object
#'
#' \code{check_parameter_summary()} tests if the character string refers to a parameter within the \code{powRICLPM} object.
#'
#' @param parameter A character string.
#' @param object An \code{powRICLPM} object.
#'
#' @noRd
check_parameter_available <- function(parameter, object) {
  condition_length <- purrr::map_int(object$conditions, function(x) {
    length(x$estimates$parameter)
  })
  names_pars <- object$conditions[[which.min(condition_length)]]$estimates$parameter
  if (!parameter %in% names_pars) {
    stop(rlang::format_error_bullets(c(
      x = "`parameter` is not available across all experimental conditions.",
      i = "Perhaps use `give(object, what = 'names')` to get an overview of parameter names in the `powRICLPM` object."
    )))
  }
  return(parameter)
}

#' Check \code{reps} argument
#'
#' \code{check_reps()} tests if the \code{reps} argument is an integer.
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

#' Check \code{target} argument
#'
#' \code{check_target()} tests if the \code{target} argument is an integer.
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

#' Check \code{object} argument
#'
#' \code{check_object()} tests if the \code{object} argument is an \code{powRICLPM} object.
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


#' Check \code{bounds} argument
#'
#' \code{check_bounds()} tests if \code{bounds} is a logical, and whether bounded estimation can be used (i.e., no constraints are imposed).
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

#' Check \code{reliability} argument
#'
#' \code{check_reliability()} checks if the \code{reliability} argument is a valid reliability coefficient.
#'
#' @inheritParams powRICLPM
#'
#' @noRd
check_reliability <- function(reliability) {
  if (!is.numeric(reliability)) {
    stop(rlang::format_error_bullets(c(
      "`reliability` must be `numeric`:",
      x = paste0("Your `reliability` is a `", class(reliability), "`.")
    )))
  }
  if (length(reliability) > 1) {
    stop(rlang::format_error_bullets(c(
      "`reliability` must be a single `numeric` value:",
      x = paste0("Your `reliability` is of length ", length(reliability), ".")
    )))
  }
  if (reliability < 0 || reliability > 1) {
    stop(rlang::format_error_bullets(c(
      "`reliability` must be between 0 and 1:",
      x = paste0("Your `reliability` is ", reliability, ".")
    )))
  }
  return(reliability)
}

#' Check \code{estimate_ME} argument
#'
#' \code{check_estimate_ME()} checks if \code{estimate_ME} is a logical of length 1.
#'
#' @inheritParams powRICLPM
#'
#' @noRd
check_estimate_ME <- function(estimate_ME) {
  if (!is.logical(estimate_ME)) {
    stop(rlang::format_error_bullets(c(
      "`estimate_ME` must be a `logical`:",
      x = paste0("Your `estimate_ME` is a `", class(estimate_ME), "`.")
    )))
  }
  if (length(estimate_ME) > 1) {
    stop(rlang::format_error_bullets(c(
      "`estimate_ME` must be a single `logical`:",
      x = paste0("Your `estimate_ME` is of length ", length(estimate_ME), ".")
    )))
  }
  return(estimate_ME)
}

#' Check \code{constraints} argument
#'
#' \code{check_constraints()} checks if the \code{constraints} arguments refers to a valid set of constraints.
#'
#' @inheritParams powRICLPM
#'
#' @noRd
check_constraints <- function(constraints, estimate_ME) {
  if (length(constraints) > 1) {
    stop(rlang::format_error_bullets(c(
      "You can only specify a single set of constraints at the time:",
      x = paste0("You specified ", length(constraints), " constraints.")
    )))
  }
  if (!constraints %in% c("none", "lagged", "residuals", "within", "stationarity", "ME")) {
    stop(rlang::format_error_bullets(c(
      "`constraints` must be 'none', 'lagged', 'residuals', 'within', 'stationarity', or 'ME':",
      x = paste0("Your `constraints` is ", constraints, ".")
    )))
  }
  if (constraints == "ME" & !estimate_ME) {
    stop(rlang::format_error_bullets(c(
      "`constraints = 'ME' is only possible when `estimate_ME = TRUE`",
      x = "Your `estimate_ME` is FALSE."
    )))
  }
  return(constraints)
}

#' Check \code{estimator} argument
#'
#' \code{check_estimator()} checks if the \code{estimator} argument refers to a valid maximum likelihood estimators implemented in \pkg{lavaan}.
#'
#' @inheritParams powRICLPM
#'
#' @noRd
check_estimator <- function(estimator, skewness, kurtosis) {
  if (is.na(estimator)) {
    if (skewness != 0 || kurtosis != 0) {
      message(rlang::format_error_bullets(c(
        i = "Estimator defaulting to `MLR` for skewed and/or kurtosed data."
      )))
      return("MLR")
    } else {
      return("ML")
    }
  }
  if (!estimator %in% c("ML", "MLR", "MLM", "MLMVS", "MLMV", "MLF")) {
    stop(rlang::format_error_bullets(c(
      "`estimator` must be 'ML', 'MLR', 'MLM', 'MLMVS', 'MLMV', or 'MLF':",
      x = paste0("Your `estimator` is ", estimator, ".")
    )))
  }
  return(estimator)
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

#' Check `save_path` argument
#'
#' `check_save()` tests if the `save_path` argument points to an existing folder.
#'
#' @param x A character string.
#'
#' @noRd
check_save_path <- function(x) {
  if (is.null(x)) {
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
