#' Check \code{target} Argument
#'
#' \code{icheck_target()} tests if the \code{target} argument is an integer.
#'
#' @noRd
icheck_target <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.double(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a numeric:",
        x = paste0("Your {.arg {arg}} is a ", typeof(x), ".")
      ),
      call = call
    )
  }
  if (x >= 1 || x <= 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be between 0 and 1:",
        x = paste0("Your {.arg {arg}} is ", x, ".")
      )
    )
  }
}

#' Check \code{time_points} Argument
#'
#' \code{icheck_T()} checks if the \code{time_points} argument represents (a) valid number(s) of repeated measures.
#'
#' @noRd
icheck_T <- function(x, ME, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!all(is.numeric(x))) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a vector of integers:",
        x = "Not all elements are numeric."
      )
    )
  }
  if (!all(x %% 1 == 0)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a vector of integers:",
        x = "Not all elements are integers."
      )
    )
  }
  if (any(x < 3) && !ME) {
    cli::cli_abort(
      c(
        "Elements in {.arg {arg}} should be larger than 2:",
        i = "The RI-CLPM is not identified with fewer than 3 time points.",
        x = "You've supplied a number of time points smaller than 3."
      )
    )
  }
  if (any(x < 4) && ME) {
    cli::cli_abort(
      c(
        "Elements in {.arg {arg}} should be larger than 3:",
        i = "If you want to estimate measurement errors in the RI-CLPM, you should have at least 4 waves of data.",
        x = "You've supplied a number of time points smaller than 4."
      )
    )
  }
  if (any(x > 15)) {
    cli::cli_warn(
      c(
        "You've supplied a large number of time points:",
        i = "This can lead to computational problems in estimation. You might want to consider methods for intensive longitudinal data."
      )
    )
  }
}

#' Check \code{ICC} Argument
#'
#' \code{icheck_ICC()} checks if the \code{ICC} argument represents (a) valid intraclass correlation(s).
#'
#' @noRd
icheck_ICC <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!all(is.double(x))) {
    cli::cli_abort(
      c(
        "{.arg {arg}} should be a double (vector):",
        x = "Not all elements in {.arg {arg}} are a double."
      )
    )
  }
  if (!all(x < 1 | x > 0)) {
    cli::cli_abort(
      c(
        "Elements in {.arg {arg}} must be between 0 and 1:",
        x = "Some elements in {.arg {arg}} are smaller than 0 or larger than 1."
      )
    )
  }
  if (!all(x < .99)) {
    cli::cli_abort(
      c(
        "Elements in {.arg {arg}} are very close to 1:",
        i = "This can lead to problems with estimation. Use a maximum of .99."
      )
    )
  }
}

#' Check Correlation Arguments
#'
#' \code{icheck_cor()} tests if an argument represents a valid correlation.
#'
#' @noRd
icheck_cor <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.double(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a double:",
        x = paste0("You've provided a ", typeof(x), ".")
      )
    )
  }
  if (x < -1 || x > 1) {
    cli::cli_abort(
      c(
        "A correlation must be between -1 and 1:",
        x = paste0("Your {.arg {arg}} was ", x, ".")
      )
    )
  }
  if (length(x) > 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} should a double of length 1:",
        x = "You've provided multiple values for {.arg {arg}}."
      )
    )
  }
}

#' Check \code{Phi} Argument
#'
#' \code{icheck_Phi()} tests if \code{Phi} represents a valid regression matrix for the within-components of the RI-CLPM.
#'
#' @noRd
icheck_Phi <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.matrix(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a matrix:",
        x = paste0("Your {.arg {arg}} is a ", typeof(x), ".")
      )
    )
  }
  if (!is_unit(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must specify a stationary process:",
        i = "This is checked by testing if the eigenvalues of {.arg {arg}} lie within unit circle.",
        x = "Eigenvalues of {.arg {arg}} are not within unit circle. Try out smaller lagged effects?"
      )
    )
  }
}

#' Check \code{reliability} Argument
#'
#' \code{icheck_rel()} checks if the \code{reliability} argument is a valid reliability coefficient.
#'
#' @noRd
icheck_rel <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!all(is.numeric(x))) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a numeric (vector):",
        x = paste0("Your {.arg {arg}} is a ", class(x), ".")
      )
    )
  }
  if (!all(x <= 1 & x > 0)) {
    cli::cli_abort(
      c(
        "Elements in {.arg {arg}} must be between 0 and 1:",
        x = paste0("Some elements in {.arg {arg}} are smaller than 0 or larger than 1.")
      )
    )
  }
  if (!all(x > 0.1)) {
    cli::cli_abort(
      c(
        "Some elements in {.arg {arg}} are close to 0:",
        x = paste0("A low reliability can lead to problems with estimation. Use a minimum of 0.1.")
      )
    )
  }
}

#' Check \code{moment} Arguments
#'
#' \code{icheck_moment()} tests if a \code{moment} argument (e.g., skewness, kurtosis) is a numeric value of length 1.
#'
#' @noRd
icheck_moment <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a numeric (vector):",
        x = paste0("Your {.arg {arg}} is a ", typeof(x), ".")
      )
    )
  }
  if (length(x) != 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} should be of length 1:",
        i = "This version of `powRICLPM` only accepts a single value for this factor.",
        x = paste0("Your {.arg {arg}} is of length ", length(x), ".")
      )
    )
  }
}

#' Check \code{significance_criterion} Argument
#'
#' \code{icheck_significance_criterion()} tests if the \code{alpha} argument is numeric value representing a valid significance criterion.
#'
#' @param x A double.
#'
#' @noRd
icheck_significance_criterion <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.double(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a double:",
        x = paste0("Your {.arg {arg}} is a ", typeof(x), ".")
      )
    )
  }
  if (1 < x || x < 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be between 0 and 1:",
        x = paste0("Your {.arg {arg}} is ", x, ".")

      )
    )
  }
}

#' Check \code{estimate_ME} Argument
#'
#' \code{icheck_ME()} checks if \code{estimate_ME} is a logical of length 1.
#'
#' @noRd
icheck_ME <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.logical(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a logical:",
        x = paste0("Your {.arg {arg}} is a ", class(x), ".")
      )
    )
  }
  if (length(x) > 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} should be of length 1:",
        x = paste0("Your {.arg {arg}} is of length ", length(x), ".")
      )
    )
  }
}

#' Check \code{reps} Argument
#'
#' \code{icheck_reps()} tests if provided argument is a valid number of replications.
#'
#' @noRd
icheck_reps <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be an integer:",
        x = paste0("Your {.arg {arg}} is a ", typeof(x), ".")
      )
    )
  }
  if (x %% 1 != 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be an integer:",
        x = paste0("Your {.arg {arg}} is not a 'whole' number.")
      )
    )
  }
  if (x < 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a positive integer:",
        x = paste0("Your {.arg {arg}} is: ", x, ".")
      )
    )
  }
}


#' Check \code{seed} Argument
#'
#' \code{icheck_seed()} checks if a seed is specified. If yes, then it tests provided argument is a valid seed. If not, a randomly generates is returned.
#'
#' @noRd
icheck_seed <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (is.na(x)) {
    x <- floor(stats::runif(1, 0, 1000000))
    cli::cli_warn(
      c("No seed was specified. A random seed was generated: {.value {x}}")
    )
    return(x)
  }
  if (!is.numeric(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a numeric:",
        x = paste0("Your {.arg {arg}} is a {typeof(x)}.")
      )
    )
  } else if (x %% 1 != 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} should be an integer:",
        x = "Your {.arg {arg}} is not a 'whole' number."
      )
    )
  }
  return(x)
}

#' Check \code{constraints} Argument
#'
#' \code{check_constraints()} checks if the \code{constraints} arguments refers to a valid set of constraints.
#'
#' @noRd
icheck_constraints <- function(x, ME, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (length(x) > 1) {
    cli::cli_abort(
      c(
        "You can only specify a single set of constraints at the time:",
        x = paste0("You specified ", length(x), " constraints.")
      )
    )
  }
  if (!x %in% c("none", "lagged", "residuals", "within", "stationarity", "ME")) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be 'none', 'lagged', 'residuals', 'within', 'stationarity', or 'ME':",
        x = paste0("Your {.arg {arg}} is ", x, ".")
      )
    )
  }
  if (x == "ME" & !ME) {
    cli::cli_abort(
      c(
        "{.arg {arg}} can only be set to 'ME' when `estimate_ME = TRUE`:",
        x = "Your {.arg {arg}} is FALSE."
      )
    )
  }
}

#' Check \code{estimator} Argument
#'
#' \code{icheck_estimator()} checks if the \code{estimator} argument refers to a valid maximum likelihood estimators implemented in \pkg{lavaan}.
#'
#' @noRd
icheck_estimator <- function(x, skewness, kurtosis, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (is.na(x)) {
    if (skewness != 0 || kurtosis != 0) {
      cli::cli_alert(
        c(
          i = "Estimator defaulting to `MLR` for skewed and/or kurtosed data."
        )
      )
      return("MLR")
    } else {
      return("ML")
    }
  }
  if (!x %in% c("ML", "MLR", "MLM", "MLMVS", "MLMV", "MLF")) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be 'ML', 'MLR', 'MLM', 'MLMVS', 'MLMV', or 'MLF':",
        x = paste0("Your {.arg {arg}} is '", x, "'.")
      )
    )
  }
}

#' Check \code{save_path} Argument
#'
#' \code{icheck_path()} tests if the provided argument is a valid path argument that points to an existing folder.
#'
#' @noRd
icheck_path <- function(x, software, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.null(x)) {
    if (!is.character(x)) {
      cli::cli_abort(
        c(
          "{.arg {arg}} must be a character string:",
          x = paste0("Your {.arg {arg}} is a ", typeof(x), ".")
        )
      )
    }
    if (!dir.exists(x)) {
      cli::cli_abort(
        c(
          "{.arg {arg}} must point to an existing folder:",
          x = "The directory in {.arg {arg}} does not exist. Create this folder or change {.arg {arg}}."
        )
      )
    }
  } else if (is.null(x) && software == "Mplus") {
    path <- getwd()
    return(path)
  }
  return(x)
}

#' Check computed \code{Psi} matrix
#'
#' \code{icheck_Psi()} checks if \code{Psi} represents a valid variance-covariance matrix.
#'
#' @noRd
icheck_Psi <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is_PD(x)) {
    cli::cli_abort(
      c(
        "The residual variance-covariance matrix for within-components (Psi) must be positive definite:",
        i = "It is computed from the specified `Phi` and `within_cor` arguments.",
        x = "Psi is not positive definite. Try smaller values for `Phi` and `within_cor`?"
      )
    )
  }
}

#' Check \code{sample_size} Argument
#'
#' \code{icheck_N()} checks if the \code{sample_size} argument represents valid sample size(s) given the specified constraints, number of time points, and estimation of measurement error.
#'
#' @noRd
icheck_N <- function(x, t, constraints = "none", ME = FALSE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!all(is.numeric(x))) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be an integer vector:",
        x = "Not all elements are numeric."
      )
    )
  }
  if (!all(x %% 1 == 0)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be an integer vector:",
        x = "Not all elements are integers."
      )
    )
  }
  if (!all(x > 0)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must only contain positive integers:",
        x = "Your {.arg {arg}} contains negative numbers."
      )
    )
  }
  n_parameters <- count_parameters(2, t, constraints, ME)
  if (!all(x > n_parameters)) {
    cli::cli_abort(
      c(
        "The sample size must be larger than the number of parameters estimated (for all experimental conditions):",
        i = paste0("The highest number of estimated parameters in an experimental condition is ", n_parameters, "."),
        x = paste0("The smallest sample size you have specified is ", min(x), ".")
      )
    )
  }
}

#' Check \code{bounds} Argument
#'
#' \code{check_bounds()} tests if \code{bounds} is a logical, and whether bounded estimation can be used (i.e., no constraints are imposed).
#'
#' @inheritParams powRICLPM
#'
#' @noRd
icheck_bounds <- function(bounds, constraints, software,
                          con = rlang::caller_arg(constraints), soft = rlang::caller_arg(software)) {
  if (!is.logical(bounds)) {
    cli::cli_abort(
      c(
        "`bounds` should be a `logical`:",
        x = paste0("Your `bounds` is a `", typeof(bounds), "`.")
      )
    )
  }
  if (bounds && constraints != "none") {
    cli::cli_abort(
      c(
        "Bounded estimation can only be used without constraints on the estimation model:",
        x = "You've placed the following constraints on the estimation model: {.value {con}}"
      )
    )
  }
  if (bounds && software == "Mplus") {
    cli::cli_abort(
      c(
        "Bounded estimation can only be used with lavaan:",
        x = "You've set the `software` argument to: {.arg {soft}}"
      )
    )
  }
}


#' Check `software` Argument
#'
#' `icheck_software()` tests if the `software` argument is a valid option, either "lavaan" or "Mplus".
#'
#' @noRd
icheck_software <- function(
    x, skewness, kurtosis,
    skew = rlang::caller_arg(skewness), kurt = rlang::caller_arg(kurtosis)
  ) {
  if (!(x %in% c("lavaan", "Mplus"))) {
    cli::cli_abort(
      c(
        "The `software` argument does not specify a valid option:",
        x = "Your `software` is: {.value {x}}",
        i = "Please specify either `lavaan` or `Mplus`."
      )
    )
  }
  if (x == "Mplus" && !(skewness == 0 && kurtosis == 0)) {
    cli::cli_abort(
      c(
        "When using Mplus, it is not possible to generate skewed or kurtosed data:",
        x = "You've set `skewness` to {.arg {skew}} and `kurtosis` to {.arg {kurt}}."
      )
    )
  }
}


icheck_alpha <- function(x) {
  cli::cli_warn(c("`alpha` is deprecated and will be removed in a future version. Please use 'significance_criterion' instead."))
  return(x)
}




