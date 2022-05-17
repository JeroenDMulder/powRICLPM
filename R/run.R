#' @title
#' Run Monte Carlo Simulation For Single Condition
#'
#' @description
#' \code{run_condition()} runs a Monte Carlo simulation for a single given condition. It generates data based on the \code{pop_synt} element in \code{object}, and estimates an RI-CLPM using its \code{est_synt} element. Data generation and model estimation are done using \pkg{lavaan}.
#'
#' @inheritParams powRICLPM
#' @param object A list with information for running a single simulation condition. See "Details" for an overview of the elements this object must contain.
#' @param progress A \pkg{progressr} object.
#'
#' @details
#' \subsection{Input: Elements in \code{object}}{To successfully run a Monte Carlo simulation, \code{object} needs the following elements:
#' \itemize{
#'   \item \code{sample_size}: The sample size.
#'   \item \code{time_points}: The number of time points.
#'   \item \code{reps}: Number of replications.
#'   \item \code{pop_synt}: \pkg{lavaan} model syntax containing population values for data generation.
#'   \item \code{pop_tab}: \pkg{lavaan} parameter table for data generation.
#'   \item \code{est_synt}: \pkg{lavaan} model syntax for estimation.
#'   \item \code{est_tab}: \pkg{lavaan} parameter table for estimation.
#'   \item \code{skewness}: The skewness value(s) for the observed variables.
#'   \item \code{kurtosis}: The kurtosis value(s) for the observed variables.
#'   \item \code{alpha}: The significance criterion.
#'   \item \code{save_path}: Folder to which simulated data are saved (optional).
#'   }
#' }
#'
#' \subsection{Output}{This function adds the following elements to \code{object}:
#' \itemize{
#'   \item \code{results}: A data frame containing the results (i.e., population values, bias, standard error of the estimate, coverage, power, etc.).
#'   \item \code{gorica_weights}: A numeric vector with GORICA weights for each replication.
#'   \item \code{errors}: A logical vector denoting failed Monte Carlo replications.
#'   \item \code{not_converged}: A logical vector denoting non-converged Monte Carlo replications.
#'   \item \code{inadmissible} A logical vector denoting Monte Carlo replications that resulted in negative variances or non-positive definite matrices.
#'   }
#' }
#'
#' @return A list.
#'
#' @importFrom lavaan simulateData inspect parameterEstimates coef lavaan
#' @noRd
run_condition <- function(object,
                          progress,
                          bounds) {
  k <- 2 # Fixed in v0.0.0.9003 and earlier
  n_parameters <-
    sum(
      factorial(1 + k) / (2 * (k - 1)) * (object$time_points + 1), # Number of (co)variances within- and between-level
      k^2 * (object$time_points - 1) # Number of lagged effects
    )

  # Get the population values
  pv <- as.numeric(object$pop_tab$pv)[object$est_tab$free]
  par <- paste0(
    object$pop_tab$lhs[object$est_tab$free],
    object$pop_tab$op[object$est_tab$free],
    object$pop_tab$rhs[object$est_tab$free]
  )

  coefs <- SEs <- low <- up <- matrix(NA, nrow = n_parameters, ncol = object$reps)
  sigs <- cover <- matrix(FALSE, nrow = n_parameters, ncol = object$reps)
  errors <- warnings <- not_converged <- inadmissible <- rep(FALSE, times = object$reps)

  # Create lavaan function for safe and quiet error and warning handling
  safe_quiet_lavaan <- purrr::safely(purrr::quietly(lavaan::lavaan))
  quiet_lavInspect <- purrr::quietly(lavaan::lavInspect)

  # Create folder for saving simulated data to
  if (!is.na(object$save_path)) {
    save_path_aux <- file.path(
      object$save_path,
      paste0("data_N", object$sample_size, "_T", object$time_points, "_RIvar", object$RI_var)
    )
    dir.create(save_path_aux)
  }

  # Start simulation
  for (r in 1:object$reps) {
    dat <- lavaan::simulateData(
      model = object$pop_synt,
      sample.nobs = object$sample_size,
      skewness = object$skewness,
      kurtosis = object$kurtosis
    )

    if (!is.na(object$save_path)) {
      utils::write.table(dat,
        file = file.path(save_path_aux, paste0("df", r, ".dat")),
        sep = "\t", col.names = FALSE, row.names = FALSE, na = "-999"
      )
    }

    if (bounds) {
      fit <- safe_quiet_lavaan(
        object$est_synt,
        data = dat,
        optim.bounds = list(
          lower = c("lv.var"),
          upper = c("lv.var"),
          lower.factor = c(1.05),
          upper.factor = c(1.30)
        )
      )
    } else {
      fit <- safe_quiet_lavaan(object$est_synt, data = dat)
    }

    # Error and warning checking
    if (!is.null(fit$error)) {
      errors[r] <- TRUE
      next
    }

    if (!identical(fit$result$warnings, character(0))) {
      if (!inspect(fit$result$result, what = "converged")) {
        not_converged[r] <- TRUE
        next # Estimates from non-converged replications are not included in results
      }
      if (quiet_lavInspect(fit$result$result, what = "post.check")$result != TRUE) {
        inadmissible[r] <- TRUE
      }
    }

    # Get estimates
    coefs[, r] <- parameterEstimates(fit$result$result, remove.nonfree = TRUE)$est[1:n_parameters]
    SEs[, r] <- parameterEstimates(fit$result$result, remove.nonfree = TRUE)$se[1:n_parameters]
    sigs[, r] <- parameterEstimates(fit$result$result, remove.nonfree = TRUE)$pvalue[1:n_parameters] < object$alpha
    low[, r] <- parameterEstimates(fit$result$result,
      remove.nonfree = TRUE,
      level = (1 - object$alpha)
    )$ci.lower[1:n_parameters]
    up[, r] <- parameterEstimates(fit$result$result,
      remove.nonfree = TRUE,
      level = (1 - object$alpha)
    )$ci.upper[1:n_parameters]
  }

  # Create and save repList
  if (!is.na(object$save_path)) {
    df_list <- paste0("df", 1:object$reps, ".dat")
    if (sum(not_converged) != 0) { # Delete names of non-converged replications
      df_list <- df_list[-which(not_converged)]
    }
    utils::write.table(df_list,
      file = file.path(save_path_aux, "dfList.dat"),
      sep = "\n", col.names = FALSE, row.names = FALSE, quote = FALSE
    )
  }

  # Compute simulation results
  converged_reps <- object$reps - sum(not_converged)
  avg <- rowMeans(coefs, na.rm = TRUE)
  stdDev <- apply(coefs, 1, stats::sd, na.rm = TRUE)
  SEAvg <- rowMeans(SEs, na.rm = TRUE)
  mse <- rowMeans((coefs - pv)^2, na.rm = TRUE)
  acc <- rowSums(up - low, na.rm = TRUE) / converged_reps
  pwr <- rowSums(sigs, na.rm = TRUE) / converged_reps
  cover <- matrix(NA, nrow = n_parameters, ncol = object$reps)
  for (r in 1:object$reps) {
    cover[, r] <- low[, r] < pv & up[, r] > pv
  }
  coverage <- rowSums(cover, na.rm = TRUE) / converged_reps

  # Structure results
  object$results <- data.frame(par, pv, avg, stdDev, SEAvg, mse, acc, coverage, pwr)
  object$errors <- errors
  object$not_converged <- not_converged
  object$inadmissible <- inadmissible

  # Signal progress bar
  progress()

  return(object)
}
