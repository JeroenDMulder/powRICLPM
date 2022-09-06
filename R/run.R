#' @title
#' Run Monte Carlo Simulation For Single Condition
#'
#' @description
#' \code{run_condition()} runs a Monte Carlo simulation for a single condition. It generates data based on the \code{pop_synt} element in \code{condition}, and estimates an RI-CLPM using its \code{est_synt} element. Data generation and model estimation are done using \pkg{lavaan}.
#'
#' @inheritParams powRICLPM
#' @param condition A list with information for running a single simulation condition. See "Details" for an overview of the elements this condition must contain.
#' @param progress A \pkg{progressr} object.
#'
#' @details
#' \subsection{Input: Elements in \code{condition}}{To successfully run a Monte Carlo simulation, \code{condition} needs the following elements:
#' \itemize{
#'   \item \code{sample_size}: The sample size.
#'   \item \code{time_points}: The number of time points.
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
#' \subsection{Output}{This function adds the following elements to \code{condition}:
#' \itemize{
#'   \item \code{results}: A data frame containing the results (i.e., population values, bias, standard error of the estimate, coverage, power, etc.).
#'   \item \code{uncertainty}: A data frame containing the 95% bootstrap confidence interval for the power estimate for all parameters in the RI-CLPM.
#'   \item \code{errors}: A logical vector denoting failed Monte Carlo replications.
#'   \item \code{not_converged}: A logical vector denoting non-converged Monte Carlo replications.
#'   \item \code{inadmissible} A logical vector denoting Monte Carlo replications that resulted in negative variances or non-positive definite matrices.
#'   }
#'   Results include estimates from replications with inadmissible estimates, which could occur for (residual) variances. The problematic parameter can be identified by inspecting the minimum estimates across runs: Negative values imply that the inadmissible result warning was due to that parameter. Parameter estimates from non-converged and erroneous runs are excluded from the results.
#' }
#'
#' @return A list.
#'
#' @importFrom lavaan simulateData lavInspect parameterEstimates lavaan
#' @noRd
run_condition <- function(condition,
                          progress,
                          bounds,
                          estimator,
                          reps,
                          bootstrap_reps,
                          constraints,
                          save_path) {

  # Get the population values
  PV <- as.numeric(condition$pop_tab$pv)[condition$est_tab$free]
  par <- paste0(
    condition$pop_tab$lhs[condition$est_tab$free],
    condition$pop_tab$op[condition$est_tab$free],
    condition$pop_tab$rhs[condition$est_tab$free]
  )
  n_pars <- length(PV)

  # Memory allocation
  coefs <- SEs <- cvr_r <- acc_r <- matrix(NA, nrow = n_pars, ncol = reps)
  sigs <- cover <- matrix(FALSE, nrow = n_pars, ncol = reps)
  errors <- not_converged <- inadmissible <- rep(FALSE, times = reps)

  # Create folder for saving simulated data to
  if (!is.null(save_path)) {
    save_path_aux <- file.path(save_path, paste0("data_N", condition$sample_size, "_T", condition$time_points, "_RIvar", condition$RI_var))
    dir.create(save_path_aux)
  }

  # Start simulation
  for (r in 1:reps) {

    # Simulate data
    dat <- lavaan::simulateData(
      model = condition$pop_synt,
      sample.nobs = condition$sample_size,
      skewness = condition$skewness,
      kurtosis = condition$kurtosis
    )

    # (optional) Save data
    if (!is.null(save_path)) {
      utils::write.table(dat,
        file = file.path(save_path_aux, paste0("df", r, ".dat")),
        sep = "\t", col.names = FALSE, row.names = FALSE, na = "-999"
      )
    }

    # Fit RI-CLPM
    fit <- tryCatch(
      {
        suppressWarnings(
          lavaan(
            model = condition$est_synt,
            data = dat,
            estimator = estimator,
            bounds = bounds,
            warn = FALSE,
            check.start = FALSE,
            check.lv.names = FALSE,
            test = "none",
            h1 = FALSE,
            baseline = FALSE,
            check.post = FALSE
          )
        )
      },
      error = function(e) {
        errors[r] <<- TRUE
        return(NULL)
      }
    )

    if (is.null(fit) || !lavInspect(fit, what = "converged")) {
      not_converged[r] <- TRUE
      next # Don't get estimates
    }
    if (!suppressWarnings(lavInspect(fit, what = "post.check"))) {
      inadmissible[r] <- TRUE
      if (!bounds) {
        next # Don't get estimates
      }
    }

    # Get estimates
    tryCatch(
      {
        coefs[, r] <- parameterEstimates(fit, remove.nonfree = TRUE)$est[1:n_pars]
        SEs[, r] <- parameterEstimates(fit, remove.nonfree = TRUE)$se[1:n_pars]
        sigs[, r] <- parameterEstimates(fit, remove.nonfree = TRUE)$pvalue[1:n_pars] < condition$alpha
        cvr_r[, r] <- suppressWarnings({
          parameterEstimates(fit, remove.nonfree = TRUE, level = (1 - condition$alpha))$ci.lower[1:n_pars] < PV &
            parameterEstimates(fit, remove.nonfree = TRUE, level = (1 - condition$alpha))$ci.upper[1:n_pars] > PV
        })
        acc_r[, r] <- parameterEstimates(fit, remove.nonfree = TRUE, level = (1 - condition$alpha))$ci.upper[1:n_pars] -
          parameterEstimates(fit, remove.nonfree = TRUE, level = (1 - condition$alpha))$ci.lower[1:n_pars]
      },
      error = function(e) {
        errors[r] <<- TRUE
        not_converged[r] <<- TRUE
      }
    )
  }

  # Create and save repList
  if (!is.null(save_path)) {
    df_list <- paste0("df", 1:reps, ".dat")
    utils::write.table(df_list,
      file = file.path(save_path_aux, "dfList.dat"),
      sep = "\n", col.names = FALSE, row.names = FALSE, quote = FALSE
    )
  }

  # Compute simulation results
  converged_reps <- reps - sum(not_converged)
  min <- suppressWarnings({
    apply(coefs, 1, min, na.rm = TRUE)
  })
  avg <- rowMeans(coefs, na.rm = TRUE)
  stdDev <- apply(coefs, 1, stats::sd, na.rm = TRUE)
  SEAvg <- rowMeans(SEs, na.rm = TRUE)
  MSE <- rowMeans((coefs - PV)^2, na.rm = TRUE)
  acc <- rowSums(acc_r, na.rm = TRUE) / converged_reps
  pwr <- rowSums(sigs, na.rm = TRUE) / converged_reps
  cvr <- rowSums(cvr_r, na.rm = TRUE) / converged_reps

  # Quantify uncertainty around Pow using bootstrapping
  Pow_uncertainty <- t(
    apply(sigs, 1, quantify_uncertainty,
      bootstrap_reps = bootstrap_reps,
      converged_reps = converged_reps
    )
  )

  # Structure results
  condition$estimates <- data.frame(
    Par = par,
    PV = PV,
    Avg = avg,
    Min = min,
    stdDev, SEAvg, MSE,
    Acc = acc,
    Cov = cvr,
    Pow = pwr
  )
  condition$uncertainty <- data.frame(par, Pow2.5 = Pow_uncertainty[, "2.5%"], Pow97.5 = Pow_uncertainty[, "97.5%"])
  condition$errors <- errors
  condition$not_converged <- not_converged
  condition$inadmissible <- inadmissible

  # Signal progress bar
  progress()

  return(condition)
}
