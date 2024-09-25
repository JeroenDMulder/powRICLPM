#' @title
#' Run Monte Carlo Simulation For Single Condition
#'
#' @description
#' \code{run_condition_monteCarlo()} runs a Monte Carlo simulation for a single experimental condition. It generates data based on the \code{pop_synt} element in \code{condition}, and estimates an RI-CLPM or STARTS using its \code{est_synt} element. Data generation and model estimation are done using \pkg{lavaan}.
#'
#' @inheritParams powRICLPM
#' @param condition A list with information for running a single simulation condition.
#' @param p A \pkg{progressr} object.
#'
#' @return The `condition` list extended with an `estimates`, `MCSE`, and `estimation_problems` data frame. `estimates` contains numeric performance measures per parameter of the estimation model. `MCSE` contains Monte Carlo standard errors for the performance measures. `estimation_problems` contains the number of replications that were completed, converged, resulted in an error or inadmissible results.
#'
#' @importFrom lavaan simulateData lavInspect parameterEstimates lavaan
#' @noRd
run_condition_monteCarlo <- function(
  condition,
  p,
  bounds,
  estimator,
  reps,
  save_path
) {

  # Generate data
  datasets <- lapply(1:reps, function(i) {
    lavaan::simulateData(
      model = condition$pop_synt,
      sample.nobs = condition$sample_size,
      skewness = condition$skewness,
      kurtosis = condition$kurtosis
    )
  })

  # (optional) Export generated data
  if (!is.null(save_path)) {
    # Create export folder
    save_path_aux <- file.path(save_path, paste0("condition", condition["condition_id"]))
    dir.create(save_path_aux)

    # Export data
    lapply(1:reps, function(i) {
      utils::write.table(
        datasets[[i]],
        file = file.path(save_path_aux, paste0("df", i, ".dat")),
        sep = "\t",
        col.names = FALSE,
        row.names = FALSE,
        na = "-999"
      )
    })

    # Create list of exported .dat-files
    utils::write.table(
      paste0("df", 1:reps, ".dat"),
      file = file.path(save_path_aux, "dfList.dat"),
      sep = "\n",
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
    )
  }

  # Fit initial lavaan model
  fit <- suppressWarnings(
    lavaan::lavaan(
      model = condition$est_synt,
      data = datasets[[1]],
      estimator = estimator,
      bounds = bounds,
      warn = FALSE, # Suppress lavaan-specific warnings
      check.start = FALSE, # Don't check consistency starting values
      check.lv.names = FALSE, # Don't check if latent variable are also observed
      h1 = FALSE, # Don't compute unrestricted model
      baseline = FALSE, # Don't compute baseline model
      check.post = TRUE, # Check admissibility of results
      store.vcov = FALSE, # Don't store variance-covariance matrix of model parameters
      fixed.x = FALSE # Treat exogenous variable as endogenous
    )
  )

  # Extract slot options
  lav_model <- fit@Model
  lav_options <- fit@Options
  lav_options$optim.attempts <- 1L
  lav_parTable <- fit@ParTable

  # Fit model to datasets
  fits <- lapply(datasets, function(x, p) {
      tryCatch(
        {
          out <- suppressWarnings(
            lavaan::lavaan(
              model = condition$est_synt,
              data = x,
              warn = FALSE,
              slotOptions = lav_options,
              slotParTable = lav_parTable,
              slotModel = lav_model
            )
          )

          # Signal progress bar
          p()

          return(out)
        },
        error = function(e, p) {
          # Signal progress bar
          p()
          return(NULL)
        }
      )
    },
    p = p
  )

  # Remove empty elements
  error <- sum(unlist(lapply(fits, is.null)))
  fits <- Filter(Negate(is.null), fits)

  # Count estimation issues
  converged <- lapply(fits, function(fit) lavaan::lavInspect(fit, what = "converged"))
  admissible <- lapply(fits, function(fit) lavaan::lavInspect(fit, what = "post.check"))

  # Remove replications with estimation issues
  if (bounds) {
    fits <- mapply(function(x, c, a) {
      if (isTRUE(c)) {
        return(x)
      } else {
        return(NULL)
      }
    }, fits, converged, admissible, SIMPLIFY = FALSE)
  } else {
    fits <- mapply(function(x, c, a) {
      if (isTRUE(c) && isTRUE(a)) {
        return(x)
      } else {
        return(NULL)
      }
    }, fits, converged, admissible, SIMPLIFY = FALSE)
  }

  fits <- Filter(Negate(is.null), fits)
  reps_completed <- length(fits)

  # Save estimation problems
  condition$estimation_information <- data.frame(
    n_completed = reps_completed,
    n_error = error,
    n_nonconvergence = reps - sum(unlist(converged)),
    n_inadmissible = reps - sum(unlist(admissible))
  )

  # Extract population values
  index_free <- lav_parTable$free != 0
  population_values <- lav_parTable$start[index_free]

  # Initialize data frames for simulation results
  condition$estimates <- data.frame(
    parameter = paste0(lav_parTable$lhs, lav_parTable$op, lav_parTable$rhs)[index_free],
    population_value = population_values,
    average = NA,
    minimum = NA,
    EmpSE = NA,
    SEAvg = NA,
    MSE = NA,
    accuracy = NA,
    coverage = NA,
    power = NA
  )

  condition$MCSEs <- data.frame(
    MCSE_bias = NA,
    MCSE_MSE = NA,
    MCSE_coverage = NA,
    MCSE_SEAvg = NA,
    MCSE_EmpSE = NA,
    MCSE_power = NA
  )

  if (length(fits) > 0) {

    # Extract model results
    estimates <- lapply(fits, function(fit) lavaan::parameterEstimates(
      object = fit,
      remove.nonfree = TRUE,
      remove.def = TRUE,
      level = (1 - condition$significance_criterion)
    ))

    point_estimates <- do.call(cbind, lapply(estimates, function(est) est$est))
    standard_errors <- do.call(cbind, lapply(estimates, function(est) est$se))
    variance_errors <- do.call(cbind, lapply(estimates, function(est) est$se^2)) # SEs squared
    CIs_lower <- do.call(cbind, lapply(estimates, function(est) est$ci.lower))
    CIs_upper <- do.call(cbind, lapply(estimates, function(est) est$ci.upper))
    p_values <- do.call(cbind, lapply(estimates, function(est) est$pvalue))

    # Compute simulation output
    min <- apply(point_estimates, 1, min)
    avg <- rowMeans(point_estimates)
    bias <- rowMeans(point_estimates - population_values)
    EmpSE <- apply(point_estimates, 1, stats::sd)
    SEAvg <- rowMeans(standard_errors)
    VEAvg <- rowMeans(variance_errors)
    MSE <- rowMeans(apply(point_estimates, 2, function(x) {(x - population_values)^2}))
    accuracy <- rowMeans(CIs_upper - CIs_lower)
    power <- rowMeans(p_values < condition$significance_criterion)
    coverage_lower <- apply(CIs_lower, 2, function(x) {x < population_values})
    coverage_upper <- apply(CIs_upper, 2, function(x) {x > population_values})
    coverage <- rowMeans(coverage_lower & coverage_upper)

    # Compute Monte Carlo Standard Error of estimates
    MCSE_bias <- compute_MCSE_bias(point_estimates, avg)
    MCSE_MSE <- compute_MCSE_MSE(point_estimates, population_values, MSE)
    MCSE_coverage <- compute_MCSE_coverage(coverage, reps_completed)
    MCSE_SEAvg <- compute_MCSE_SEAvg(variance_errors, VEAvg, SEAvg, reps_completed)
    MCSE_EmpSE <- compute_MCSE_EmpSE(EmpSE, reps_completed)
    MCSE_power <- compute_MCSE_power(power, reps)

    # Structure estimates, MCSEs, and replication info
    condition$estimates <- data.frame(
      parameter = paste0(lav_parTable$lhs, lav_parTable$op, lav_parTable$rhs)[index_free],
      population_value = population_values,
      average = avg,
      bias = bias,
      minimum = min,
      EmpSE = EmpSE,
      SEAvg = SEAvg,
      MSE = MSE,
      accuracy = accuracy,
      coverage = coverage,
      power = power
    )

    condition$MCSEs <- data.frame(
      MCSE_bias = MCSE_bias,
      MCSE_MSE = MCSE_MSE,
      MCSE_coverage = MCSE_coverage,
      MCSE_SEAvg = MCSE_SEAvg,
      MCSE_EmpSE = MCSE_EmpSE,
      MCSE_power = MCSE_power
    )
  }
  return(condition)
}

