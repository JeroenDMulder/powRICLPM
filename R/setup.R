#' Set Up \code{powRICLPM} Analysis
#'
#' \code{create_conditions()} restructures the arguments of \code{powRICLPM()} in a list, such that it can be used by \code{run_condition_MonteCarlo()}, performing a Monte Carlo power analysis for each experimental condition.
#'
#' @inheritParams powRICLPM
#'
#' @noRd
create_conditions <- function(
  target_power,
  sample_size,
  time_points,
  ICC,
  RI_cor,
  Phi,
  within_cor,
  Psi,
  reliability,
  skewness,
  kurtosis,
  estimate_ME,
  significance_criterion,
  reps,
  bootstrap_reps,
  seed,
  constraints,
  bounds,
  estimator,
  save_path,
  software
) {

  # Create data.frame with rows as experimental conditions
  conditions <- expand.grid(
    sample_size = sample_size,
    time_points = time_points,
    ICC = ICC,
    RI_cor = RI_cor,
    within_cor = within_cor,
    reliability = reliability,
    constraints = constraints,
    skewness = skewness,
    kurtosis = kurtosis,
    significance_criterion = significance_criterion,
    estimate_ME = estimate_ME
  )

  # Add matrix input to conditions
  conditions$Phi <- replicate(nrow(conditions), Phi, simplify = FALSE)
  conditions$Psi <- replicate(nrow(conditions), Psi, simplify = FALSE)

  # Compute and add additional parameters per condition
  conditions$condition_id <- 1:nrow(conditions)
  conditions$RI_var <- sapply(conditions$ICC, compute_RI_var)
  conditions$RI_cov <- mapply(compute_RI_cov, conditions$RI_cor, conditions$RI_var)
  conditions$ME_var <- mapply(compute_ME_var, conditions$RI_var, conditions$reliability)

  # Create list of conditions
  conditions <- split(conditions, seq(nrow(conditions)))

  # Create syntax per condition
  if (software == "lavaan") {
    conditions <- lapply(conditions, create_lavaan)
  } else if (software == "Mplus") {
    conditions <- lapply(conditions, create_Mplus, reps = reps , seed = seed)
    lapply(conditions, save_condition_Mplus, save_path)
  }

  return(conditions)

}


