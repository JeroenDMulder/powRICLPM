#' Set up \code{powRICLPM} analysis
#'
#' \code{setup()} restructures the arguments of \code{powRICLPM()} in a list, such that it can be used by \code{run()}, performing a Monte Carlo power analysis for each experimental condition.
#'
#' @inheritParams powRICLPM
#'
#' @noRd
setup <- function(target_power,
                  sample_size,
                  time_points,
                  ICC,
                  RI_cor,
                  Phi,
                  wSigma,
                  Psi,
                  reliability,
                  skewness,
                  kurtosis,
                  estimate_ME,
                  alpha,
                  reps,
                  bootstrap_reps,
                  seed,
                  constraints,
                  bounds,
                  estimator,
                  save_path) {

  # Create experimental conditions
  params <- expand.grid(
    sample_size = sample_size,
    time_points = time_points,
    ICC = ICC,
    RI_cor = RI_cor,
    reliability = reliability
  )
  params$RI_var <- purrr::map_dbl(params$ICC, compute_RI_var)
  params$RI_cov <- purrr::map2_dbl(params$RI_cor, params$RI_var, compute_RI_cov)
  params$ME_var <- purrr::map2_dbl(params$RI_var, params$reliability, compute_ME_var)

  # Create lavaan syntax for generating data
  conds <- lapply(
    X = asplit(params, MARGIN = 1), # List of rows
    FUN = create_lavaan,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    constraints = constraints,
    estimate_ME = estimate_ME,
    skewness = skewness,
    kurtosis = kurtosis,
    alpha = alpha
  )

  # Create powRICLPM object, combining conditions and general session info
  object <- list(
    conditions = conds,
    session = list(
      Psi = Psi,
      reliability = reliability,
      estimate_ME = estimate_ME,
      reps = reps,
      bootstrap_reps = bootstrap_reps,
      target_power = target_power,
      constraints = constraints,
      bounds = bounds,
      estimator = estimator,
      save_path = save_path,
      version = utils::packageVersion("powRICLPM")
    )
  )
  rm(params, conds)
  return(object)
}

#' Set up \code{powRICLPM} analysis for Mplus
#'
#' \code{setup_Mplus()} restructures the arguments of \code{powRICLPM_Mplus()} in a list, such that we can generate Mplus syntax for a Monte Carlo power analysis.
#'
#' @inheritParams powRICLPM
#'
#' @noRd
setup_Mplus <- function(sample_size,
                        time_points,
                        ICC,
                        RI_cor,
                        Phi,
                        wSigma,
                        Psi,
                        reps,
                        seed,
                        save_path,
                        constraints) {

  # Create sample_size, time_points, and ICC elements for powRICLPM condition list
  grid <- expand.grid(
    sample_size = sample_size,
    time_points = time_points,
    ICC = ICC,
    RI_cor = RI_cor
  )

  # Compute RI_var and RI_cor based on ICC in each condition
  grid$RI_var <- purrr::map_dbl(grid$ICC, compute_RI_var)
  grid$RI_cov <- purrr::map2_dbl(grid$RI_cor, grid$RI_var, compute_RI_cov)

  # Create list with each element containing info for a single condition
  conditions <- purrr::pmap(
    list(
      sample_size = grid$sample_size,
      time_points = grid$time_points,
      ICC = grid$ICC,
      RI_var = grid$RI_var,
      RI_cov = grid$RI_cov,
      Phi = list(Phi),
      wSigma = list(wSigma),
      Psi = list(Psi),
      reps = reps,
      seed = seed,
      save_path = save_path,
      constraints = constraints
    ),
    list
  )
  return(conditions)
}
