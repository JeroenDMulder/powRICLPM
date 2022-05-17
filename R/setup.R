#' Prepare Monte Carlo power analysis
#'
#' `setup()` restructures the arguments of `powRICLPM()` in a list, such that it can be used to call `run()` on to perform the Monte Carlo power analysis. Additionally, it computes objects needed to perform the Monte Carlo power analysis and includes them in the list too.
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
                  skewness,
                  kurtosis,
                  alpha,
                  reps,
                  seed,
                  save_path,
                  parameter,
                  constraints,
                  bounds) {

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

  # Create lavaan syntax for generating data
  pop_synt <- purrr::pmap(
    list(grid$time_points, grid$RI_var, grid$RI_cov),
    function(time_points, RI_var, RI_cov) {
      create_lavaan(
        time_points = time_points,
        RI_var = RI_var,
        RI_cov = RI_cov,
        Phi = Phi,
        wSigma = wSigma,
        Psi = Psi,
        syntax = TRUE,
        estimation = FALSE,
        constraints = constraints
      )
    }
  )

  # Create lavaan parameter table for population model
  pop_tab <- purrr::pmap(
    list(grid$time_points, grid$RI_var, grid$RI_cov),
    function(time_points, RI_var, RI_cov) {
      create_lavaan(
        time_points = time_points,
        RI_var = RI_var,
        RI_cov = RI_cov,
        Phi = Phi,
        wSigma = wSigma,
        Psi = Psi,
        estimation = FALSE,
        constraints = constraints
      )
    }
  )

  # Create lavaan syntax for estimating the model
  est_synt <- purrr::pmap(
    list(grid$time_points, grid$RI_var, grid$RI_cov),
    function(time_points, RI_var, RI_cov) {
      create_lavaan(
        time_points = time_points,
        RI_var = RI_var,
        RI_cov = RI_cov,
        Phi = Phi,
        wSigma = wSigma,
        Psi = Psi,
        estimation = TRUE,
        syntax = TRUE,
        constraints = constraints
      )
    }
  )


  # Create lavaan parameter table for estimating model
  est_tab <- purrr::pmap(
    list(grid$time_points, grid$RI_var, grid$RI_cov),
    function(time_points, RI_var, RI_cov) {
      create_lavaan(
        time_points = time_points,
        RI_var = RI_var,
        RI_cov = RI_cov,
        Phi = Phi,
        wSigma = wSigma,
        Psi = Psi,
        estimation = TRUE,
        constraints = constraints
      )
    }
  )

  # Create list with each element containing info for a single condition
  conditions <- purrr::pmap(
    list(
      grid$sample_size,
      grid$time_points,
      grid$ICC,
      grid$RI_var,
      grid$RI_cov,
      reps,
      pop_synt,
      pop_tab,
      est_synt,
      est_tab,
      skewness,
      kurtosis,
      alpha,
      save_path
    ),
    list
  )

  # Name elements in powRICLPM condition list
  conditions <- purrr::map(conditions, function(x) {
    names(x) <- c(
      "sample_size", "time_points", "ICC", "RI_var", "RI_cov",
      "reps", "pop_synt", "pop_tab", "est_synt", "est_tab",
      "skewness", "kurtosis", "alpha", "save_path"
    )
    return(x)
  })

  # Create powRICLPM object, combining conditions and general session info
  object <- list(
    conditions = conditions,
    session = list(
      Psi = Psi,
      reps = reps,
      target_power = target_power,
      save_path = save_path,
      parameter = parameter,
      constraints = constraints,
      bounds = bounds
    )
  )
  return(object)
}

#' Prepare Mplus power analysis
#'
#' `setup_Mplus()` restructures the arguments of `powRICLPM_Mplus()` in a list, such that we can generate Mplus syntax for the Monte Carlo power analysis. Additionally, it computes objects needed to perform the Monte Carlo power analysis and includes them in the list too.
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
