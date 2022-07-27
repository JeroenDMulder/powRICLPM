#' @title
#' Power analysis for the RI-CLPM
#'
#' @description
#' \code{powRICLPM()} performs a Monte Carlo power analysis for the random intercept cross-lagged panel model (RI-CLPM). It computes performance metrics (e.g., bias, mean square error, coverage, power, etc) for all RI-CLPM parameters, and can perform power analysis across multiple experimental conditions simultaneously. Conditions are defined in terms of sample size, number of time points, and proportion of between-unit variance (ICC). See "Details" for information on a) the data simulation and model estimation, b) internal naming conventions of parameters, c) the option to include measurement errors and various constraints, and d) parallel execution capabilities for speeding up the analysis.
#'
#' @param target_power A numeric value between 0 and 1, denoting the targeted power level.
#' @param search_lower A positive integer, denoting the lower bound of a range of sample sizes.
#' @param search_upper A positive integer, denoting the upper bound of a range of sample sizes.
#' @param search_step A positive integer, denoting an increment in sample size.
#' @param sample_size (optional) An integer (vector), indicating specific sample sizes at which to evaluate power, rather than specifying a range using the \code{search_} arguments.
#' @param time_points An integer (vector) with elements at least larger than 3, indicating number of time points.
#' @param ICC A \code{double} (vector), denoting the proportion of variance at the between-unit level.
#' @param RI_cor A \code{double}, denoting the correlation between random intercepts.
#' @param Phi A matrix, with standardized autoregressive effects (on the diagonal) and cross-lagged effects (off-diagonal) in the population. Columns represent predictors and rows represent outcomes.
#' @param wSigma A correlation matrix for the within-unit components.
#' @param reliability (optional) A numeric value between 0 and 1, denoting the reliability of the variables.
#' @param skewness (optional) A numeric value, denoting the skewness values for the observed variables (see \code{\link[lavaan]{simulateData}}).
#' @param kurtosis (optional) A numeric value, denoting the excess kurtosis values (i.e., compared to the kurtosis of a normal distribution) for the observed variables (see \code{\link[lavaan]{simulateData}}).
#' @param est_ME (optional) A logical, denoting if measurement error variance should be estimated in the RI-CLPM (see "Details").
#' @param alpha (optional) A \code{double}, denoting the significance criterion.
#' @param reps A positive \code{integer}, denoting the number of Monte Carlo replications to be used during simulations.
#' @param bootstrap_reps (optional) A positive \code{integer}, denoting the number of bootstrap samples to use for quantifying the uncertainty around the power analysis results.
#' @param seed An `integer` of length 1. If multiple cores are used, a seed of length 1 will be used to generate a full L'Ecuyer-CMRG seed for all cores (see \code{\link[furrr]{furrr_options}}).
#' @param constraints (optional) A character string, specifying the type of constraints that should be imposed on the estimation model (see "Details").
#' @param bounds (optional) A logical, denoting if bounded estimation should be used for the latent variable variances in the model (see "Details").
#' @param estimator (options) A character, denoting the estimator to be used (see "Details").
#'
#' @details
#' A rationale for the power analysis strategy implemented in this package can be found in Mulder (2022).
#'
#' \subsection{Data generation and model estimation}{Data are generated using \code{\link[lavaan]{simulateData}} from the \pkg{lavaan} package. Based on \code{Phi} and \code{wSigma}, the residual variances and covariances for the within-components at wave 2 and later are computed, such that the within-components themselves have a variance of 1. This implies that the lagged effects in \code{Phi} can be interpreted as standardized effects.
#'
#' Data are analyzed using \code{\link[lavaan]{lavaan}} from the \pkg{lavaan} package. The default estimator is maximum likelihood. Other maximum likelihood based estimators implemented in \href{https://lavaan.ugent.be/tutorial/est.html}{\pkg{lavaan}} can be specified as well. When skewed or kurtosed data are generated (using the \code{skewness} and \code{kurtosis} arguments), the estimator defaults to robust maximum likelihood (i.e., \code{estimator = "MLR"}). The population parameter values are used as starting values. Parameter estimates from unconverged model solutions are discarded from the results. However, parameter estimates from solutions with inadmissible parameter estimates (e.g., a negative random intercept variance), are included in the results. The results include the minimum estimates for all parameters across replications. This can be used to diagnose which parameter is the cause of the inadmissible solution.}
#'
#' \subsection{Naming conventions for observed and latent variables}{The observed variables in the RI-CLPM are given default names, namely capital letters in alphabetical order, with numbers denoting the measurement occasion. For example, for a bivariate RICLPM with 3 time points, we observe \code{A1}, \code{A2}, \code{A3}, \code{B1}, \code{B2}, and \code{B3}. Their within-components are denoted by \code{wA1}, \code{wA2}, ..., \code{wB3}, respectively. The between-components have \code{RI_} prepended to the variable name, resulting in \code{RI_A} and \code{RI_B}.
#'
#' Parameters are denoted using \pkg{lavaan} model syntax (see \href{https://lavaan.ugent.be/tutorial/syntax1.html}{the \pkg{lavaan} website}). For example, the random intercept variances are denoted by \code{RI_A~~RI_A} and \code{RI_B~~RI_B}, the cross-lagged effects at the first wave as \code{wB2~wA1} and \code{wA2~wB1}, and the autoregressive effects as \code{wA2~wA1} and \code{wB2~wB1}. Use \code{give(object, "names")} to extract parameter names from the \code{powRICLPM} object.}
#'
#' \subsection{Imposing constraints}{The following constraints can be imposed on the estimation model using the \code{constraints = "..."} argument:
#'
#' \itemize{
#'   \item \code{lagged}: Time-invariant autoregressive and cross-lagged effects.
#'   \item \code{residuals}: Time-invariant residual variances.
#'   \item \code{within}: Time-invariant lagged effects and residual variances.
#'   \item \code{stationarity}: Constraints such that at the within-unit level a stationary process is estimated. This included time-invariant lagged effects, and constraints on the residual variances.
#'   \item \code{ME}: Time-invariant measurement error variances. Only possible when \code{est_ME = TRUE}.
#' }
#' }
#'
#' \subsection{Bounded estimation}{Bounded estimation is useful to avoid nonconvergence in small samples (< 100). Here, automatic wide bounds are used as advised by \doi{https://doi.org/10.1080/10705511.2021.1982716}{De Jonckere and Rosseel (2022}; see \code{optim.bounds} in \code{\link[lavaan]{lavOptions}}). This option can only be used when no constraints are imposed on the estimation model.}
#'
#' \subsection{Parallel processing using \pkg{furrr}}{To speed up the analysis, power analysis for multiple experimental conditions can be executed in parallel. This has been implemented using \pkg{furrr}. By default the analysis is executed sequentially (i.e., single-core). Parallel execution (i.e., multicore) can be setup using \code{\link[future]{plan}}, for example \code{plan(multisession, workers = 4)}. For more information and options, see \url{https://furrr.futureverse.org}.}
#'
#' \subsection{Progress bar using \pkg{progressr}}{A progress bar displaying the status of the power analysis has been implemented using \pkg{progressr}. By default, a simple progress bar will be shown. For more information on how to control this progress bar and several other notification options (e.g., auditory notifications), see \url{https://progressr.futureverse.org}.}
#'
#' @return
#' A list containing a \code{conditions} and \code{session} element. \code{condition} itself is a list of experimental conditions, where each element is again a list containing the input and output of the power analysis for that particular experimental condition. \code{session} is a list containing information common to all experimental conditions.
#'
#' @author Jeroen D. Mulder \email{j.d.mulder@@uu.nl}
#' @export
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{powRICLPM_Mplus}}: Create Mplus model syntax for RI-CLPM power analysis.
#'   \item \code{\link{summary}}: Summarize the setup of \code{powRICLPM} object.
#'   \item \code{\link{give}}: Extract information from \code{powRICLPM} objects.
#'   \item \code{\link{plot}}: Visualize results \code{powRICLPM} object for a specific parameter.
#' }
#'
#' @examples
#' # Example - Simulate power across range of sample sizes
#' # Define population parameters for lagged effects and within-component correlations
#' Phi <- matrix(c(.4, .1, .2, .3), ncol = 2, byrow = TRUE)
#' wSigma <- matrix(c(1, .3, .3, 1), ncol = 2, byrow = TRUE)
#'
#' # Setup parallel computing (multicore, speeding up the analysis)
#' \dontrun{
#' library(furrr)
#' library(progressr)
#' plan(multisession, workers = 4)
#'
#' # Run preliminary analysis (with limited "reps")
#' with_progress({
#'   out1 <- powRICLPM(
#'     target_power = 0.8,
#'     search_lower = 500,
#'     search_upper = 1000,
#'     search_step = 100,
#'     time_points = c(3, 4),
#'     ICC = c(0.4, 0.5, 0.6),
#'     RI_cor = 0.3,
#'     Phi = Phi,
#'     wSigma = wSigma,
#'     reps = 50,
#'     seed = 1234
#'   )
#' })
#' }
powRICLPM <- function(target_power,
                      search_lower = NULL,
                      search_upper = NULL,
                      search_step = 20,
                      sample_size = NULL,
                      time_points,
                      ICC,
                      RI_cor,
                      Phi,
                      wSigma,
                      reliability = 1,
                      skewness = 0,
                      kurtosis = 0,
                      est_ME = FALSE,
                      alpha = 0.05,
                      reps = 20,
                      bootstrap_reps = 1000,
                      seed = NA,
                      constraints = "none",
                      bounds = FALSE,
                      estimator = NA) {

  # Check arguments I
  message(rlang::format_error_bullets(c(
    i = "Checking arguments..."
  )))
  target_power <- check_target(target_power)
  time_points <- check_T(time_points, est_ME)
  ICC <- check_ICC(ICC)
  RI_cor <- check_RIcor(RI_cor)
  wSigma <- check_wSigma(wSigma)
  Phi <- check_Phi(Phi)
  reliability <- check_reliability(reliability)
  skewness <- check_skewness(skewness)
  kurtosis <- check_kurtosis(kurtosis)
  alpha <- check_alpha(alpha)
  est_ME <- check_est_ME(est_ME)
  reps <- check_reps(reps)
  bootstrap_reps <- check_reps(bootstrap_reps)
  seed <- check_seed(seed)
  constraints <- check_constraints(constraints)
  bounds <- check_bounds(bounds, constraints)
  estimator <- check_estimator(estimator, skewness, kurtosis)

  # Compute population parameter values for data generation
  Psi <- compute_Psi(Phi, wSigma)

  # Get candidate sample sizes
  if (is.null(sample_size)) {
    sample_size <- seq(search_lower, search_upper, search_step)
  }

  # Check arguments II
  sample_size <- check_N(sample_size, time_points, constraints, est_ME)
  Psi <- check_Psi(Psi)

  # Update
  message(rlang::format_error_bullets(c(
    i = "Creating lavaan syntax..."
  )))

  # Setup analysis
  object <- setup(
    target_power = target_power,
    sample_size = sample_size,
    time_points = time_points,
    ICC = ICC,
    RI_cor = RI_cor,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    reliability = reliability,
    skewness = skewness,
    kurtosis = kurtosis,
    est_ME = est_ME,
    alpha = alpha,
    reps = reps,
    bootstrap_reps = bootstrap_reps,
    seed = seed,
    constraints = constraints,
    bounds = bounds,
    estimator = estimator
  )

  # Update
  message(rlang::format_error_bullets(c(
    i = paste0("Starting Monte Carlo replications...")
  )))

  # Prepare progress bar
  p <- progressr::progressor(along = object$conditions)

  # Run Monte Carlo simulation for each condition
  object$conditions <- furrr::future_map(object$conditions,
    run_condition,
    progress = p,
    bounds = bounds,
    estimator = estimator,
    reps = reps,
    bootstrap_reps = bootstrap_reps,
    constraints = constraints,
    .options = furrr::furrr_options(
      seed = seed,
      scheduling = 2L # Dynamic scheduling
    )
  )

  # Assign "powRICLPM" class to object
  class(object) <- c("powRICLPM", class(object))

  return(object)
}


#' @title
#' Create Mplus syntax for RI-CLPM power analysis
#'
#' @description
#' \code{powRICLPM_Mplus()} creates and saves syntax for performing a Monte Carlo power analysis for the random intercept cross-lagged panel model (RI-CLPM) in Mplus. It can create Mplus model syntax across multiple experimental conditions simultaneously. Conditions are defined in terms of sample size, number of time points, and proportion of between-unit variance (ICC). See "Details" for information on a) the naming conventions of parameters, and b) the various constraints that can be imposed on the model.
#'
#' @inheritParams powRICLPM
#' @param save_path Character string denoting a path leading to the folder where to save the Mplus syntax files to.
#'
#' @details
#' \subsection{Syntax generation}{Mplus model syntax is created in multiple steps: First, the \code{MODEL POPULATION} command syntax is created in which parameters are constrained to population values. Second, the \code{MODEL} command syntax is created for model estimation. Optionally, syntax for constraints on the estimation model, in the \code{MODEL CONSTRAINTS} command, is created next. Ultimately, the parameter tables are combined to form character vectors containing the Mplus syntax to be exported (see "Details" of \code{\link{powRICLPM}} for more information on the \code{constraints} options).}
#'
#' \subsection{Naming conventions}{Details on the naming conventions can be found in the "Details" section of \code{\link{powRICLPM}}.}
#'
#' @return NULL
#'
#' @examples
#' # Example - Create Mplus syntax to simulate power across range of sample sizes
#' # Define population parameters for lagged effects and within-component correlations
#' Phi <- matrix(c(.4, .1, .2, .3), ncol = 2, byrow = TRUE)
#' wSigma <- matrix(c(1, .3, .3, 1), ncol = 2, byrow = TRUE)
#'
#' # Create and save Mplus model syntax
#' \dontrun{
#' powRICLPM_Mplus(
#'   search_lower = 500,
#'   search_upper = 1000,
#'   search_step = 100,
#'   time_points = c(3, 4),
#'   ICC = c(0.4, 0.5, 0.6),
#'   RI_cor = 0.3,
#'   Phi = Phi,
#'   wSigma = wSigma,
#'   reps = 10000,
#'   seed = 1234
#' )
#' }
#' @export
powRICLPM_Mplus <- function(search_lower = NULL,
                            search_upper = NULL,
                            search_step = 20,
                            sample_size = NULL,
                            time_points,
                            ICC,
                            RI_cor,
                            Phi,
                            wSigma,
                            reps = 1000,
                            seed = NA,
                            save_path = getwd(),
                            constraints = "none") {
  # Check arguments I
  time_points <- check_T(time_points)
  ICC <- check_ICC(ICC)
  RI_cor <- check_RIcor(RI_cor)
  wSigma <- check_wSigma(wSigma)
  Phi <- check_Phi(Phi)
  reps <- check_reps(reps)
  seed <- check_seed(seed)
  save_path <- check_save(save_path)
  constraints <- check_constraints(constraints)

  # Compute population parameter values for data generation
  Psi <- compute_Psi(Phi, wSigma)

  # Get candidate sample sizes
  if (is.null(sample_size)) {
    sample_size <- seq(search_lower, search_upper, search_step)
  }

  # Check arguments II
  sample_size <- check_N(sample_size, time_points)
  Psi <- check_Psi(Psi)

  # Setup of Mplus model syntax generation
  conditions <- setup_Mplus(
    sample_size = sample_size,
    time_points = time_points,
    ICC = ICC,
    RI_cor = RI_cor,
    Phi = Phi,
    wSigma = wSigma,
    Psi = Psi,
    reps = reps,
    seed = seed,
    save_path = save_path,
    constraints = constraints
  )

  # Create Mplus model syntax for each condition
  purrr::walk(conditions, create_Mplus)

  # Inform user
  message(rlang::format_error_bullets(c(
    i = "Mplus model syntax created:")))
  cat("\n\n  Directory:", save_path)
  cat("\n  Sample size(s):", sample_size)
  cat("\n  Number of time points:", time_points)
  cat("\n  Proportion(s) random intercept variance:", ICC)

  # Return
  invisible()
}
