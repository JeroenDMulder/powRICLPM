#' Power analysis for the RI-CLPM (and STARTS model)
#'
#' @description
#' Perform a Monte Carlo power analysis for the random intercept cross-lagged panel model (RI-CLPM). This function computes performance metrics (e.g., bias, mean square error, coverage, power, etc) for all RI-CLPM parameters, and can perform power analyses across multiple experimental conditions simultaneously. Conditions are defined in terms of sample size, number of time points, and proportion of between-unit variance (ICC). See "Details" for information on a) the data simulation, b) model estimation, c) internal naming conventions of parameters, d) the option to include measurement errors (i.e., estimating the Stable Trait Autoregressive Trait State model), e) imposing various constraints over time, and f) parallel execution capabilities for speeding up the analysis.
#'
#' @param target_power A numeric value between 0 and 1, denoting the targeted power level.
#' @param search_lower A positive integer, denoting the lower bound of a range of sample sizes.
#' @param search_upper A positive integer, denoting the upper bound of a range of sample sizes.
#' @param search_step A positive integer, denoting an increment in sample size.
#' @param sample_size (optional) An integer (vector), indicating specific sample sizes at which to evaluate power, rather than specifying a range using the \code{search_*} arguments.
#' @param time_points An integer (vector) with elements at least larger than 3, indicating number of time points.
#' @param ICC A \code{double} (vector) with elements between 0 and 1, denoting the proportion of (true score) variance at the between-unit level. When measurement error is included in the data generating model, ICC is computed as the variance of the random intercept factor divided by the true score variance (i.e., controlled for measurement error).
#' @param RI_cor A \code{double} between 0 and 1, denoting the correlation between random intercepts.
#' @param Phi A matrix, with standardized autoregressive effects (on the diagonal) and cross-lagged effects (off-diagonal) in the population. Columns represent predictors and rows represent outcomes.
#' @param within_cor A \code{double} between 0 and 1, denoting the correlation between the within-unit components.
#' @param reliability (optional) A numeric value between 0 and 1, denoting the reliability of the variables.
#' @param skewness (optional) A numeric value, denoting the skewness values for the observed variables (see \code{\link[lavaan]{simulateData}}).
#' @param kurtosis (optional) A numeric value, denoting the excess kurtosis values (i.e., compared to the kurtosis of a normal distribution) for the observed variables (see \code{\link[lavaan]{simulateData}}).
#' @param estimate_ME (optional) A logical, denoting if measurement error variance should be estimated in the RI-CLPM (see "Details").
#' @param alpha (optional) A \code{double}, denoting the significance criterion.
#' @param reps A positive \code{integer}, denoting the number of Monte Carlo replications to be used during simulations.
#' @param bootstrap_reps (optional) A positive \code{integer}, denoting the number of bootstrap samples to use for quantifying the uncertainty (i.e., 95\% bootstrap confidence interval) around the power analysis results.
#' @param seed An \code{integer} of length 1. If multiple cores are used, a seed of length 1 will be used to generate a full L'Ecuyer-CMRG seed for all cores (see \code{\link[furrr]{furrr_options}}).
#' @param constraints (optional) A character string, specifying the type of constraints that should be imposed on the estimation model (see "Details").
#' @param bounds (optional) A logical, denoting if bounded estimation should be used for the latent variable variances in the model (see "Details").
#' @param estimator (options) A character, denoting the estimator to be used (default: \code{ML}, see "Details").
#' @param save_path A character string naming the directory to save (data) files to (used for validation purposes of this package). Variables are saved in alphabetical and numerical order.
#'
#' @details
#' A rationale for the power analysis strategy implemented in this package can be found in Mulder (2022).
#'
#' \subsection{Data generation}{Data are generated using \code{\link[lavaan]{simulateData}} from the \pkg{lavaan} package. Based on \code{Phi} and \code{within_cor}, the residual variances and covariances for the within-components at wave 2 and later are computed, such that the within-components themselves have a variance of 1. This implies that the lagged effects in \code{Phi} can be interpreted as standardized effects.}
#'
#' \subsection{Model estimation}{Data are analyzed using \code{\link[lavaan]{lavaan}} from the \pkg{lavaan} package. The default estimator is maximum likelihood (\code{ML}). Other maximum likelihood based estimators implemented in \href{https://lavaan.ugent.be/tutorial/est.html}{\pkg{lavaan}} can be specified as well. When skewed or kurtosed data are generated (using the \code{skewness} and \code{kurtosis} arguments), the estimator defaults to robust maximum likelihood \code{MLR}. The population parameter values are used as starting values.
#'
#' Parameter estimates from non-converged model solutions are discarded from the results. When \code{bounds = FALSE}, inadmissible parameter estimates from converged solutions (e.g., a negative random intercept variance) are discarded. When \code{bounds = TRUE}, inadmissible parameter estimates are retained following advice by De Jonckere and Rosseel (2022). The results include the minimum estimates for all parameters across replications to diagnose which parameter(s) is (are) the cause of the inadmissible solution.}
#'
#' \subsection{Naming conventions for observed and latent variables}{The observed variables in the RI-CLPM are given default names, namely capital letters in alphabetical order, with numbers denoting the measurement occasion. For example, for a bivariate RICLPM with 3 time points, we observe \code{A1}, \code{A2}, \code{A3}, \code{B1}, \code{B2}, and \code{B3}. Their within-components are denoted by \code{wA1}, \code{wA2}, ..., \code{wB3}, respectively. The between-components have \code{RI_} prepended to the variable name, resulting in \code{RI_A} and \code{RI_B}.
#'
#' Parameters are denoted using \pkg{lavaan} model syntax (see \href{https://lavaan.ugent.be/tutorial/syntax1.html}{the \pkg{lavaan} website}). For example, the random intercept variances are denoted by \code{RI_A~~RI_A} and \code{RI_B~~RI_B}, the cross-lagged effects at the first wave as \code{wB2~wA1} and \code{wA2~wB1}, and the autoregressive effects as \code{wA2~wA1} and \code{wB2~wB1}. Use \code{give(object, "names")} to extract parameter names from the \code{powRICLPM} object.}
#'
#' \subsection{Measurement errors (STARTS model)}{Including measurement error to the RI-CLPM makes the model equivalent to the Stable Trait Autoregressive Trait State (STARTS) model by Kenny and Zautra (2001) without constraints over time. Measurement error can be added to the generated data through the \code{reliability} argument. Setting \code{reliability = 0.8} implies that 80\% is true score variance and 20\% is measurement error variance; \code{ICC} then denotes the proportion of \emph{true score variance} captured by the random intercept factors. Estimating measurement errors (i.e., the STARTS model) is done by setting \code{est_ME = TRUE}.}
#'
#' \subsection{Imposing constraints}{The following constraints can be imposed on the estimation model using the \code{constraints = "..."} argument:
#'
#' \itemize{
#'   \item \code{lagged}: Time-invariant autoregressive and cross-lagged effects.
#'   \item \code{residuals}: Time-invariant residual variances.
#'   \item \code{within}: Time-invariant lagged effects and residual variances.
#'   \item \code{stationarity}: Constraints such that at the within-unit level a stationary process is estimated. This included time-invariant lagged effects, and constraints on the residual variances.
#'   \item \code{ME}: Time-invariant measurement error variances. Only possible when \code{estimate_ME = TRUE}.
#' }
#' }
#'
#' \subsection{Bounded estimation}{Bounded estimation is useful to avoid nonconvergence in small samples (< 100). Here, automatic wide bounds are used as advised by De Jonckere and Rosseel (2022), see \code{optim.bounds} in \code{\link[lavaan]{lavOptions}}. This option can only be used when no constraints are imposed on the estimation model.}
#'
#' \subsection{Parallel processing using \pkg{furrr}}{To speed up the analysis, power analysis for multiple experimental conditions can be executed in parallel. This has been implemented using \pkg{furrr}. By default the analysis is executed sequentially (i.e., single-core). Parallel execution (i.e., multicore) can be setup using \code{\link[future]{plan}}, for example \code{plan(multisession, workers = 4)}. For more information and options, see \url{https://furrr.futureverse.org}.
#'
#' A progress bar displaying the status of the power analysis has been implemented using \pkg{progressr}. By default, a simple progress bar will be shown. For more information on how to control this progress bar and several other notification options (e.g., auditory notifications), see \url{https://progressr.futureverse.org}.}
#'
#' @return
#' A \code{list} containing a \code{conditions} and \code{session} element. \code{condition} itself is a \code{list} of experimental conditions, where each element is again a \code{list} containing the input and output of the power analysis for that particular experimental condition. \code{session} is a \code{list} containing information common to all experimental conditions.
#'
#' @author Jeroen D. Mulder \email{j.d.mulder@@uu.nl}
#'
#' @references
#' De Jonckere, J., & Rosseel, Y. (2022). Using bounded estimation to avoid nonconvergence in small sample structural equation modeling. \emph{Structural Equation Modeling}, \emph{29}(3), 412-427. \doi{10.1080/10705511.2021.1982716}
#'
#' Kenny, D. A., & Zautra, A. (2001). Trait–state models for longitudinal data. \emph{New methods for the analysis of change} (pp. 243–263). American Psychological Association. \doi{10.1037/10409-008}
#'
#' Mulder, J. D. (2022). Power analysis for the random intercept cross-lagged panel model using the \emph{powRICLPM} R-package. \emph{Structural Equation Modeling}. \doi{10.1080/10705511.2022.2122467}
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{powRICLPM_Mplus}}: Create Mplus model syntax for RI-CLPM power analysis.
#'   \item \code{\link{summary.powRICLPM}}: Summarize the setup of \code{powRICLPM} object.
#'   \item \code{\link{give}}: Extract information from \code{powRICLPM} objects.
#'   \item \code{\link{plot.powRICLPM}}: Visualize results \code{powRICLPM} object for a specific parameter.
#' }
#'
#' @examples
#' # Power analysis across range of sample sizes
#' ## Define population parameters for lagged effects
#' Phi <- matrix(c(.4, .1, .2, .3), ncol = 2, byrow = TRUE)
#'
#' ## (optional) Set up parallel computing (i.e., multicore, speeding up the analysis)
#' library(furrr)
#' library(progressr)
#' future::plan(multisession, workers = 4)
#'
#' \dontrun{
#' ## Run analysis ("reps" is extremely small, because this is an example)
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
#'     within_cor = 0.3,
#'     reps = 5,
#'     seed = 1234
#'   )
#' })
#' }
#'
#' \dontshow{
#' ## Shut down parallel workers (done for sake of example, normally not needed)
#' future::plan("sequential")
#' }
#'
#' @importFrom future plan
#' @export
powRICLPM <- function(target_power,
                      search_lower = NULL,
                      search_upper = NULL,
                      search_step = 20,
                      sample_size = NULL,
                      time_points,
                      ICC,
                      RI_cor,
                      Phi,
                      within_cor,
                      reliability = 1,
                      skewness = 0,
                      kurtosis = 0,
                      estimate_ME = FALSE,
                      alpha = 0.05,
                      reps = 20,
                      bootstrap_reps = 1000,
                      seed = NA,
                      constraints = "none",
                      bounds = FALSE,
                      estimator = NA,
                      save_path = NULL) {

  # Check arguments I
  message(rlang::format_error_bullets(c(
    i = "Checking arguments..."
  )))
  target_power <- check_target(target_power)
  time_points <- check_T(time_points, estimate_ME)
  ICC <- check_ICC(ICC)
  RI_cor <- check_RIcor(RI_cor)
  wSigma <- check_within_cor(within_cor)
  Phi <- check_Phi(Phi)
  reliability <- check_reliability(reliability)
  skewness <- check_skewness(skewness)
  kurtosis <- check_kurtosis(kurtosis)
  alpha <- check_alpha(alpha)
  estimate_ME <- check_estimate_ME(estimate_ME)
  reps <- check_reps(reps)
  bootstrap_reps <- check_reps(bootstrap_reps)
  seed <- check_seed(seed)
  constraints <- check_constraints(constraints)
  bounds <- check_bounds(bounds, constraints)
  estimator <- check_estimator(estimator, skewness, kurtosis)
  save_path <- check_save_path(save_path)

  # Compute population parameter values for data generation
  Psi <- compute_Psi(Phi, wSigma)

  # Get candidate sample sizes
  if (is.null(sample_size)) {
    sample_size <- seq(search_lower, search_upper, search_step)
  }

  # Check arguments II
  sample_size <- check_N(sample_size, time_points, constraints, estimate_ME)
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
    estimate_ME = estimate_ME,
    alpha = alpha,
    reps = reps,
    bootstrap_reps = bootstrap_reps,
    seed = seed,
    constraints = constraints,
    bounds = bounds,
    estimator = estimator,
    save_path
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
    save_path = save_path,
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
#' \code{powRICLPM_Mplus()} creates and saves (a) syntax file(s) for performing a Monte Carlo power analysis for the random intercept cross-lagged panel model (RI-CLPM) in Mplus. Mplus model syntax can be created across multiple experimental conditions simultaneously. Conditions are defined in terms of sample size, number of time points, and proportion of between-unit variance (ICC). See "Details" for information on a) the naming conventions of parameters, and b) the various constraints that can be imposed on the model.
#'
#' @inheritParams powRICLPM
#' @param save_path A \code{character} string, denoting the path to the folder to save the Mplus syntax files in (default: current working directory).
#'
#' @details
#' \subsection{Syntax generation}{Mplus model syntax is created in multiple steps: First, the \code{MODEL POPULATION} command syntax is created in which parameters are constrained to population values. Second, the \code{MODEL} command syntax is created for model estimation. Optionally, syntax for constraints on the estimation model, in the \code{MODEL CONSTRAINTS} command, is created next. Ultimately, the parameter tables are combined to form character vectors containing the Mplus syntax to be exported (see "Details" of \code{\link{powRICLPM}} for more information on the \code{constraints} options).}
#'
#' \subsection{Naming conventions}{Details on the naming conventions can be found in the "Details" section of \code{\link{powRICLPM}}.}
#'
#' @return No return value, called for side effects (i.e., saving .txt file with Mplus syntax to \code{save_path}).
#'
#' @examples
#' # Create Mplus syntax for power analysis across range of sample sizes
#' ## Define population parameters for lagged effects
#' Phi <- matrix(c(.4, .1, .2, .3), ncol = 2, byrow = TRUE)
#'
#' ## Create and save Mplus model syntax
#' powRICLPM_Mplus(
#'   sample_size = c(400, 500),
#'   time_points = 3,
#'   ICC = 0.5,
#'   RI_cor = 0.3,
#'   Phi = Phi,
#'   within_cor = 0.3,
#'   reps = 10000,
#'   seed = 1234
#' )
#' @export
powRICLPM_Mplus <- function(search_lower = NULL,
                            search_upper = NULL,
                            search_step = 20,
                            sample_size = NULL,
                            time_points,
                            ICC,
                            RI_cor,
                            Phi,
                            within_cor,
                            reps = 1000,
                            seed = NA,
                            save_path = getwd(),
                            constraints = "none") {
  # Check arguments I
  time_points <- check_T(time_points, estimate_ME = FALSE)
  ICC <- check_ICC(ICC)
  RI_cor <- check_RIcor(RI_cor)
  wSigma <- check_within_cor(within_cor)
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
    i = "Mplus model syntax created:"
  )))
  cat("\n\n  Directory:", save_path)
  cat("\n  Sample size(s):", sample_size)
  cat("\n  Number of time points:", time_points)
  cat("\n  Proportion(s) random intercept variance:", ICC)

  # Return
  invisible()
}
