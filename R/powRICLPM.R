#' @title
#' Power analysis for the RI-CLPM
#'
#' @description
#' \code{powRICLPM()} performs a Monte Carlo power analysis for the random intercept cross-lagged panel model (RI-CLPM). It computes performance \emph{metrics} for all RI-CLPM parameters (e.g., bias, mean square error, coverage, power, etc), and can perform power analysis across multiple experimental conditions simultaneously. Conditions can vary in terms of sample size, number of time points, and proportion of between-unit variance (i.e., ICC). See "Details" for information on the data simulation/estimation, internal naming conventions of parameters, and parallel execution options for speeding up the analysis.
#'
#' @param target_power A numeric value between 0 and 1, denoting the targeted power level.
#' @param search_lower A positive integer, denoting the lower bound of the range of sample sizes.
#' @param search_upper A positive integer, denoting the upper bound of the range of sample sizes.
#' @param search_step A positive integer, denoting the increment in sample size.
#' @param sample_size An integer (vector) with elements at least larger than 20 (i.e., the number of free parameters in a basic bivariate RI-CLPM with 3 repeated measures), indicating the sample sizes at which to evaluate power. This argument can be specified as an alternative to the `search_` arguments to denote specific sample sizes, rather than an entire range.
#' @param time_points An integer (vector) with elements larger than 3, indicating number of time points.
#' @param ICC A `double` denoting the proportion of variance at the between-unit level.
#' @param RI_cor A `double` denoting the correlation between random intercepts.
#' @param Phi A matrix of standardized autoregressive and cross-lagged effects in the population. Columns represent predictors and rows represent outcomes.
#' @param wSigma A correlation matrix for the within-unit components.
#' @param skewness A numeric value denoting the skewness values for the observed variables. For more information, see \code{\link[lavaan]{simulateData}}.
#' @param kurtosis A numeric value denoting the kurtosis values for the observed variables. For more information, see \code{\link[lavaan]{simulateData}}.
#' @param alpha A `double` denoting the significance criterion (defaults to 0.05).
#' @param reps A positive `integer` denoting the number of Monte Carlo replications to be used during simulations.
#' @param seed An `integer` of length 1. If multiple cores are used, a seed of length 1 will be used to generate a full L'Ecuyer-CMRG seed for all cores. For more information, see \code{\link[furrr]{furrr_options}}.
#' @param save_path A character string naming the directory to save any (data) files to.
#' @param parameter A character string denoting the parameter of primary interest. If specified, \code{\link{coef}}, \code{\link{summary}}, and \code{\link{plot}} output results specifically for this parameter.
#' @param constraints A character string specifying the RI-CLPM parameters that should be constrained over time in the estimation model. "\code{lagged}" refers to both cross-lagged and autoregressive effects, "\code{residuals}" refers to the residual variances, "\code{within}" refers to both lagged effects and residual variances, and "\code{stationarity}" imposes stationarity constraints on the lagged effects and residual variances. By default, the lagged effects and residual variances are allowed to vary freely over time.
#' @param bounds A logical denoting if bounded estimation should be used for the latent variable variances in the model. This can only be used when no constraints are imposed on the estimation model. See "Details". Bounded estimation is useful to avoid nonconvergence in small samples (< 100). Here, automatic wide bounds are used as advised by De Jonckere and Rosseel (2022). For more information, see \code{optim.bounds} in \code{\link[lavaan]{lavOptions}}.
#'
#' @details
#' A rationale for the power analysis strategy as implemented in this package can be found in Mulder (2022).
#'
#' \subsection{Data generation and model estimation}{Data are generated using \code{\link[lavaan]{simulateData}} from the \pkg{lavaan} package. The within-components in the data generating model have variance of 1 such that the lagged effects as specified in \code{Phi} can be interpreted as standardized effects. This also implies that \code{Phi} is subject to the stationarity constraints of a VAR(1) model (i.e., eigenvalues smaller than 1). Based on \code{Phi} and \code{wSigma}, the residual variances-covariance for the within-components at wave 2 and later is computed (see \code{\link{compute_Psi}}).
#'
#' Data are analyzed using \code{\link[lavaan]{lavaan}} from the \pkg{lavaan} package. The default estimator is maximum likelihood. The population parameter values are used as starting values. Optionally, the user can use bounded estimation, or impose constraints on parameters over time, as explained in the respective arguments.}
#'
#' \subsection{Naming conventions for observed and latent variables}{The observed variables in the RI-CLPM are given default names, namely capital letters in alphabetical order, with numbers denoting the measurement occasion. For example, for a bivariate RICLPM with 3 time points, we observe \code{A1}, \code{A2}, \code{A3}, \code{B1}, \code{B2}, and \code{B3}. Their within-components are denoted by \code{wA1}, \code{wA2}, ..., \code{wB3}, respectively. The between-components have \code{RI_} prepended to the variable name, resulting in \code{RI_A} and \code{RI_B}.
#'
#' Parameters are denoted using \pkg{lavaan} model syntax. For more information, see \url{https://lavaan.ugent.be/tutorial/syntax1.html}. For example, the random intercept variances are denoted by \code{RI_A~~RI_A} and \code{RI_B~~RI_B}, the cross-lagged effects at the first wave as \code{wB2~wA1} and \code{wA2~wB1}, and the autoregressive effects as \code{wA2~wA1} and \code{wB2~wB1}. Use \code{summary(object, names = TRUE)} to extract parameter names from the \code{powRICLPM} object.}
#'
#' \subsection{Parallel processing using `furrr`}{To speed up the analysis, power analysis for multiple experimental conditions can be executed in parallel. This has been implemented using \pkg{furrr}. By default the analysis is executed sequentially (i.e., single-core). Parallel execution (i.e., multicore) can be setup using \code{\link[future]{plan}}, for example \code{plan(multisession, workers = 4)}. For more information and options, see \url{https://furrr.futureverse.org}.}
#'
#' \subsection{Progress bar using `progressr`}{A progress bar displaying the status of the power analysis has been implemented using \pkg{progressr}. By default, a simple progress bar will be shown. For more information on how to control this progress bar and several other notification options (e.g., auditory notifications), see \url{https://progressr.futureverse.org}.}
#'
#' @return
#' A list containing a "conditions" and "session" element. The "condition" element is again a list of experimental conditions, where each element is itself a list containing the input and output of the power analysis for a specific experimental condition. This includes:
#'
#' \itemize{
#'   \item \code{sample_size}: A sample size.
#'   \item \code{time_points}: A number of time points.
#'   \item \code{ICC}: A proportion of between-unit variance.
#'   \item \code{RI_var}: Random intercept variance (based on \code{ICC}).
#'   \item \code{RI_cov}: Random intercept covariance (based on \code{ICC} and \code{RI_var}).
#'   \item \code{reps}: The number of Monte Carlo replication.
#'   \item \code{pop_synt}: \pkg{lavaan} model syntax containing population values for data generation.
#'   \item \code{pop_tab}: \pkg{lavaan} parameter table for data generation.
#'   \item \code{est_synt}: \pkg{lavaan} model syntax for estimation.
#'   \item \code{est_tab}: \pkg{lavaan} parameter table for estimation.
#'   \item \code{skewness}: Skewness value(s) for the observed variables.
#'   \item \code{kurtosis}: Kurtosis value(s) for the observed variables.
#'   \item \code{alpha}: Significance criterion.
#'   \item \code{save_path}: The directory (data) files were saved to.
#'   \item \code{results}: Data frame containing the power analysis results
#'   \item \code{errors}: A logical vector denoting failed Monte Carlo replications,
#'   \item \code{not_converged}: A logical vector denoting non-converged Monte Carlo replications.
#'   \item \code{inadmissible} A logical vector denoting Monte Carlo replications that resulted in negative variances or non-positive definite matrices.
#' }
#'
#' The "session" element is a list containing information common to all experimental conditions, including
#' \itemize{
#'   \item \code{Psi}: Residual variance-covariance matrix for the within-components.
#'   \item \code{reps}: The number of Monte Carlo replication.
#'   \item \code{target_power}: The desired power level.
#'   \item \code{save_path}: The directory (data) files were saved to.
#'   \item \code{parameter}: The parameter of primary interest.
#'   \item \code{constraints}: The constraints imposed on the estimation model.
#'   \item \code{bounds}: A logical denoting if bounded estimation was used for the latent variable variances in the model.
#' }
#'
#' @author Jeroen D. Mulder \email{j.d.mulder@@uu.nl}
#' @export
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{powRICLPM_Mplus}}: Create Mplus model syntax for RI-CLPM power analysis.
#'   \item \code{\link{summary.powRICLPM}}: Summarize powRICLPM results.
#'   \item \code{\link{coef.powRICLPM}}: Extract metrics for a specific parameter, across all experimental conditions.
#'   \item \code{\link{plot.powRICLPM}}: Visualize results of a powRICLPM analysis.
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
#'     reps = 5,
#'     seed = 123456
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
                      skewness = 0,
                      kurtosis = 0,
                      alpha = 0.05,
                      reps = 1000,
                      seed = NA,
                      save_path = NA,
                      parameter = NULL,
                      constraints = NULL,
                      bounds = FALSE) {

  # Check arguments I
  target_power <- check_target(target_power)
  time_points <- check_T(time_points)
  ICC <- check_ICC(ICC)
  RI_cor <- check_RIcor(RI_cor)
  wSigma <- check_wSigma(wSigma)
  Phi <- check_Phi(Phi)
  skewness <- check_skewness(skewness)
  kurtosis <- check_kurtosis(kurtosis)
  alpha <- check_alpha(alpha)
  reps <- check_reps(reps)
  seed <- check_seed(seed)
  save_path <- check_save(save_path)
  parameter <- check_parameter(parameter)
  constraints <- match.arg(constraints, c("none", "lagged", "residuals", "within", "stationarity"))
  bounds <- check_bounds(bounds, constraints)

  # Compute population parameter values for data generation
  Psi <- compute_Psi(Phi, wSigma)

  # Get candidate sample sizes
  if (is.null(sample_size)) {
    sample_size <- seq(search_lower, search_upper, search_step)
  }

  # Check arguments II
  sample_size <- check_N(sample_size, time_points)
  Psi <- check_Psi(Psi)

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
    skewness = skewness,
    kurtosis = kurtosis,
    alpha = alpha,
    reps = reps,
    seed = seed,
    save_path = save_path,
    parameter = parameter,
    constraints = constraints,
    bounds = bounds
  )

  # Prepare progress bar
  p <- progressr::progressor(along = object$conditions)

  # Run Monte Carlo simulation for each condition
  object$conditions <- furrr::future_map(object$conditions,
    run_condition,
    progress = p,
    bounds = bounds,
    .options = furrr::furrr_options(
      seed = seed,
      scheduling = 2L # Dynamic scheduling
    )
  )

  # object$conditions <- purrr::map(object$conditions,
  #                                 run_condition,
  #                                 progress = p,
  #                                 bounds = bounds)

  # Assign "powRICLPM" class to object
  class(object) <- c("powRICLPM", class(object))

  return(object)
}


#' @title
#' Create Mplus xyntax for RI-CLPM power analysis
#'
#' @description
#' \code{powRICLPM_Mplus()} creates and saves syntax for performing a Monte Carlo power analysis for the random intercept cross-lagged panel model (RI-CLPM) in Mplus. It can create Mplus model syntax across multiple experimental conditions simultaneously. Conditions can vary in terms of sample size, number of time points, and proportion of between-unit variance. See "Details" for information on the naming conventions of parameters.
#'
#' @inheritParams powRICLPM
#'
#' @details
#' \subsection{Syntax generation}{Mplus model syntax is created in multiple steps: First, the \code{MODEL POPULATION} command syntax is created in which parameters are constrained to population values. Second, the \code{MODEL} command syntax is created for model estimation. Optionally, if the user wants to impose constraints over time for the estimation model, the \code{MODEL CONSTRAINTS} command syntax is created for model estimation. Ultimately, the parameter tables are combined to form character vectors containing the Mplus syntax to be exported.}
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
#'   seed = 123456
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
                            constraints = NULL) {
  # Check arguments I
  time_points <- check_T(time_points)
  ICC <- check_ICC(ICC)
  RI_cor <- check_RIcor(RI_cor)
  wSigma <- check_wSigma(wSigma)
  Phi <- check_Phi(Phi)
  reps <- check_reps(reps)
  seed <- check_seed(seed)
  save_path <- check_save(save_path)
  constraints <- match.arg(constraints, c("none", "lagged", "residuals", "within", "stationarity"))

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
  cat("Mplus model syntax created:")
  cat("\n\n  Directory:", save_path)
  cat("\n  Sample size(s):", sample_size)
  cat("\n  Number of time points:", time_points)
  cat("\n  Proportion(s) random intercept variance:", ICC)

  # Return
  invisible()
}
