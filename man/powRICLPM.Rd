% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/powRICLPM.R
\name{powRICLPM}
\alias{powRICLPM}
\title{Power analysis for the RI-CLPM (and STARTS model)}
\usage{
powRICLPM(
  target_power = 0.8,
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
  significance_criterion = 0.05,
  alpha = NULL,
  reps = 20,
  bootstrap_reps = NULL,
  seed = NA,
  constraints = "none",
  bounds = FALSE,
  estimator = "ML",
  save_path = NULL,
  software = "lavaan"
)
}
\arguments{
\item{target_power}{A numeric value between 0 and 1, denoting the targeted power level.}

\item{search_lower}{A positive \code{integer}, denoting the lower bound of a range of sample sizes.}

\item{search_upper}{A positive \code{integer}, denoting the upper bound of a range of sample sizes.}

\item{search_step}{A positive \code{integer}, denoting an increment in sample size.}

\item{sample_size}{(optional) An \code{integer} (vector), indicating specific sample sizes at which to evaluate power, rather than specifying a range using the \code{search_*} arguments.}

\item{time_points}{An \code{integer} (vector) with elements at least larger than 3, indicating number of time points.}

\item{ICC}{A \code{double} (vector) with elements between 0 and 1, denoting the proportion of (true score) variance at the between-unit level. When measurement error is included in the data generating model, ICC is computed as the variance of the random intercept factor divided by the true score variance (i.e., controlled for measurement error).}

\item{RI_cor}{A \code{double} between 0 and 1, denoting the correlation between random intercepts.}

\item{Phi}{A matrix, with standardized autoregressive effects (on the diagonal) and cross-lagged effects (off-diagonal) in the population. Columns represent predictors and rows represent outcomes.}

\item{within_cor}{A \code{double} between 0 and 1, denoting the correlation between the within-unit components.}

\item{reliability}{(optional) A \code{numeric} vector with elements between 0 and 1, denoting the reliability of the variables (see "Details").}

\item{skewness}{(optional) A \code{numeric}, denoting the skewness values for the observed variables (see \code{\link[lavaan]{simulateData}}).}

\item{kurtosis}{(optional) A \code{numeric} value, denoting the excess kurtosis values (i.e., compared to the kurtosis of a normal distribution) for the observed variables (see \code{\link[lavaan]{simulateData}}).}

\item{estimate_ME}{(optional) A \code{logical}, denoting if measurement error variance should be estimated in the RI-CLPM (see "Details").}

\item{significance_criterion}{(optional) A \code{double}, denoting the significance criterion.}

\item{alpha}{(don't use) Deprecated, use \code{significance_criterion} instead.}

\item{reps}{A positive \code{integer}, denoting the number of Monte Carlo replications to be used during simulations.}

\item{bootstrap_reps}{(superseded) Uncertainty regarding simulation estimates is now computed analytically based on Morris et al. (2017). This argument is not used anymore.}

\item{seed}{An \code{integer} of length 1. If multiple cores are used, a seed will be used to generate a full L'Ecuyer-CMRG seed for all cores.}

\item{constraints}{(optional) A \code{character} string, specifying the type of constraints that should be imposed on the estimation model (see "Details").}

\item{bounds}{(optional) A \code{logical}, denoting if bounded estimation should be used for the latent variable variances in the model (see "Details").}

\item{estimator}{(options) A \code{character} string of length 1, denoting the estimator to be used (default: \code{ML}, see "Details").}

\item{save_path}{A \code{character} string of length 1, naming the directory to save (data) files to (used for validation purposes of this package). Variables are saved in alphabetical and numerical order.}

\item{software}{A \code{character} string of length, naming which software to use for simulations; either "lavaan" or "Mplus" (see "Details").}
}
\value{
An object of class \code{powRICLPM}, upon which \code{summary()}, \code{print()}, and \code{plot()} can be used. The returned object is a \code{list} with a \code{conditions} and \code{session} element. \code{condition} itself is a \code{list} of experimental conditions, where each element is again a \code{list} containing the input and output of the power analysis for that particular experimental condition. \code{session} is a \code{list} containing information common to all experimental conditions.
}
\description{
Perform a Monte Carlo power analysis for the random intercept cross-lagged panel model (RI-CLPM) and the stable trait autoregressive trait state model (STARTS). This function computes performance metrics such as bias, mean square error, coverage, power, etc, for all model parameters, and can perform power analyses across multiple experimental conditions simultaneously. Conditions are defined in terms of sample size, number of time points, proportion of between-unit variance (ICC), and indicator reliability. See "Details" for information on (a) internal data simulation, (b) internal model estimation, (c) \code{powRICLPM}'s naming conventions of parameters, (d) parallel execution capabilities for speeding up the analysis, and (e) various extensions, such as the option to include measurement errors for data generation and estimation (i.e., the STARTS model), imposing various constraints over time, and many more.
}
\details{
A rationale for the power analysis strategy implemented in this package can be found in Mulder (2023).

\subsection{Data Generation}{Data are generated using \code{\link[lavaan]{simulateData}} from the \pkg{lavaan} package. Based on \code{Phi} and \code{within_cor}, the residual variances and covariances for the within-components at wave 2 and later are computed, such that the within-components themselves have a variance of 1. This implies that the lagged effects in \code{Phi} can be interpreted as standardized effects.}

\subsection{Model Estimation using lavaan}{When \code{software = "lavaan"} (default), generated data are analyzed using \code{\link[lavaan]{lavaan}} from the \pkg{lavaan} package. The default estimator is maximum likelihood (\code{ML}). Other maximum likelihood based estimators implemented in \href{https://lavaan.ugent.be/tutorial/est.html}{\pkg{lavaan}} can be specified as well. When skewed or kurtosed data are generated (using the \code{skewness} and \code{kurtosis} arguments), the estimator defaults to robust maximum likelihood \code{MLR}. The population parameter values are used as starting values.

Parameter estimates from non-converged model solutions are discarded from the results. When \code{bounds = FALSE}, inadmissible parameter estimates from converged solutions (e.g., a negative random intercept variance) are discarded. When \code{bounds = TRUE}, inadmissible parameter estimates are retained following advice by De Jonckere and Rosseel (2022). The results include the minimum estimates for all parameters across replications to diagnose which parameter(s) might be the cause of the inadmissible solution.}

\subsection{Using Mplus}{When \code{software = "Mplus"}, Mplus input files will be generated and saved into \code{save_path}. Note that it is not possible to generate skewed or kurtosed data in Mplus via the \code{powRICLPM} package. Furthermore, bounded estimation is not available in Mplus. Therefore, the \code{skewness}, \code{kurtosis}, and \code{bounds} will be ignored when \code{software = "Mplus"}. }

\subsection{Naming Conventions Observed and Latent Variables}{The observed variables in the RI-CLPM are given default names, namely capital letters in alphabetical order, with numbers denoting the measurement occasion. For example, for a bivariate RICLPM with 3 time points, we observe \code{A1}, \code{A2}, \code{A3}, \code{B1}, \code{B2}, and \code{B3}. Their within-components are denoted by \code{wA1}, \code{wA2}, ..., \code{wB3}, respectively. The between-components have \code{RI_} prepended to the variable name, resulting in \code{RI_A} and \code{RI_B}.

Parameters are denoted using \pkg{lavaan} model syntax (see \href{https://lavaan.ugent.be/tutorial/syntax1.html}{the \pkg{lavaan} website}). For example, the random intercept variances are denoted by \code{RI_A~~RI_A} and \code{RI_B~~RI_B}, the cross-lagged effects at the first wave as \code{wB2~wA1} and \code{wA2~wB1}, and the autoregressive effects as \code{wA2~wA1} and \code{wB2~wB1}. Use \code{give(object, "names")} to extract parameter names from the \code{powRICLPM} object.}

\subsection{Parallel Processing and Progress Bar}{To speed up the analysis, power analysis for multiple experimental conditions can be executed in parallel. This has been implemented using \pkg{future}. By default the analysis is executed sequentially (i.e., single-core). Parallel execution (i.e., multicore) can be setup using \code{\link[future]{plan}}, for example \code{plan(multisession, workers = 4)}. For more information and options, see \url{https://future.futureverse.org/articles/future-1-overview.html#controlling-how-futures-are-resolved}.

A progress bar displaying the status of the power analysis has been implemented using \pkg{progressr}. By default, a simple progress bar will be shown. For more information on how to control this progress bar and several other notification options (e.g., auditory notifications), see \url{https://progressr.futureverse.org}.}

\subsection{Extension: Measurement Errors (STARTS model)}{Including measurement error to the RI-CLPM makes the model equivalent to the bivariate STARTS model by Kenny and Zautra (2001) without constraints over time. Measurement error can be added to the generated data through the \code{reliability} argument. Setting the reliability-argument to 0.8 implies that 80 percent is the true score variance, and 20 measurement error variance. \code{ICC} then denotes the proportion of \emph{true score variance} captured by the random intercept factors. Estimating measurement errors (i.e., the STARTS model) is done by setting \code{estimate_ME = TRUE}.}

\subsection{Extension: Imposing Constraints}{The following constraints can be imposed on the estimation model using the \code{constraints = "..."} argument:

\itemize{
\item \code{lagged}: Time-invariant autoregressive and cross-lagged effects.
\item \code{residuals}: Time-invariant residual variances.
\item \code{within}: Time-invariant lagged effects and residual variances.
\item \code{stationarity}: Constraints such that at the within-unit level a stationary process is estimated. This included time-invariant lagged effects, and constraints on the residual variances.
\item \code{ME}: Time-invariant measurement error variances. Only possible when \code{estimate_ME = TRUE}.
}
}

\subsection{Extension: Bounded Estimation}{Bounded estimation is useful to avoid nonconvergence in small samples. Here, automatic wide bounds are used as advised by De Jonckere and Rosseel (2022), see \code{optim.bounds} in \code{\link[lavaan]{lavOptions}}. This option can only be used when no constraints are imposed on the estimation model.}
}
\examples{
# Define population parameters for lagged effects
Phi <- matrix(c(.4, .1, .2, .3), ncol = 2, byrow = TRUE)

# (optional) Set up parallel computing (i.e., multicore, speeding up the analysis)
library(future)
library(progressr)
future::plan(multisession, workers = 6)

\donttest{
# Run analysis (`reps` is small, because this is an example)
with_progress({
  out_preliminary <- powRICLPM(
    target_power = 0.8,
    search_lower = 500,
    search_upper = 700,
    search_step = 100,
    time_points = c(3, 4),
    ICC = c(0.4, 0.6),
    reliability = c(1, 0.8),
    RI_cor = 0.3,
    Phi = Phi,
    within_cor = 0.3,
    reps = 100,
    seed = 1234
  )
})
}

\dontshow{
## Shut down parallel workers (done for sake of example, normally not needed)
future::plan("sequential")
}

}
\references{
De Jonckere, J., & Rosseel, Y. (2022). Using bounded estimation to avoid nonconvergence in small sample structural equation modeling. \emph{Structural Equation Modeling}, \emph{29}(3), 412-427. \doi{10.1080/10705511.2021.1982716}

Kenny, D. A., & Zautra, A. (2001). Trait–state models for longitudinal data. \emph{New methods for the analysis of change} (pp. 243–263). American Psychological Association. \doi{10.1037/10409-008}

Mulder, J. D. (2022). Power analysis for the random intercept cross-lagged panel model using the \emph{powRICLPM} R-package. \emph{Structural Equation Modeling}. \doi{10.1080/10705511.2022.2122467}
}
\seealso{
\itemize{
\item \code{\link{summary.powRICLPM}}: Summarize the setup of \code{powRICLPM} object.
\item \code{\link{give}}: Extract information from \code{powRICLPM} objects.
\item \code{\link{plot.powRICLPM}}: Visualize results \code{powRICLPM} object for a specific parameter.
}
}
\author{
Jeroen D. Mulder \email{j.d.mulder@uu.nl}
}
