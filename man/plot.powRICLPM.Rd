% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.powRICLPM}
\alias{plot.powRICLPM}
\title{Plot results from \code{powRICLPM} object}
\usage{
\method{plot}{powRICLPM}(x, y = "power", ..., parameter = NULL)
}
\arguments{
\item{x}{A \code{powRICLPM} object.}

\item{y}{(optional) A \code{character} string, specifying which outcome is plotted on the y-axis (see "Details").}

\item{...}{(don't use)}

\item{parameter}{Character string of length denoting the parameter to visualize the results for.}
}
\value{
A \code{ggplot2} object.
}
\description{
Visualizes (using \pkg{ggplot2}) the results from a \code{powRICLPM} analysis, for a specific parameter, across all experimental conditions. By default, sample size is plotted on the x-axis, power on the y-axis, and results are grouped by the number of time points and wrapped by the proportion of between-unit variance. Optionally, the \code{y} argument can be used to change the variable on the y-axis to other outcomes from the \code{powRICLPM} analysis.
}
\details{
\subsection{y-axis options}{The following outcomes can be plotted on the y-axis:

\itemize{
  \item \code{average}: The average estimate.
  \item \code{MSE}: The mean square error.
  \item \code{coverage}: The coverage rate
  \item \code{accuracy}: The average width of the confidence interval.
  \item \code{SD}: Standard deviation of parameter estimates.
  \item \code{SEAvg}: Average standard error.
  \item \code{bias}: The absolute difference between the average estimate and population value.
}
}
}
\examples{
\dontshow{
load(system.file("extdata", "out_preliminary.RData", package = "powRICLPM"))
}
# Visualize power for "wB2~wA1" across simulation conditions
plot(out_preliminary, parameter = "wB2~wA1")

# Visualize bias for "wB2~wA1" across simulation conditions
plot(out_preliminary, y = "bias", parameter = "wB2~wA1")

# Visualize coverage rate for "wB2~wA1" across simulation conditions
plot(out_preliminary, y = "coverage", parameter = "wB2~wA1")

# Visualize MSE for autoregressive effect across simulation conditions
plot(out_preliminary, y = "MSE", parameter = "wA2~wA1")

# Error: No parameter specified
try(plot(out_preliminary))
}
\seealso{
\itemize{
  \code{\link{give}}: Extract information (e.g., performance measures) for a specific parameter, across all experimental conditions. This function is used internally in \code{plot.powRICLPM}.
}
}
