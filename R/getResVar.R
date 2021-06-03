#' Compute Residual Variances of Lagged Within-Components
#'
#' For a stationary process, this function computes the residual variance of the within components given lagged Phi and an observed variance covariance matrix.
#' It is based on Equation (3.26) in Kim and Nelson (1999, p. 27).
#'
#' @param Phi Matrix of autoregressive and cross-lagged effects.
#' @param Sigma Variance-covariance matrix.
#'
#' @return Residual variance of lagged within-unit components.
#' @export
#'
#' @examples
#' # Specify lagged effects
#' Phi <- matrix(c(.45 , .1, .15, .15 ), ncol = 2, byrow = T)
#'
#' # Specify an observed variance-covariance matrix
#' Sigma <- matrix(c(1 , .4, .4, 1 ), ncol = 2, byrow = T)
#'
#' # Compute residual variances
#' getResVar(Phi, Sigma)

getResVar <- function(Phi, Sigma){
  matrix((diag(length(Sigma)) - Phi %x% Phi) %*% c(Sigma), nrow = nrow(Phi))
}
