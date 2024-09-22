#' Compute Residual Variances of Lagged Within-Components
#'
#' @description
#' \code{compute_Psi()} computes the variance-covariance matrix of the within-unit residuals (within a random intercept cross-lagged panel model) from wave 2 and later from specified lagged effects in \code{Phi} and specified correlations between the within-components \code{within_cor}.
#'
#' @inheritParams powRICLPM
#'
#' @return A variance-covariance matrix for within-unit residuals from wave 2 and later.
#'
#' @details
#' The function is based on Equation (3.26) in \href{https://doi.org/10.7551/mitpress/6444.001.0001}{Kim and Nelson (1999, p. 27)}.
#'
#' @noRd
compute_Psi <- function(Phi, within_cor) {
  wSigma <- matrix(c(1, within_cor, within_cor, 1), ncol = 2, byrow = TRUE)
  Psi <- matrix(
    data = (diag(length(wSigma)) - Phi %x% Phi) %*% c(wSigma),
    nrow = nrow(Phi)
  )
  return(Psi)
}

#' Compute Random Intercept Variance
#'
#' \code{compute_RI_var()} computes the variance of the random intercept based on the proportion of between-unit variance \code{ICC} and conditional on within-unit variances of 1.
#'
#' @inheritParams powRICLPM
#'
#' @return A scalar representing the random intercept variance.
#'
#' @noRd
compute_RI_var <- function(ICC) {
  1 / (1 - ICC) - 1
}

#' Compute Random Intercept Correlation
#'
#' @param RI_cov A numeric value denoting the covariance between the random intercepts.
#' @param RI_var A numeric value denoting the variance of the random intercepts.
#'
#' @return A scalar representing the correlation between the random intercepts.
#'
#' @noRd
compute_RI_cov <- function(RI_cor, RI_var) {
  RI_cor * RI_var
}

#' Compute Measurement Error Variance
#'
#' @inheritParams compute_RI_cov
#' @param reliability A numeric value between 0 and 1, denoting the reliability of the variables.
#'
#' @noRd
compute_ME_var <- function(RI_var, reliability) {
  ((1 - reliability) * (1 + RI_var)) / reliability
}
