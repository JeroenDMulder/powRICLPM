#' Compute residual variances of lagged within-components
#'
#' @description
#' \code{compute_Psi()} computes the variance-covariance matrix of the within-unit residuals (within a random intercept cross-lagged panel model) from wave 2 and later, given the lagged effects in \code{Phi} and an observed variance-covariance matrix \code{Sigma}.
#'
#' @inheritParams powRICLPM
#'
#' @return A variance-covariance matrix for within-unit residuals from wave 2 and later.
#'
#' @details
#' The function is based on Equation (3.26) in \href{https://doi.org/10.7551/mitpress/6444.001.0001}{Kim and Nelson (1999, p. 27)}.
#'
#' @noRd
compute_Psi <- function(Phi, wSigma) {
  matrix(
    (diag(length(wSigma)) - Phi %x% Phi) %*% c(wSigma),
    nrow = nrow(Phi)
  )
}

#' Compute random intercept variance
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

#' Compute random intercept correlation
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

#' Compute measurement error variance
#'
#' @inheritParams compute_RI_cov
#' @param reliability A numeric value between 0 and 1, denoting the reliability of the variables.
#'
#' @noRd
compute_ME_var <- function(RI_var, reliability) {
  ((1 - reliability) * (1 + RI_var)) / reliability
}
