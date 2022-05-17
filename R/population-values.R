#' @title
#' Compute Residual Variances of Lagged Within-Components
#'
#' @description
#' Within an RI-CLPM context, this function computes the variance-covariance matrix of the within-unit residuals from wave 2 and later, given the lagged effects in \code{Phi} and an observed variance-covariance matrix \code{Sigma}.
#'
#' @inheritParams powRICLPM
#'
#' @return A variance-covariance matrix for within-unit residuals from wave 2 and later.
#'
#' @details
#' The function is based on Equation (3.26) in Kim and Nelson (1999, p. 27).
compute_Psi <- function(Phi, wSigma) {
  matrix(
    (diag(length(wSigma)) - Phi %x% Phi) %*% c(wSigma),
    nrow = nrow(Phi)
  )
}

#' @title
#' Compute Random Intercept Variance
#'
#' Compute the variance of the random intercept based on the proportion of between-unit variance (ICC) and conditional on within-unit variances of 1.
#'
#' @inheritParams powRICLPM
#'
#' @return A scalar representing the random intercept variance.
#'
#' @noRd
compute_RI_var <- function(ICC) {
  1 / (1 - ICC) - 1
}

#' @title
#' Compute Random Intercept Correlation
#'
#' @param RI_cov A numeric value denoting the covariance between the random intercepts.
#' @param RI_var A numeric value denoting the variance of the random intercepts.
#'
#' @return A scalar representing the correlation between the random intercepts.
#'
#' @noRd
compute_RI_cov <- function(RI_cor, RI_var) {
  return(RI_cor * RI_var)
}
