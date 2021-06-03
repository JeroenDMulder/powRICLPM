#' Create Lavaan Code for the RICLPM
#'
#' @param nK Number of variables
#' @param nT Number of time points
#' @param Phi Matrix of autoregressive and cross-lagged effects in the population. Columns are predictors, rows are outcomes.
#' @param Psi Residual variance-covariance matrix at within-unit level.
#' @param wSigma Correlation matrix of within-unit components
#' @param RICov Vector of correlations between random intercepts. The elements should in ascending order.
#' @param RIVarProp Proportion of variance at the between-unit level.
#'
#' @return
#' @export
#'
#' @examples
#' # Get lavaan syntax for generating data of a bivariate RI-CLPM with 3 repeated measures
#' Phi = matrix(c(.5, .1, .4, .5), ncol = 2, byrow = T)
#' wSigma = matrix(c(1 , .3, .3, 1) , ncol = 2, byrow = T)
#'
#' pop <- getLavMod(nK = 2, nT = 3, Phi, Psi = Psi, wSigma = wSigma, RICov = .3, RIVarProp = .5)
#'
#' # Get lavaan syntax for estimating a bivariate RI-CLPM with 3 repeated measures
#' mod <- getLavMod(nK = 2, nT = 3)
getLavMod <- function(nK = 2, nT, Phi = NULL, Psi = NULL, wSigma = NULL, RICov = NULL, RIVarProp = NULL, nameVar = NULL){

  # Options
  if(is.null(nameVar)) nameVar <- LETTERS[1:nK] # Generate default variable names

  # Global
  nameObs <- matrix(sapply(nameVar, paste0, 1:nT), nrow = nT, ncol = nK) # Create matrix of observed variable names
  nameWComps <- matrix(paste0("w", nameObs), nrow = nT, ncol = nK) # Create matrix of within-component names
  nameRI <- paste0("RI_", nameVar) # Create matrix of between-component names

  # Create parameter table
  lavTab <- rbind(lavRI(nT = nT, nK = nK, nameRI = nameRI, nameObs = nameObs),
                  lavRIVar(nK= nK, nameRI = nameRI, RIVarProp = RIVarProp),
                  lavRICov(nameRI = nameRI, RICov = RICov),
                  lavWComp(nameWComps = nameWComps, nameObs = nameObs),
                  lavWEff(nK, nT, nameWComps, Phi),
                  lavRes1Var(nameWComps = nameWComps, wSigma = wSigma),
                  lavRes1Cov(nameWComps = nameWComps, wSigma = wSigma),
                  lavRes2Var(Psi, nameWComps),
                  lavRes2Cov(Psi, nameWComps),
                  lavME(nameObs))
  return(as.data.frame(lavTab, stringsAsFactors = F))
}

#' Create Lavaan Code for Random Intercepts of RICLPM
#'
#' @param nT Number of time points
#' @param nameRI Character vector with names of the random intercepts
#' @param nameObs Character vector with names of the observed variables
#'
#' @return
#'
#' @examples
lavRI <- function(nT, nK, nameRI, nameObs){
  lhs <- rep(nameRI, each = nT)
  op <- rep("=~", times = nK*nT)
  pv <- rep("1", times = nK*nT)
  con <- rep("*", times = nK*nT)
  rhs <- c(nameObs)
  return(cbind(lhs, op, pv, con, rhs))
}

#' Create Lavaan Code for Variance of Random Intercepts of RICLPM
#'
#' @param nK Number of variables
#' @param nameRI Character vector with names of the random intercepts.
#' @param RIVarProp Proportion of variance at the between-unit level.
#'
#' @return
#'
#' @examples
lavRIVar <- function(nK, nameRI, RIVarProp = NULL){
  lhs <- nameRI
  op <- rep("~~", times = nK)
  if(is.null(RIVarProp)){
    pv <- con <- rep("", times = nK)
  } else {
    pv <- rep((1 / (1 - RIVarProp) - 1), times = nK)
    con <- rep("*", times = nK)
  }
  rhs <- nameRI
  return(cbind(lhs, op, pv, con, rhs))
}

#' Create Lavaan Code for Covariance of Random Intercepts of RICLPM
#'
#' @param nameRI Character vector with names of the random intercepts.
#' @param RICov Integer vector covariances between the random intercepts.
#'
#' @return
#'
#' @examples
lavRICov <- function(nameRI, RICov){
  combnRI <- t(combn(nameRI, 2))
  lhs <- combnRI[, 1]
  op <- "~~"
  if(is.null(RICov)){
    pv <- con <- ""
  } else {
    pv <- RICov
    con <- "*"
  }
  rhs <- combnRI[, 2]
  return(cbind(lhs, op, pv, con, rhs))
}

#' Create Lavaan Code for Within Components of RICLPM
#'
#' @param nameWComps Character vector with names of within-unit components.
#' @param nameObs Character vector with names of observed variables.
#'
#' @return
#'
#' @examples
lavWComp <- function(nameWComps, nameObs){
  lhs <- c(nameWComps)
  op <- rep("=~", times = length(nameWComps))
  pv <- rep("1", times = length(nameWComps))
  con <- "*"
  rhs <- c(nameObs)
  return(cbind(lhs, op, pv, con, rhs))
}

#' Create Lavaan Code for Within Lagged Effects of RICLPM
#'
#' @param nK Number of variables.
#' @param nT Number of time points.
#' @param nameWComps Character vector with names of within-unit components.
#' @param Phi Matrix of autoregressive and cross-lagged effects in the population.
#'
#' @return
#'
#' @examples
lavWEff <- function(nK, nT, nameWComps, Phi = NULL){
  lhs <- rep(c(t(nameWComps))[-(1:nK)] # Create vector with outcomes
             , each = nK)
  op <- rep("~", times = nK^2)
  if(is.null(Phi)){
    pv <- con <- rep("", times = nK^2)
  } else {
    pv <- c(t(Phi))
    con <- "*"
  }
  rhs <- c(apply(nameWComps[-nT,], 1, rep, times = nK))
  return(cbind(lhs, op, pv, con, rhs))
}

#' Create Lavaan Code for Residual Variance at Wave 1 of RICLPM
#'
#' @param nameWComps Character vector with names of within-unit components.
#' @param wSigma Correlation matrix of within-unit components.
#'
#' @return
#'
#' @examples
lavRes1Var <- function(nameWComps, wSigma){
  lhs <- rhs <- nameWComps[1, ]
  op <- rep("~~", times = nK)
  if(is.null(wSigma)){
    pv <- con <- rep("", times = nK)
  } else {
    pv <- rep("1", times = nK)
    con <- "*"
  }
  return(cbind(lhs, op, pv, con, rhs))
}

#' Create Lavaan Code for Residual Covariance at Wave 1 of RICLPM
#'
#' @param nameWComps Character vector with names of within-unit components.
#' @param wSigma Correlation matrix of within-unit components.
#'
#' @return
#'
#' @examples
lavRes1Cov <- function(nameWComps, wSigma = NULL){
  combnWComps <- t(combn(nameWComps[1, ], 2))
  lhs <- combnWComps[,1]
  rhs <- combnWComps[,2]
  op <- rep("~~", times = nrow(combnWComps))
  if(is.null(wSigma)){
    pv <- con <- rep("", times = nrow(combnWComps))
  } else {
    pv <- c(wSigma[lower.tri(wSigma)]) # Get covariances
    con <- "*"
  }
  return(cbind(lhs, op, pv, con, rhs))
}

#' Create Lavaan Code for Residual Variance at Wave 2 and Later of RICLPM
#'
#' @param Psi Residual variance-covariance matrix.
#' @param nameWComps Character vector with names of within-unit components.
#'
#' @return
#'
#' @examples
lavRes2Var <- function(Psi, nameWComps){
  lhs <- rhs <- c(nameWComps[-1, ])
  op <- rep("~~", times = (nK*(nT-1)))
  if(is.null(Psi)){
    pv <- con <- rep("", times = (nK*(nT-1)))
  } else {
    pv <- rep(diag(Psi), each = nK)
    con <- "*"
  }
  return(cbind(lhs, op, pv, con, rhs))
}

#' Create Lavaan Code for Residual Covariance at Wave 2 and Later of RICLPM
#'
#' @param Psi Residual variance-covariance matrix.
#' @param nameWComps Character vector with names of within-unit components.
#'
#' @return
#'
#' @examples
lavRes2Cov <- function(Psi, nameWComps){
  combnWComps <- t(apply(nameWComps[-1,], 1, combn, m = 2))
  lhs <- combnWComps[, 1]
  rhs <- combnWComps[, 2]
  op <- rep("~~", times = nrow(combnWComps))
  if(is.null(Psi)){
    pv <- con <- rep("", nrow(combnWComps))
  } else {
    pv <- c(Psi[lower.tri(Psi)])
    con <- "*"
  }
  return(cbind(lhs, op, pv, con, rhs))
}

#' Create Lavaan Code No Measurement Errors in RICLPM
#'
#' @param nameObs Character vector with names of observed variables.
#'
#' @return
#'
#' @examples
lavME <- function(nameObs){
  lhs <- rhs <- c(nameObs)
  op <- rep("~~", times = length(nameObs))
  pv <- rep("0", times = length(nameObs))
  con <- "*"
  return(cbind(lhs, op, pv, con, rhs))
}

