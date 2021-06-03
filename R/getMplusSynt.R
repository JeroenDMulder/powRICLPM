#' Create Mplus Syntax for RICLPM Power Analysis
#'
#' @param nK Number of variables.
#' @param nT Number of time points.
#' @param nN Sample size.
#' @param Phi Matrix of autoregressive and cross-lagged effects in the population.
#' @param Psi Residual variance-covariance matrix at within-unit level.
#' @param wSigma Correlation matrix of within-unit components.
#' @param RICov Vector of correlations between random intercepts. The elements should in ascending order.
#' @param RIVarProp Proportion of variance at the between-unit level.
#' @param nameVar Character vector of variable names.
#' @param nReps Number of Monte Carlo repetitions.
#' @param seed Number for initialization of pseudorandom processes during power analysis (e.g. for generation of data).
#'
#' @return
#' @export
#'
#' @examples
#' nN = 300
#' nK = 2
#' nT = 3
#' Phi = matrix(c(.5, .1, .4, .5), ncol = nK, byrow = T)
#' wSigma = matrix(c(1 , .3, .3, 1) , ncol = nK, byrow = T)
#' RICov = c(.3)
#' RIVarProp = c(.25, .5)
#' nReps = 10
#' seed = 123
getMplusSynt <- function(nK = 2, nT, nN = nN, Phi = NULL, Psi = NULL, wSigma = NULL, RICov = NULL, RIVarProp = NULL, nameVar = NULL, nReps = 1000, seed = NULL){

  # Options
  if(is.null(nameVar)) nameVar <- LETTERS[1:nK] # Generate default variable names

  # Global
  nameObs <- matrix(sapply(nameVar, paste0, 1:nT), nrow = nT, ncol = nK) # Create matrix of observed variable names
  nameWComps <- matrix(paste0("w", nameObs), nrow = nT, ncol = nK) # Create matrix of within-component names
  nameRI <- paste0("RI_", nameVar) # Create matrix of between-component names

  # Create TITLE:, ANALYSIS:, MONTECARLO:
  preamble <- MplusPre(nK = nK, nT = nT, nN = nN, nameObs = nameObs, nReps = nReps)

  # Create MODEL POPULATION:
  MplusTabPop <- as.data.frame(
    rbind.data.frame(MplusRI(nK = nK, nT = nT, nameRI = nameRI, nameObs = nameObs),
          MplusRIVar(nK = nK, nameRI = nameRI, RIVarProp = RIVarProp),
          MplusRICov(nameRI = nameRI, RICov = RICov),
          MplusWComp(nameWComps = nameWComps, nameObs = nameObs),
          MplusWEff(nK, nT, nameWComps, Phi),
          MplusRes1Var(nameWComps, wSigma),
          MplusRes1Cov(nameWComps, wSigma),
          MplusRes2Var(Psi, nameWComps),
          MplusRes2Cov(Psi, nameWcomps),
          MplusME(nameObs)
          , stringsAsFactors = F)
    )
  MplusTabPop$end <- ";"

  # Create MODEL:
  MplusTabMod <- as.data.frame(
    rbind.data.frame(MplusRI(nK = nK, nT = nT, nameRI = nameRI, nameObs = nameObs),
                     MplusRIVar(nK = nK, nameRI = nameRI),
                     MplusRICov(nameRI = nameRI),
                     MplusWComp(nameWComps = nameWComps, nameObs = nameObs),
                     MplusWEff(nK, nT, nameWComps, Phi, mod = T),
                     MplusRes1Var(nameWComps = nameWComps),
                     MplusRes1Cov(nameWComps = nameWComps),
                     MplusRes2Var(nameWComps = nameWComps),
                     MplusRes2Cov(nameWcomps = nameWComps),
                     MplusME(nameObs)
                     , stringsAsFactors = F))
  MplusTabMod$end <- ";"

  # Merge table into model syntax
  syntax <- paste0(preamble,
                   paste0(paste0(MplusTabPop[,1], MplusTabPop[,2], MplusTabPop[,3], MplusTabPop[,4], MplusTabPop[,5])
                          , collapse = "\n"),
                   "\n\nMODEL:\n ",
                   paste0(paste0(MplusTabMod[,1], MplusTabMod[,2], MplusTabMod[,3], MplusTabMod[,4], MplusTabMod[,5])
                          , collapse = "\n"),
                   "\n\nOUTPUT:\n TECH1 SAMPSTAT;"
                  , collapse = "\n")

  return(syntax)
}


#' Create Mplus Preamble for RICLPM Power Analysis
#'
#' @param nK Number of variables.
#' @param nT Number of time points.
#' @param nN Sample size.
#' @param nameObs Character vector of observed variable names.
#' @param nReps Number of Monte Carlo repetitions.
#' @param seed Number for initialization of pseudorandom processes during power analysis (e.g. for generation of data).
#'
#' @return
#' @export
#'
#' @examples
MplusPre <- function(nK, nT, nN, nameObs, nReps, seed = 1234){
  TITLE <- paste0("TITLE:\n Power analysis RICLPM with K = ", nK, ", nT = ", nT, ", N = ", nN, "\n\n")
  MONTECARLO <- paste0("MONTECARLO:\n NAMES = ", paste(nameObs, collapse = " "), ";\n NOBSERVATIONS = ", nN, ";\n NREPS = ", nReps, ";\n SEED = ", seed, "; \n\n")
  ANALYSIS <- paste0("ANALYSIS:\n MODEL = NOCOV;\n\nMODEL POPULATION:\n")
return(paste0(TITLE, MONTECARLO, ANALYSIS))
}

#' Create Mplus Syntax for Random Intercepts of RICLPM
#'
#' @param nT Number of time points
#' @param nameRI Character vector with names of the random intercepts
#' @param nameObs Character vector with names of the observed variables
#'
#' @return
#'
#' @examples
MplusRI <- function(nK, nT, nameObs, nameRI){
  lhs <- rep(nameRI, each = nT)
  op <- " BY "
  con <- "@1"
  rhs <- c(nameObs)
  return(cbind.data.frame(lhs, op, rhs, con, stringsAsFactors = F))
}

#' Create Mplus Syntax for Variance of Random Intercepts of RICLPM
#'
#' @param nK Number of variables
#' @param nameRI Character vector with names of the random intercepts.
#' @param RIVarProp Proportion of variance at the between-unit level.
#'
#' @return
#'
#' @examples
MplusRIVar <- function(nK, RIVarProp = NULL, nameRI){
  lhs <- nameRI
  op <- rhs <- rep("", times = nK) # Empty
  if(is.null(RIVarProp)){
    con <- ""
  } else {
    con <- rep(paste0("@", 1 / (1 - RIVarProp) - 1), times = nK)
  }
  return(cbind.data.frame(lhs, op, rhs, con, stringsAsFactors = F))
}

#' Create Mplus Syntax for Covariance of Random Intercepts of RICLPM
#'
#' @param nameRI Character vector with names of the random intercepts.
#' @param RICov Integer vector covariances between the random intercepts.
#'
#' @return
#'
#' @examples
MplusRICov <- function(nameRI, RICov = NULL){
  combnRI <- t(combn(nameRI, 2))
  lhs <- combnRI[, 1]
  op <- rep(" WITH ", times = nrow(combnRI))
  if(is.null(RICov)){
    con <- ""
  } else {
    con <- paste0("@", RICov)
  }
  rhs <- combnRI[, 2]
  return(cbind.data.frame(lhs, op, rhs, con, stringsAsFactors = F))
}

#' Create Mplus Syntax for Within Components of RICLPM
#'
#' @param nameWComps Character vector with names of within-unit components.
#' @param nameObs Character vector with names of observed variables.
#'
#' @return
#'
#' @examples
MplusWComp <- function(nameWComps, nameObs){
  lhs <- c(nameWComps)
  op <- rep(" BY ", times = length(nameWComps))
  con <- rep("@1", times = length(nameWComps))
  rhs <- c(nameObs)
  return(cbind.data.frame(lhs, op, rhs, con, stringsAsFactors = F))
}

#' Create Mplus Syntax for Within Lagged Effects of RICLPM
#'
#' @param nK Number of variables.
#' @param nT Number of time points.
#' @param nameWComps Character vector with names of within-unit components.
#' @param Phi Matrix of autoregressive and cross-lagged effects in the population.
#'
#' @return
#'
#' @examples
MplusWEff <- function(nK, nT, nameWComps, Phi = NULL, mod = F){
  lhs <- rep(c(t(nameWComps))[-(1:nK)], each = nK) # Create vector with outcomes
  op <- rep(" ON ", times = nK^2)
  if(mod){
    con <- paste0("*", rep(c(Phi), times = (nT - 1)))
  } else {
    con <- paste0("@", rep(c(Phi), times = (nT - 1)))
  }
  rhs <- c(apply(nameWComps[-nT,], 1, rep, times = nK))
  return(cbind.data.frame(lhs, op, rhs, con, stringsAsFactors = F))
}

#' Create Mplus Syntax for Residual Variance at Wave 1 of RICLPM
#'
#' @param nameWComps Character vector with names of within-unit components.
#' @param wSigma Correlation matrix of within-unit components.
#'
#' @return
#'
#' @examples
MplusRes1Var <- function(nameWComps, wSigma = NULL){
  lhs <- nameWComps[1, ]
  op <- rhs <- ""
  if(is.null(wSigma)){
    con <- ""
  } else {
    con <- rep("@1", times = nK)
  }
  return(cbind.data.frame(lhs, op, rhs, con, stringsAsFactors = F))
}

#' Create Mplus Syntax for Residual Covariance at Wave 1 of RICLPM
#'
#' @param nameWComps Character vector with names of within-unit components.
#' @param wSigma Correlation matrix of within-unit components.
#'
#' @return
#'
#' @examples
MplusRes1Cov <- function(nameWComps, wSigma = NULL){
  combnWComps <- t(combn(nameWComps[1, ], 2))
  lhs <- combnWComps[,1]
  rhs <- combnWComps[,2]
  op <- rep(" WITH ", times = nrow(combnWComps))
  if(is.null(wSigma)){
    con <- ""
  } else {
    resCov <- c(wSigma[lower.tri(wSigma)]) # Get covariances
    con <- paste0("@", resCov)
  }
  return(cbind.data.frame(lhs, op, rhs, con, stringsAsFactors = F))
}

#' Create Mplus Syntax for Residual Variance at Wave 2 and Later of RICLPM
#'
#' @param Psi Residual variance-covariance matrix.
#' @param nameWComps Character vector with names of within-unit components.
#'
#' @return
#'
#' @examples
MplusRes2Var <- function(Psi = NULL, nameWComps){
  lhs <- c(nameWComps[-1, ])
  op <- rhs <- ""
  if(is.null(Psi)){
    con <- ""
  } else {
    con <- rep(paste0("@", diag(Psi)), each = nT - 1)
  }
  return(cbind.data.frame(lhs, op, rhs, con, stringsAsFactors = F))
}

#' Create Mplus Syntax for Residual Covariance at Wave 2 and Later of RICLPM
#'
#' @param Psi Residual variance-covariance matrix.
#' @param nameWComps Character vector with names of within-unit components.
#'
#' @return
#'
#' @examples
MplusRes2Cov <- function(Psi = NULL, nameWcomps){
  combnWComps <- t(apply(nameWComps[-1,], 1, combn, m = 2))
  lhs <- combnWComps[, 1]
  rhs <- combnWComps[, 2]
  op <- rep(" WITH ", times = nrow(combnWComps))
  if(is.null(Psi)){
    con <- ""
  } else {
    resCov <- c(Psi[lower.tri(Psi)])
    con <- paste0("@", resCov)
  }
  return(cbind.data.frame(lhs, op, rhs, con, stringsAsFactors = F))
}

#' Create Mplus Syntax No Measurement Errors in RICLPM
#'
#' @param nameObs Character vector with names of observed variables.
#'
#' @return
#'
#' @examples
MplusME <- function(nameObs){
  lhs <- c(nameObs)
  op <- rhs <- ""
  con <- rep("@0", times = length(nameObs))
  return(cbind.data.frame(lhs, op, rhs, con, stringsAsFactors = F))
}

