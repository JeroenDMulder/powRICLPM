#' Power Analysis for the RI-CLPM
#'
#' @import doSNOW
#'
#' @param nK Number of variables
#' @param nT Number of time points
#' @param nN Sample sizes at which to evaluate power. This can be a single integer or an integer vector.
#' @param Phi Matrix of autoregressive and cross-lagged effects in the population. Columns are predictors, rows are outcomes.
#' @param wSigma Correlation matrix of within-unit components
#' @param RICov Vector of correlations between random intercepts. The elements should in ascending order.
#' @param RIVarProp Proportion of variance at the between-unit level. This can be a single integer or an integer vector if the power should be evaluated at multiple proportions.
#' @param nReps Number of repetitions for simulations
#' @param seed Number for initialization of pseudorandom processes in the function.
#' @param nWorkers Number of cores to use for the power analysis. Executing the analysis using multiple cores can significantly reduce computation time
#' @param Mplus Return syntax for performing the specified power analysis in Mplus
#'
#' @return
#' @export
#'
#' @examples
#' nN = 500
#' nK = 2
#' nT = 3
#' Phi = matrix(c(.5, .1, .4, .5), ncol = nK, byrow = T)
#' wSigma = matrix(c(1 , .3, .3, 1) , ncol = nK, byrow = T)
#' RICov = c(.3)
#' RIVarProp = .5
#' nReps = 100
#' seed = 123
#' nWorkers = 4
#' test <- powerRICLPM(nK = nK, nT = nT, nN = nN, RIVarProp  = RIVarProp, RICov = RICov, Phi = Phi, wSigma = wSigma, nReps = nReps, nWorkers = 4, savePath = "./tests/mplus", saveDat = T)
#' test2 <- powerRICLPM(nK = nK, nT = nT, nN = nN, RIVarProp  = RIVarProp, RICov = RICov, Phi = Phi, wSigma = wSigma, nReps = nReps, nWorkers = 4, Mplus = T)

powerRICLPM <- function(nK, nT, nN, RIVarProp, RICov, Phi, wSigma, nReps, nWorkers = 1, Mplus = F, saveDat = F, savePath = "."){

  ##################
  # CHECK INPUT ----
  ##################

  # Check argument nK
  if(any(nK < 1)) stop("nK should be larger than 0.")
  # Check argument nT
  if(any(nT < 3)) stop("The RICLPM is not identified with fewer than 3 time points.")
  # Check argument nN
  if(!is.numeric(nN)) stop("nN should be of type numeric.")
  if(any(nN < 0)) stop("The sample size(s) should be positive.")
  # Check argument wSigma
  if(any(diag(wSigma) != 1)) stop("wSigma should be a correlation matrix with 1's on the diagonal.")
  # Check argument RIVarProp
  if(RIVarProp >= 1 || RIVarProp <= 0) stop("The proportion of variance at the between-unit level should be higher than 0 and smaller than 1.")
  # Check argument Phi
  PhiEigVal<- eigen(Phi, only.values=T)$values # Compute eigenvalues
  if(is.complex(PhiEigVal)){
    if(any(Re(PhiEigVal) < 0) | any(Im(PhiEigVal) < 0)) stop("Phi is not positive definite (complex eigenvalues).")
    if(any( sqrt( (Re(PhiEigVal)^2) + (Im(PhiEigVal)^2) ) > 1) ) stop("Complex eigenvalues are not within unit circle.")
  } else {
    if(any(PhiEigVal < 0)) stop("Phi is not positive definite.")
    if(any(PhiEigVal >= 1)) stop("Eigenvalues are not within unit circle.")
  }

  # Compute residual variance-covariance matrix Psi and check for positive definiteness
  Psi <- getResVar(Phi, wSigma)
  if(any(eigen(Psi, only.values=T)$values < 0)) stop("The variance-covariance matrix of the residuals Psi is not positive definite.")

  # Create combinations of simulation conditions
  design <- expand.grid("nN" = nN, "nT" = nT, "RIVarProp" = RIVarProp)
  nD <- nrow(design)

  # Check saveData-options
  if(nD > 1 & saveDat) stop("The current version of powRICLPM cannot save data for multiple power scenarios.")

  ############
  # MPLUS ----
  ############

  # Create Mplus syntax
  if(Mplus){
    popSynt <- plyr::alply(.data = design
                           , .margins = 1
                           , .fun = function(x) getMplusSynt(nK = nK, nT = x[["nT"]], nN = x["nN"], Phi = Phi, Psi = Psi, wSigma = wSigma, RICov = RICov, RIVarProp = x["RIVarProp"], nReps = nReps, seed = seed))
    return(popSynt)
  }

  #######################
  # SIMULATION SETUP ----
  #######################

  # Create lavaan parameter tables for generating data
  popTab <- plyr::alply(.data = design
                        , .margins = 1
                        , .fun = function(x) getLavMod(nK = nK, nT = x[["nT"]], Phi = Phi, wSigma = wSigma, RICov = RICov, RIVarProp = x[["RIVarProp"]], Psi = Psi))

  # Create lavaan syntax for generating data
  popSynt <- lapply(popTab, function(x) paste0(paste0(x[,"lhs"], x[,"op"], x[,"pv"], x[,"con"], x[,"rhs"])
                                               , collapse = "\n"))

  # Create lavaan parameter tables and syntax for estimation
  modTab <- plyr::alply(design, 1, function(x) getLavMod(nK = nK, nT = x[["nT"]]))
  modSynt <- lapply(modTab, function(x) paste0(paste0(x[,"lhs"], x[,"op"], x[,"pv"], x[,"con"], x[,"rhs"])
                                               , collapse = "\n"))
  browser()

  ##################
  # SIMULATIONS ----
  ##################

  # Initialize parallelization
  set.seed(seed)
  cl <- snow::makeSOCKcluster(nWorkers)
  pb <- utils::txtProgressBar(max = nD, style = 3)
  progress <- function(x){utils::setTxtProgressBar(pb, x)}
  opts <- list(progress = progress)
  doSNOW::registerDoSNOW(cl)

  res <- foreach::foreach(d = 1:nD, .options.snow = opts) %dopar% { # Results

    # Simulation conditions
    n <- design[[d,"nN"]] # Sample size
    nPar <- # Number of parameters
      sum(factorial(1 + nK) / (2*(nK - 1)) * (nT + 1), # Number of (co)variances within- and between-level
          nK^2*(nT - 1)) # Number of lagged effects
    pop <- popSynt[[d]] # Population model syntax
    mod <- modSynt[[d]] # Estimation model syntax
    iFree <- as.data.frame(modTab[[d]])$pv == "" # Indicator freely estimated parameters
    pv <- as.numeric(popTab[[d]]$pv)[iFree] # Population values
    par <- paste0(popTab[[d]]$lhs[iFree], popTab[[d]]$op[iFree], popTab[[d]]$rhs[iFree])

    # Allocate memory for results
    coefs <- SEs <- low95 <- up95 <- matrix(NA, nrow = nPar, ncol = nReps)
    sigs <- cover95 <- matrix(F, nrow = nPar, ncol = nReps)

    # Initialize fail counter and index
    nFail <- 0
    iFail <- c()

    for(r in 1:nReps){
      # Generate data
      dat <- lavaan::simulateData(pop, sample.nobs = n)

      # Save data
      if(saveDat){
        data.table::fwrite(dat, file = paste0(savePath, "/rep", r, ".dat"), sep = "\t", col.names = F, row.names = F, na = "-999")
      }

      # Fit model
      fit <- lavaan::lavaan(mod, dat)

      # Check non-convergence
      if(!lavaan::inspect(fit, "post.check")){ # T if negative variances are found or non-positive definite matrices
        nFail <- nFail + 1
        iFail <- c(iFail, r)
        warning(paste0("Negative variance(s) or non-positive definite latent and/or observed variable variance-covariance matrix: rep ", r, " in nN = ", design[d, "nN"], ", nT = ", design[d, "nT"], ", and RIVarProp = ", design[d, "RIVarProp"]))
        next # Go to next iteration
      }

      # Get estimates
      coefs[, r] <- lavaan::coef(fit) # Save coefficients
      SEs[, r] <- lavaan::parameterEstimates(fit, remove.nonfree = T)$se # Save standard errors
      sigs[, r] <- lavaan::parameterEstimates(fit, remove.nonfree = T)$pvalue < .05
      low95[, r] <- lavaan::parameterEstimates(fit, remove.nonfree = T)$ci.lower
      up95[, r] <- lavaan::parameterEstimates(fit, remove.nonfree = T)$ci.upper
    }

    # Create and save repList
    if(saveDat){
      nameReps <- paste0("rep", 1:nReps, ".dat")[-iFail]
      data.table::fwrite(as.list(nameReps), file = paste0(savePath, "/nameReps.dat"), sep = "\n", col.names = F, row.names = F)
    }

    # Summarize results
    nSuc <- nReps - nFail
    avg <- rowMeans(coefs, na.rm = T)
    stdDev <- apply(coefs, 1, sd, na.rm = T)
    SEAvg <- rowMeans(SEs, na.rm = T)
    mse <- rowMeans((coefs - pv)^2, na.rm = T)
    cover95 <- rowSums(pv > low95 & pv < up95, na.rm = T) / nSuc
    sig <- rowSums(sigs, na.rm = T) / nSuc

    # Bind results of this particular scenario
    dRes <- cbind(par,
                  round(cbind(pv, avg, stdDev, SEAvg, mse, cover95, sig), digits = 4))
    list(design = design[d, ], fail = list(nFail, iFail), power = dRes)
  }
  snow::stopCluster(cl)

  #######################
  # COMPLETE RESULTS ----
  #######################
  return(res)
}

