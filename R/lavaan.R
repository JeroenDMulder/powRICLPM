#' @title
#' Create lavaan syntax for data generation and model estimation
#'
#' @description
#' \code{create_lavaan()} creates a \pkg{lavaan} parameter table and model syntax for data generation and estimation of the specified model.
#'
#' @inheritParams powRICLPM
#' @param RI_var Numeric value denoting the random intercept variance.
#' @param RI_cov Numeric value denoting the covariance between the random intercepts.
#' @param Psi Variance-covariance matrix of within-unit residuals from wave 2 onwards.
#'
#' @return A list with contining information from a single experimental condition
#'
#' @details
#' \subsection{Naming conventions}{Details on the naming conventions can be found in the "Details" section of \code{\link{powRICLPM}}.}
#'
#' @noRd
create_lavaan <- function(cond,
                          Phi = NULL,
                          wSigma = NULL,
                          Psi = NULL,
                          constraints,
                          est_ME = FALSE,
                          skewness,
                          kurtosis,
                          alpha) {

  # Generate default variable names
  name_var <- LETTERS[1:2]

  # Create matrix of names for observed variable, within, and between components
  name_obs <- sapply(name_var, paste0, 1:cond[["time_points"]])
  name_within <- sapply(name_var, function(x) {
    paste0("w", x, 1:cond[["time_points"]])
  })
  name_RI <- paste0("RI_", name_var)

  # Create population parameter table
  pop_tab <- rbind(
    lav_RI(cond = cond, name_RI, name_obs),
    pop_RI_var(cond = cond, name_RI),
    pop_RI_cor(cond = cond, name_RI),
    lav_within(cond = cond, name_within, name_obs),
    pop_lagged(cond = cond, name_within, Phi),
    pop_within_var1(cond = cond, name_within),
    pop_within_cov1(cond = cond, name_within, wSigma),
    pop_within_var2(cond = cond, name_within, Psi),
    pop_within_cov2(cond = cond, name_within, Psi),
    pop_ME(cond = cond, name_obs)
  )
  rownames(pop_tab) <- NULL

  # Create estimation parameter table
  est_tab <- rbind(
    lav_RI(cond = cond, name_RI, name_obs),
    est_RI_var(cond = cond, name_RI),
    est_RI_cor(cond = cond, name_RI),
    lav_within(cond = cond, name_within, name_obs),
    est_lagged(cond = cond, name_within, Phi, constraints),
    est_within_var1(cond = cond, name_within, constraints),
    est_within_cov1(cond = cond, name_within, wSigma, constraints),
    est_within_var2(cond = cond, name_within, Psi, constraints),
    est_within_cov2(cond = cond, name_within, Psi, constraints),
    if (est_ME) {
      est_ME(cond = cond, name_obs, constraints)
    },
    if (constraints == "stationarity") {
      rbind(
        lav_within_cor(cond = cond),
        lav_stationarity(cond = cond)
      )
    }
  )
  rownames(est_tab) <- NULL

  # Create lavaan syntax
  pop_synt <- paste0( # Paste over parameters
    paste0( # Paste over columns
      pop_tab[, 1],
      pop_tab[, 2],
      pop_tab[, 3],
      pop_tab[, 4],
      pop_tab[, 5]
    ),
    collapse = "\n"
  )

  est_synt <- paste0( # Paste over parameters
    paste0( # Paste over columns
      est_tab[, 1],
      est_tab[, 2],
      est_tab[, 3],
      est_tab[, 4],
      est_tab[, 5]
    ),
    collapse = "\n"
  )

  # Create condition list with extra element space
  list(
    sample_size = cond[["sample_size"]],
    time_points = cond[["time_points"]],
    ICC = cond[["ICC"]],
    RI_var = cond[["RI_var"]],
    RI_cov = cond[["RI_cov"]],
    pop_synt = pop_synt,
    pop_tab = pop_tab,
    est_synt = est_synt,
    est_tab = est_tab,
    est_ME = est_ME,
    skewness = skewness,
    kurtosis = kurtosis,
    alpha = alpha,
    estimates = NA,
    uncertainty = NA,
    errors = NA,
    not_converged = NA,
    inadmissible = NA
  )
}

lav_RI <- function(cond, name_RI, name_obs) {
  lhs <- rep(name_RI, each = cond[["time_points"]])
  op <- rep("=~", times = 2 * cond[["time_points"]])
  pv <- rep("1", times = 2 * cond[["time_points"]])
  con <- rep("*", times = 2 * cond[["time_points"]])
  rhs <- c(unlist(name_obs))
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_RI_var <- function(cond, name_RI) {
  lhs <- rhs <- name_RI
  op <- "~~"
  con <- "*"
  pv <- cond[["RI_var"]]
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_RI_var <- function(cond, name_RI) {
  lhs <- rhs <- name_RI
  op <- "~~"
  con <- "*"
  pv <- paste0("start(", cond[["RI_var"]], ")")
  free <- TRUE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_RI_cor <- function(cond, name_RI) {
  lhs <- name_RI[1]
  rhs <- name_RI[2]
  op <- "~~"
  con <- "*"
  pv <- cond[["RI_cov"]]
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_RI_cor <- function(cond, name_RI) {
  lhs <- name_RI[[1]]
  rhs <- name_RI[[2]]
  op <- "~~"
  con <- "*"
  pv <- paste0("start(", cond[["RI_cov"]], ")")
  free <- TRUE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_within <- function(cond, name_within, name_obs) {
  lhs <- c(name_within)
  op <- "=~"
  pv <- "1"
  con <- "*"
  rhs <- c(name_obs)
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_lagged <- function(cond, name_within, Phi) {
  lhs <- rep(c(t(name_within))[-(1:2)], each = 2)
  op <- "~"
  con <- "*"
  pv <- c(t(Phi))
  free <- FALSE
  rhs <- c(apply(name_within[-cond[["time_points"]], ], 1, rep, times = 2))
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_lagged <- function(cond, name_within, Phi, constraints) {
  lhs <- rep(c(t(name_within))[-(1:2)], each = 2)
  op <- "~"
  con <- "*"
  if (constraints == "lagged" || constraints == "within" ||
      constraints == "stationarity") {
    pv <- c("alpha", "beta", "delta", "gamma") # Labels for constraints
  } else {
    pv <- paste0("start(", c(t(Phi)), ")")
  }
  free <- TRUE
  rhs <- c(apply(name_within[-cond[["time_points"]], ], 1, rep, times = 2))
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_within_var1 <- function(cond, name_within) {
  lhs <- rhs <- c(t(name_within[1, ]))
  op <- "~~"
  con <- "*"
  pv <- "1"
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_within_var1 <- function(cond, name_within, constraints) {
  lhs <- rhs <- c(t(name_within[1, ]))
  op <- "~~"
  con <- "*"
  if (constraints == "stationarity") {
    pv <- c("varA1", "varB1") # Label but freely estimate
  } else {
    pv <- paste0("start(1)")
  }
  free <- TRUE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_within_cov1 <- function(cond, name_within, wSigma) {
  lhs <- name_within[1, "A"]
  rhs <- name_within[1, "B"]
  op <- "~~"
  con <- "*"
  pv <- c(wSigma[lower.tri(wSigma)]) # Get covariances
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_within_cov1 <- function(cond, name_within, wSigma, constraints) {
  lhs <- name_within[1, 1]
  rhs <- name_within[1, 2]
  op <- "~~"
  con <- "*"
  if (constraints == "stationarity") { # Label
    pv <- "cor1"
  } else { # Freely estimate
    pv <- paste0("start(", c(wSigma[lower.tri(wSigma)]), ")")
  }
  free <- TRUE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_within_var2 <- function(cond, name_within, Psi) {
  lhs <- rhs <- c(name_within[-1, ])
  op <- "~~"
  con <- "*"
  pv <- rep(diag(Psi), each = (cond[["time_points"]] - 1))
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_within_var2 <- function(cond, name_within, Psi, constraints) {
  lhs <- rhs <- c(name_within[-1, ])
  op <- "~~"
  con <- "*"
  if (constraints == "residuals" || constraints == "within") { # Constrain over time
    pv <- rep(c("rvarA", "rvarB"), each = (cond[["time_points"]] - 1))
  } else if (constraints == "stationarity") {
    pv <- c(
      paste0("rvarA", 2:cond[["time_points"]]),
      paste0("rvarB", 2:cond[["time_points"]])
    )
  } else { # Freely estimate
    pv <- paste0("start(", rep(diag(Psi), each = (cond[["time_points"]] - 1)), ")")
  }
  free <- TRUE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_within_cov2 <- function(cond, name_within, Psi) {
  lhs <- name_within[-1, 1]
  rhs <- name_within[-1, 2]
  op <- "~~"
  con <- "*"
  pv <- c(Psi[lower.tri(Psi)])
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_within_cov2 <- function(cond, name_within, Psi, constraints) {
  lhs <- name_within[-1, 1]
  rhs <- name_within[-1, 2]
  op <- "~~"
  con <- "*"
  if (constraints == "residuals" || constraints == "within") { # Constrain over time
    pv <- "rcov"
  } else if (constraints == "stationarity") { # Label
    pv <- paste0("rcov", 2:cond[["time_points"]])
  } else { # Freely estimate
    pv <- paste0("start(", c(Psi[lower.tri(Psi)]), ")")
  }
  free <- TRUE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_ME <- function(cond, name_obs) {
  lhs <- rhs <- c(name_obs)
  op <- "~~"
  pv <- cond[["ME_var"]]
  free <- FALSE
  con <- "*"
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_ME <- function(cond, name_obs, constraints) {
  lhs <- rhs <- c(name_obs)
  op <- "~~"
  con <- "*"
  if (constraints == "stationarity" || constraints == "ME") { # Include constraints on ME
    pv <- "MEvar"
    free <- TRUE
  } else { # Freely estimate ME
    pv <- paste0("start(", cond[["ME_var"]], ")")
    free <- TRUE
  }
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_ME0 <- function(cond, name_obs) {
  lhs <- rhs <- c(name_obs)
  op <- "~~"
  pv <- 0
  free <- FALSE
  con <- "*"
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_within_cor <- function(cond) {
  lhs <- paste0("cor", 2:cond[["time_points"]])
  op <- ":="
  pv <- con <- ""
  rhs1 <- paste0("alpha*delta + beta*gamma + alpha*gamma*cor", 1:(cond[["time_points"]] - 1))
  rhs2 <- paste0(" + beta*delta*cor", 1:(cond[["time_points"]] - 1))
  rhs3 <- paste0(" + rcov", 2:cond[["time_points"]])
  rhs <- paste0(rhs1, rhs2, rhs3)
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_stationarity <- function(cond) {
  lhs <- c(
    paste0("rvarA", 2:cond[["time_points"]]),
    paste0("rvarB", 2:cond[["time_points"]])
  )
  op <- "=="
  rhs <- c(
    paste0("1 - (alpha^2 + beta^2 + 2*alpha*beta*cor", 1:(cond[["time_points"]] - 1), ")"),
    paste0("1 - (delta^2 + gamma^2 + 2*delta*gamma*cor", 1:(cond[["time_points"]] - 1), ")")
  )
  pv <- con <- ""
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}
