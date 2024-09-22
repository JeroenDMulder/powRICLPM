#' @title
#' Create Mplus Syntax for RI-CLPM Power Analysis
#'
#' @description
#' \code{create_Mplus()} creates Mplus model syntax for a Monte Carlo power analysis for the random intercept cross-lagged panel model (RI-CLPM).
#'
#' @param condition A list, containing values for factors that define an experimental condition (i.e., sample size, number of time points, ICC, and reliability), as well as population values for parameters that depend on these (e.g., the random intercept variances, and covariance between them is a function of `ICC`).
#'
#' \subsection{Naming conventions}{Details on the naming conventions can be found in the "Details" section of \code{\link{powRICLPM}}.}
#'
#' @return The `condition` list, extended with a Mplus syntax for Monte Carlo power analysis.
#'
#' @noRd
create_Mplus <- function(condition, reps, seed) {

  # Generate default variable names
  name_var <- LETTERS[1:2]

  # Create matrix of names for observed variable, within, and between components
  name_obs <- sapply(name_var, paste0, 1:condition[["time_points"]])
  name_within <- sapply(name_var, function(x) {
    paste0("w", x, 1:condition[["time_points"]])
  })
  name_RI <- paste0("RI_", name_var)

  # Create TITLE:, ANALYSIS:, MONTECARLO:
  PREAMBLE <- Mplus_pre(condition, name_obs, reps = reps, seed = seed)

  # Create MODEL POPULATION:
  Mplus_population <- rbind(
    Mplus_RI(condition, name_obs, name_RI),
    Mplus_RI_var(condition, estimation = FALSE, name_RI),
    Mplus_RI_cov(condition, estimation = FALSE, name_RI),
    Mplus_within(condition, name_obs, name_within),
    Mplus_lagged(condition, estimation = FALSE, name_within),
    Mplus_within_var1(condition, estimation = FALSE, name_within),
    Mplus_within_cov1(condition, estimation = FALSE, name_within),
    Mplus_within_var2(condition, estimation = FALSE, name_within),
    Mplus_within_cov2(condition, estimation = FALSE, name_within),
    Mplus_pop_ME(condition, name_obs),
    stringsAsFactors = F
  )

  # Add Mplus command end
  Mplus_population$end <- ";"

  # Merge parameter table into model syntax
  MODEL_POPULATION <- paste0( # Paste over parameters
    paste0(
      Mplus_population[, 1], # Paste over columns
      Mplus_population[, 2],
      Mplus_population[, 3],
      Mplus_population[, 4],
      Mplus_population[, 5]
    ),
    collapse = "\n  "
  )

  # Create MODEL:
  Mplus_estimation <- rbind(
    Mplus_RI(condition, name_obs, name_RI),
    Mplus_RI_var(condition, estimation = TRUE, name_RI),
    Mplus_RI_cov(condition, estimation = TRUE, name_RI),
    Mplus_within(condition, name_obs, name_within),
    Mplus_lagged(condition, estimation = TRUE, name_within),
    Mplus_within_var1(condition, estimation = TRUE, name_within),
    Mplus_within_cov1(condition, estimation = TRUE, name_within),
    Mplus_within_var2(condition, estimation = TRUE, name_within),
    Mplus_within_cov2(condition, estimation = TRUE, name_within),
    Mplus_estimate_ME(condition, name_obs = name_obs),
    stringsAsFactors = FALSE
  )

  # Add Mplus command end
  Mplus_estimation$end <- ";"

  # Merge parameter table into model syntax
  MODEL <- paste0(
    "\n\nMODEL:\n  ",
    paste0( # Paste over parameters
      paste0(
        Mplus_estimation[, 1], # Paste over columns
        Mplus_estimation[, 2],
        Mplus_estimation[, 3],
        Mplus_estimation[, 4],
        Mplus_estimation[, 5]
      ),
      collapse = "\n  "
    )
  )

  # Delete rownames
  row.names(Mplus_population) <- row.names(Mplus_estimation) <- NULL

  if (condition$constraints == "stationarity") {
    # Create MODEL CONSTRAINT:
    Mplus_constraint <- rbind(
      Mplus_new(condition),
      Mplus_within_cor(condition),
      Mplus_stationarity(condition),
      stringsAsFactors = FALSE
    )
    # Add Mplus command end
    Mplus_constraint$end <- ";"

    # Delete row names
    row.names(Mplus_constraint) <- NULL

    # Merge parameter table into model syntax
    MODEL_CONSTRAINT <- paste0(
      "\n\nMODEL CONSTRAINT:\n  ",
      paste0( # Paste over parameters
        paste0(
          Mplus_constraint[, 1],
          Mplus_constraint[, 2],
          Mplus_constraint[, 3],
          Mplus_constraint[, 4],
          Mplus_constraint[, 5]
        ),
        collapse = "\n  "
      )
    )
  } else {
    MODEL_CONSTRAINT <- NULL
  }

  OUTPUT <- "\n\nOUTPUT:\n  TECH1 SAMPSTAT;"

  # Combine all Mplus commands
  Mplus_syntax <- paste0(PREAMBLE,
    MODEL_POPULATION,
    MODEL,
    MODEL_CONSTRAINT,
    OUTPUT,
    collapse = "\n"
  )

  # Create condition list with extra element space
  list(
    sample_size = condition[["sample_size"]],
    time_points = condition[["time_points"]],
    ICC = condition[["ICC"]],
    reliability = condition[["reliability"]],
    RI_var = condition[["RI_var"]],
    RI_cov = condition[["RI_cov"]],
    Mplus_synt = Mplus_syntax,
    estimate_ME = condition[["estimate_ME"]],
    skewness = condition[["skewness"]],
    kurtosis = condition[["kurtosis"]],
    significance_criterion = condition[["significance_criterion"]],
    estimates = NA,
    MCSEs = NA,
    reps = NA,
    condition_id = condition[["condition_id"]]
  )

}

Mplus_pre <- function(condition, name_obs, reps, seed) {
  # Create TITLE command
  TITLE <- paste0(
    "TITLE:\n  Power analysis RI-CLPM with N = ", condition$sample_size,
    ", T = ", condition$time_points,
    ", ICC = ", condition$ICC,
    ", reliability = ", condition$reliability, "\n\n"
  )

  # Create MONTECARLO command
  MONTECARLO <- paste0(
    "MONTECARLO:\n  NAMES = ", paste(unlist(name_obs), collapse = " "),
    ";\n  NOBSERVATIONS = ", condition$sample_size,
    ";\n  NREPS = ", reps,
    ";\n  SEED = ", seed,
    ";\n\n"
  )

  # Create ANALYSIS command
  ANALYSIS <- paste0("ANALYSIS:\n  MODEL = NOCOV;\n\nMODEL POPULATION:\n  ")

  return(paste0(TITLE, MONTECARLO, ANALYSIS))
}

Mplus_RI <- function(condition, name_obs, name_RI) {
  lhs <- rep(name_RI, each = condition$time_points)
  op <- " BY "
  con <- "@1"
  rhs <- c(unlist(name_obs))
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_RI_var <- function(condition, estimation = FALSE, name_RI) {
  lhs <- name_RI
  op <- rhs <- rep("", times = 2)
  if (estimation) {
    con <- rep(paste0("*", condition$RI_var), times = 2)
  } else {
    con <- rep(paste0("@", condition$RI_var), times = 2)
  }
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_RI_cov <- function(condition, estimation = FALSE, name_RI) {

  # Create combinations of random intercept factors
  combn_RI <- t(utils::combn(name_RI, 2))

  # Create syntax (parameter table) elements
  lhs <- combn_RI[, 1]
  op <- rep(" WITH ", times = nrow(combn_RI))
  if (estimation) {
    con <- paste0("*", condition$RI_cov)
  } else {
    con <- paste0("@", condition$RI_cov)
  }
  rhs <- combn_RI[, 2]
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_within <- function(condition, name_obs, name_within) {
  lhs <- c(unlist(name_within))
  op <- rep(" BY ", times = length(name_within))
  con <- rep("@1", times = length(name_within))
  rhs <- c(unlist(name_obs))
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_lagged <- function(condition, estimation = FALSE, name_within) {
  # Create vector with outcomes
  lhs <- rep(c(t(name_within))[-(1:2)], each = 2)
  op <- " ON "

  if (estimation) { # Estimation
    if (
      condition$constraints == "none" ||
      condition$constraints == "residuals" ||
      condition$constraints == "ME"
    ) { # Freely estimate
      con <- paste0("*", rep(unlist(condition$Phi), times = (condition$time_points - 1)))
    } else if (condition$constraints == "lagged" ||
      condition$constraints == "within" ||
      condition$constraints == "stationarity") { # Constrain over time
      con <- c("(alpha)", "(beta)", "(delta)", "(gamma)")
    }
  } else {
    con <- paste0("@", rep(unlist(condition$Phi), times = (condition$time_points - 1)))
  }

  # Create vector with predictors
  rhs <- c(apply(name_within[-condition$time_points, ], 1, rep, times = 2))

  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_within_var1 <- function(condition, estimation = FALSE, name_within) {
  lhs <- name_within[1, ]
  op <- rhs <- ""
  if (estimation) { # Estimation
    if (condition$constraints == "stationarity") { # Label but freely estimate
      con <- c("(varA1)", "(varB1)")
    } else if (
      condition$constraints == "none" ||
      condition$constraints == "lagged" ||
      condition$constraints == "residuals" ||
      condition$constraints == "within" ||
      condition$constraints == "ME"
    ) {
      con <- rep("*1", times = 2)
    }
  } else { # Data generation
    con <- rep("@1", times = 2)
  }
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_within_cov1 <- function(condition, estimation = FALSE, name_within) {
  lhs <- unlist(name_within[1, 1])
  rhs <- unlist(name_within[1, 2])
  op <- " WITH "

  if (estimation) {
    if (condition$constraints == "stationarity") { # Label but freely estimate
      con <- "(cor1)"
    } else if (
      condition$constraints == "none" ||
      condition$constraints == "lagged" ||
      condition$constraints == "residuals" ||
      condition$constraints == "within" ||
      condition$constraints == "ME"
    ) { # Freely estimate
      con <- paste0("*", condition$within_cor)
    }
  } else { # Data generation
    con <- paste0("@", condition$within_cor)
  }
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_within_var2 <- function(condition, estimation = FALSE, name_within) {
  lhs <- c(unlist(name_within[-1, ]))
  op <- rhs <- ""

  if (estimation) { # Estimation
    if (
      condition$constraints == "none" ||
      condition$constraints == "lagged" ||
      condition$constraints == "ME"
    ) { # Freely estimate
      con <- rep(paste0("*", diag(condition$Psi[[1]])), each = condition$time_points - 1)
    } else if (
      condition$constraints == "residuals" ||
      condition$constraints == "within"
    ) { # Constrain over time
      con <- rep(paste0("(rvar", LETTERS[1:2], ")"), each = condition$time_points - 1)
    } else if (condition$constraints == "stationarity") {
      con <- c(
        paste0("(rvarA", 2:condition$time_points, ")"),
        paste0("(rvarB", 2:condition$time_points, ")")
      )
    }
  } else { # Data generation
    con <- rep(paste0("@", diag(condition$Psi[[1]])), each = condition$time_points - 1)
  }
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_within_cov2 <- function(condition, estimation = FALSE, name_within) {
  # Create within-component combinations
  combn_within <- t(apply(name_within[-1, ], 1, utils::combn, m = 2))

  # Create syntax (parameter table) elements
  lhs <- combn_within[, 1]
  rhs <- combn_within[, 2]
  op <- rep(" WITH ", times = nrow(combn_within))

  # Select residual covariances
  resCov <- c(condition$Psi[[1]][lower.tri(condition$Psi[[1]])])

  # Estimation
  if (estimation) {
    if (
      condition$constraints == "none" ||
      condition$constraints == "lagged" ||
      condition$constraints == "ME"
    ) { # Freely estimate
      con <- paste0("*", resCov)
    } else if (condition$constraints == "residuals" ||
      condition$constraints == "within") { # Constrain over time
      con <- "(rcov)"
    } else if (condition$constraints == "stationarity") {
      con <- paste0("(rcov", 2:condition$time_points, ")")
    }

    # Data generation
  } else {
    con <- paste0("@", resCov)
  }
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_pop_ME <- function(condition, name_obs) {
  lhs <- c(unlist(name_obs))
  op <- rhs <- ""
  con <- paste0("@", condition[["ME_var"]])
  return(cbind.data.frame(lhs, op, rhs, con,
                          stringsAsFactors = FALSE
  ))
}

Mplus_estimate_ME <- function(condition, name_obs) {
  lhs <- c(unlist(name_obs))
  op <- rhs <- ""
  if (!condition[["estimate_ME"]]) {
    con <- "@0"
  } else if (
    condition[["constraints"]] == "stationarity" ||
    condition[["constraints"]] == "ME"
  ) {
    label <- rep(c("MEvarA", "MEvarB"), each = condition[["time_points"]])
    con <- paste0("*", condition[["ME_var"]], " (", label, ")")
  } else { # Freely estimate
    con <- paste0("*", condition[["ME_var"]])
  }
  return(cbind.data.frame(lhs, op, rhs, con,
                          stringsAsFactors = FALSE
  ))
}

Mplus_new <- function(condition) {
  lhs <- paste0("NEW(cor", 2:condition$time_points, ")")
  op <- rhs <- con <- ""
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_within_cor <- function(condition) {
  lhs <- paste0("cor", 2:condition$time_points)
  op <- " = "
  rhs1 <- paste0("alpha*delta + beta*gamma + alpha*gamma*cor", 1:(condition$time_points - 1))
  rhs2 <- paste0(" + beta*delta*cor", 1:(condition$time_points - 1))
  rhs3 <- paste0(" + rcov", 2:condition$time_points)
  rhs <- paste0(rhs1, rhs2, rhs3)
  con <- ""
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_stationarity <- function(condition) {
  lhs <- c(
    paste0("rvarA", 2:condition$time_points),
    paste0("rvarB", 2:condition$time_points)
  )
  op <- "="
  rhs <- c(
    paste0("1 - (alpha^2 + beta^2 + 2*alpha*beta*cor", 1:(condition$time_points - 1), ")"),
    paste0("1 - (delta^2 + gamma^2 + 2*delta*gamma*cor", 1:(condition$time_points - 1), ")")
  )
  con <- ""
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}
