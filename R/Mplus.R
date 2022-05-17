#' @title
#' Create Mplus Model Syntax for RICLPM Power Analysis
#'
#' @description
#' Creates Mplus model syntax for a Monte Carlo power analysis for the random intercept cross-lagged panel model (RI-CLPM).
#'
#' @param input A list with elements representing input for the Monte Carlo power analysis. See "Details" for the required elements in \code{input}.
#'
#' @details
#' \subsection{\code{Input}}{The \code{input} argument must contain the following elements:
#' \itemize{
#'   \item \code{sample_size}: The sample size.
#'   \item \code{time_points}: The number of time points.
#'   \item \code{ICC}: The proportion of between-unit variance.
#'   \item \code{RI_var}: The random intercept variance.
#'   \item \code{RI_cov}: The covariance between the random intercepts.
#'   \item \code{Phi}: A matrix of standardized autoregressive and cross-lagged effects.
#'   \item \code{wSigma}: A correlation matrix for the within-components.
#'   \item \code{Psi}: Residual variance-covariance matrix for the within-components.
#'   \item \code{reps}: Number of replications.
#'   \item \code{seed}: An integer of length 1.
#'   \item \code{save_path}: The directory (data) to save Mplus model syntax to.
#'   \item \code{constraints}: The constraints imposed on the estimation model.}}
#'
#' \subsection{Naming conventions}{Details on the naming conventions can be found in the "Details" section of \code{\link{powRICLPM}}.}
#'
#' @return NULL
create_Mplus <- function(input) {

  # Number of variables
  input$k <- 2

  # Generate default variable names
  name_var <- LETTERS[1:input$k]

  # Create matrix of names for observed variable, within, and between components
  input$name_obs <- suppressMessages(
    purrr::map_dfc(name_var, paste0, 1:input$time_points)
  )
  input$name_within <- suppressMessages(
    purrr::map_dfc(name_var, function(x) {
      paste0("w", x, 1:input$time_points)
    })
  )
  input$name_RI <- paste0("RI_", name_var)

  # Create TITLE:, ANALYSIS:, MONTECARLO:
  PREAMBLE <- Mplus_pre(input)

  # Create MODEL POPULATION:
  Mplus_population <- rbind(
    Mplus_RI(input),
    Mplus_RI_var(input),
    Mplus_RI_cov(input),
    Mplus_within(input),
    Mplus_lagged(input),
    Mplus_within_var1(input),
    Mplus_within_cov1(input),
    Mplus_within_var2(input),
    Mplus_within_cov2(input),
    Mplus_ME(input),
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
    Mplus_RI(input),
    Mplus_RI_var(input, estimation = TRUE),
    Mplus_RI_cov(input, estimation = TRUE),
    Mplus_within(input),
    Mplus_lagged(input, estimation = TRUE),
    Mplus_within_var1(input, estimation = TRUE),
    Mplus_within_cov1(input, estimation = TRUE),
    Mplus_within_var2(input, estimation = TRUE),
    Mplus_within_cov2(input, estimation = TRUE),
    Mplus_ME(input),
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

  if (input$constraints == "stationarity") {
    # Create MODEL CONSTRAINT:
    Mplus_constraint <- rbind(
      Mplus_new(input),
      Mplus_within_cor(input),
      Mplus_stationarity(input),
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

  # Save Mplus model syntax
  cat(Mplus_syntax,
    file = file.path(
      input$save_path,
      paste0(
        "Mplus_N", input$sample_size,
        "_T", input$time_points,
        "_ICC", input$ICC, ".txt"
      )
    )
  )
  invisible()
}



Mplus_pre <- function(input) {
  # Create TITLE command
  TITLE <- paste0(
    "TITLE:\n  Power analysis RICLPM with N = ", input$sample_size,
    ", T = ", input$time_points,
    ", ICC = ", input$ICC, "\n\n"
  )

  # Create MONTECARLO command
  MONTECARLO <- paste0(
    "MONTECARLO:\n  NAMES = ", paste(unlist(input$name_obs), collapse = " "),
    ";\n  NOBSERVATIONS = ", input$sample_size,
    ";\n  NREPS = ", input$reps,
    ";\n  SEED = ", input$seed, ";\n\n"
  )

  # Create ANALYSIS command
  ANALYSIS <- paste0("ANALYSIS:\n  MODEL = NOCOV;\n\nMODEL POPULATION:\n  ")

  return(paste0(TITLE, MONTECARLO, ANALYSIS))
}

Mplus_RI <- function(input) {
  lhs <- rep(input$name_RI, each = input$time_points)
  op <- " BY "
  con <- "@1"
  rhs <- c(unlist(input$name_obs))
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_RI_var <- function(input, estimation = FALSE) {
  lhs <- input$name_RI
  op <- rhs <- rep("", times = input$k)
  if (estimation) {
    con <- rep(paste0("*", input$RI_var), times = input$k)
  } else {
    con <- rep(paste0("@", input$RI_var), times = input$k)
  }
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_RI_cov <- function(input, estimation = FALSE) {

  # Create combinations of random intercept factors
  combn_RI <- t(utils::combn(input$name_RI, 2))

  # Create syntax (parameter table) elements
  lhs <- combn_RI[, 1]
  op <- rep(" WITH ", times = nrow(combn_RI))
  if (estimation) {
    con <- paste0("*", input$RI_cov)
  } else {
    con <- paste0("@", input$RI_cov)
  }
  rhs <- combn_RI[, 2]
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_within <- function(input) {
  lhs <- c(unlist(input$name_within))
  op <- rep(" BY ", times = length(input$name_within))
  con <- rep("@1", times = length(input$name_within))
  rhs <- c(unlist(input$name_obs))
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_lagged <- function(input, estimation = FALSE) {
  # Create vector with outcomes
  lhs <- rep(c(t(input$name_within))[-(1:input$k)], each = input$k)
  op <- " ON "

  if (estimation) { # Estimation
    if (input$constraints == "none" ||
      input$constraints == "residuals") { # Freely estimate
      con <- paste0("*", rep(c(input$Phi), times = (input$time_points - 1)))
    } else if (input$constraints == "lagged" ||
      input$constraints == "within" ||
      input$constraints == "stationarity") { # Constrain over time
      con <- c("(alpha)", "(beta)", "(delta)", "(gamma)")
    }
  } else {
    con <- paste0("@", rep(c(input$Phi), times = (input$time_points - 1)))
  }

  # Create vector with predictors
  rhs <- c(apply(input$name_within[-input$time_points, ], 1, rep, times = input$k))

  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_within_var1 <- function(input, estimation = FALSE) {
  lhs <- t(input$name_within[1, ])
  op <- rhs <- ""
  if (estimation) { # Estimation
    if (input$constraints == "stationarity") { # Label but freely estimate
      con <- c("(varA1)", "(varB1)")
    } else if (input$constraints == "none" ||
      input$constraints == "lagged" ||
      input$constraints == "residuals" ||
      input$constraints == "within") {
      con <- rep("*1", times = input$k)
    }
  } else { # Data generation
    con <- rep("@1", times = input$k)
  }
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_within_cov1 <- function(input, estimation = F) {
  lhs <- unlist(input$name_within[1, 1])
  rhs <- unlist(input$name_within[1, 2])
  op <- rep(" WITH ", times = 1)

  if (estimation) {
    if (input$constraints == "stationarity") { # Label but freely estimate
      con <- "(cor1)"
    } else if (input$constraints == "none" ||
      input$constraints == "lagged" ||
      input$constraints == "residuals" ||
      input$constraints == "within") { # Freely estimate
      con <- paste0("*", c(input$wSigma[lower.tri(input$wSigma)]))
    }
  } else { # Data generation
    con <- paste0("@", c(input$wSigma[lower.tri(input$wSigma)]))
  }
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_within_var2 <- function(input, estimation = FALSE) {
  lhs <- c(unlist(input$name_within[-1, ]))
  op <- rhs <- ""

  if (estimation) { # Estimation
    if (input$constraints == "none" ||
      input$constraints == "lagged") { # Freely estimate
      con <- rep(paste0("*", diag(input$Psi)), each = input$time_points - 1)
    } else if (input$constraints == "residuals" ||
      input$constraints == "within") { # Constrain over time
      con <- rep(paste0("(rvar", LETTERS[1:input$k], ")"), each = input$time_points - 1)
    } else if (input$constraints == "stationarity") {
      con <- c(
        paste0("(rvarA", 2:input$time_points, ")"),
        paste0("(rvarB", 2:input$time_points, ")")
      )
    }
  } else { # Data generation
    con <- rep(paste0("@", diag(input$Psi)), each = input$time_points - 1)
  }
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_within_cov2 <- function(input, estimation = FALSE) {
  # Create within-component combinations
  combn_within <- t(apply(input$name_within[-1, ], 1, utils::combn, m = 2))

  # Create syntax (parameter table) elements
  lhs <- combn_within[, 1]
  rhs <- combn_within[, 2]
  op <- rep(" WITH ", times = nrow(combn_within))

  # Select residual covariances
  resCov <- c(input$Psi[lower.tri(input$Psi)])

  # Estimation
  if (estimation) {
    if (input$constraints == "none" ||
      input$constraints == "lagged") { # Freely estimate
      con <- paste0("*", resCov)
    } else if (input$constraints == "residuals" ||
      input$constraints == "within") { # Constrain over time
      con <- "(rcov)"
    } else if (input$constraints == "stationarity") {
      con <- paste0("(rcov", 2:input$time_points, ")")
    }

    # Data generation
  } else {
    con <- paste0("@", resCov)
  }
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_ME <- function(input) {
  lhs <- c(unlist(input$name_obs))
  op <- rhs <- ""
  con <- rep("@0", times = length(input$name_obs))
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_new <- function(input) {
  lhs <- paste0("NEW(cor", 2:input$time_points, ")")
  op <- rhs <- con <- ""
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_within_cor <- function(input) {
  lhs <- paste0("cor", 2:input$time_points)
  op <- " = "
  rhs1 <- paste0("alpha*delta + beta*gamma + alpha*gamma*cor", 1:(input$time_points - 1))
  rhs2 <- paste0(" + beta*delta*cor", 1:(input$time_points - 1))
  rhs3 <- paste0(" + rcov", 2:input$time_points)
  rhs <- paste0(rhs1, rhs2, rhs3)
  con <- ""
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}

Mplus_stationarity <- function(input) {
  lhs <- c(
    paste0("rvarA", 2:input$time_points),
    paste0("rvarB", 2:input$time_points)
  )
  op <- "="
  rhs <- c(
    paste0("1 - (alpha^2 + beta^2 + 2*alpha*beta*cor", 1:(input$time_points - 1), ")"),
    paste0("1 - (delta^2 + gamma^2 + 2*delta*gamma*cor", 1:(input$time_points - 1), ")")
  )
  con <- ""
  return(cbind.data.frame(lhs, op, rhs, con,
    stringsAsFactors = FALSE
  ))
}
