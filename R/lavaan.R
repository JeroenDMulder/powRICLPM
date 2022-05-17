#' @title
#' Create Lavaan Model Syntax for RICLPM Power Analysis
#'
#' @description
#' Creates a \pkg{lavaan} parameter table or model syntax for the specified model.
#'
#' @inheritParams powRICLPM
#' @param RI_var Numeric value denoting the random intercept variance.
#' @param RI_cov Numeric value denoting the covariance between the random intercepts.
#' @param Psi Variance-covariance matrix of within-unit residuals from wave 2 onwards.
#' @param syntax Logical indicating whether model syntax should be created.
#' @param estimation Logical indicator whether lavaan model syntax is inteded to simulate data. If FALSE, the lavaan syntax is created for the estimation of data.
#'
#' @return A data frame containing the model parameters (parameter elements as characters).
#'
#' @details
#' \subsection{Data generation}{If lavaan model syntax needs to be created for data generation, the user must provide values for the \code{ICC}, \code{RI_cor}, \code{Phi}, \code{wSigma}, and \code{Psi} arguments. By default, these arguments are set to \code{NULL}, such that when the model syntax is made from the parameter table, these parameters are estimated rather than set.}
#'
#' \subsection{Naming conventions}{Details on the naming conventions can be found in the "Details" section of \code{\link{powRICLPM}}.}
create_lavaan <- function(time_points,
                          RI_var = NULL,
                          RI_cov = NULL,
                          Phi = NULL,
                          wSigma = NULL,
                          Psi = NULL,
                          syntax = FALSE,
                          estimation = FALSE,
                          constraints) {

  # Save all arguments to list
  input <- as.list(environment())

  # Number of variables
  input$k <- 2

  # Generate default variable names
  name_var <- LETTERS[1:input$k]

  # Create matrix of names for observed variable, within, and between components
  input$name_obs <- suppressMessages(
    purrr::map_dfc(name_var, paste0, 1:time_points)
  )
  input$name_within <- suppressMessages(
    purrr::map_dfc(name_var, function(x) {
      paste0("w", x, 1:time_points)
    })
  )
  input$name_RI <- paste0("RI_", name_var)

  # Create parameter table
  lav_table <- rbind(
    lav_RI(input = input),
    lav_RI_var(input = input),
    lav_RI_cor(input = input),
    lav_within(input = input),
    lav_lagged(input = input),
    lav_within_var1(input = input),
    lav_within_cov1(input = input),
    lav_within_var2(input = input),
    lav_within_cov2(input = input),
    lav_ME(input = input),
    lav_within_cor(input = input),
    lav_stationarity(input = input)
  )

  # Remove rownames
  rownames(lav_table) <- NULL

  # Merge parameter table into model syntax
  if (syntax) {
    lav_syntax <- paste0( # Paste over parameters
      paste0( # Paste over columns
        lav_table[, 1],
        lav_table[, 2],
        lav_table[, 3],
        lav_table[, 4],
        lav_table[, 5]
      ),
      collapse = "\n"
    )

    return(lav_syntax)
  }
  return(lav_table)
}

lav_RI <- function(input) {
  lhs <- rep(input$name_RI, each = input$time_points)
  op <- rep("=~", times = input$k * input$time_points)
  pv <- rep("1", times = input$k * input$time_points)
  con <- rep("*", times = input$k * input$time_points)
  rhs <- c(unlist(input$name_obs))
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_RI_var <- function(input) {
  lhs <- rhs <- input$name_RI
  op <- rep("~~", times = input$k)
  con <- rep("*", times = input$k)
  if (input$estimation) {
    pv <- paste0("start(", input$RI_var, ")")
    free <- TRUE
  } else {
    pv <- rep(input$RI_var, times = input$k)
    free <- FALSE
  }
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_RI_cor <- function(input) {
  # Create combinations of random intercept factors
  combnRI <- t(utils::combn(input$name_RI, 2))

  # Create syntax (parameter table) elements
  lhs <- combnRI[, 1]
  op <- "~~"
  con <- "*"
  if (input$estimation) {
    pv <- paste0("start(", input$RI_cov, ")")
    free <- TRUE
  } else {
    pv <- input$RI_cov
    free <- FALSE
  }
  rhs <- combnRI[, 2]
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_within <- function(input) {
  lhs <- c(unlist(input$name_within))
  op <- rep("=~", times = length(input$name_within))
  pv <- rep("1", times = length(input$name_within))
  con <- "*"
  rhs <- c(unlist(input$name_obs))
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_lagged <- function(input) {
  lhs <- rep(c(t(input$name_within))[-(1:input$k)], each = input$k)
  op <- rep("~", times = input$k^2)
  con <- "*"

  # Estimation
  if (input$estimation) {
    if (input$constraints == "none" || input$constraints == "residuals") {
      pv <- paste0("start(", c(t(input$Phi)), ")")
    } else if (input$constraints == "lagged" || input$constraints == "within" ||
      input$constraints == "stationarity") {
      pv <- c("alpha", "beta", "delta", "gamma") # Labels for constraints
    }
    free <- TRUE

    # Data generation
  } else {
    pv <- c(t(input$Phi))
    free <- FALSE
  }

  rhs <- c(apply(input$name_within[-input$time_points, ], 1, rep, times = input$k))
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_within_var1 <- function(input) {
  lhs <- rhs <- t(input$name_within[1, ])
  op <- rep("~~", times = input$k)
  con <- "*"

  # Estimation
  if (input$estimation) {
    if (input$constraints == "stationarity") {
      pv <- c("varA1", "varB1") # Label but freely estimate
    } else if (input$constraints == "none" ||
      input$constraints == "lagged" ||
      input$constraints == "residuals" ||
      input$constraints == "within") { # Freely estimate
      pv <- paste0("start(1)")
    }
    free <- TRUE

    # Data generation
  } else {
    pv <- "1"
    free <- FALSE
  }

  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_within_cov1 <- function(input) {
  lhs <- unlist(input$name_within[1, 1])
  rhs <- unlist(input$name_within[1, 2])
  op <- "~~"
  con <- "*"

  # Estimation
  if (input$estimation) {
    if (input$constraints == "stationarity") { # Label
      pv <- "cor1"
    } else if (input$constraints == "none" ||
      input$constraints == "lagged" ||
      input$constraints == "residuals" ||
      input$constraints == "within") { # Freely estimate
      pv <- paste0("start(", c(input$wSigma[lower.tri(input$wSigma)]), ")")
    }
    free <- TRUE

    # Data generation
  } else {
    pv <- c(input$wSigma[lower.tri(input$wSigma)]) # Get covariances
    free <- FALSE
  }
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_within_var2 <- function(input) {
  lhs <- rhs <- c(unlist(input$name_within[-1, ]))
  op <- "~~"
  con <- "*"

  # Estimation
  if (input$estimation) {
    if (input$constraints == "none" ||
      input$constraints == "lagged") { # Freely estimate
      pv <- paste0("start(", rep(diag(input$Psi), each = (input$time_points - 1)), ")")
    } else if (input$constraints == "residuals" ||
      input$constraints == "within") { # Constrain over time
      pv <- rep(c("rvarA", "rvarB"), each = (input$time_points - 1))
    } else if (input$constraints == "stationarity") {
      pv <- c(
        paste0("rvarA", 2:input$time_points),
        paste0("rvarB", 2:input$time_points)
      )
    }
    free <- TRUE

    # Data generation
  } else {
    pv <- rep(diag(input$Psi), each = (input$time_points - 1))
    free <- FALSE
  }
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_within_cov2 <- function(input) {

  # Create combinations of later within-components
  combnWComps <- t(apply(input$name_within[-1, ], 1, utils::combn, m = 2))

  # Create syntax (parameter table) elements
  lhs <- combnWComps[, 1]
  rhs <- combnWComps[, 2]
  op <- rep("~~", times = nrow(combnWComps))
  con <- "*"

  # Estimation
  if (input$estimation) {
    if (input$constraints == "none" ||
      input$constraints == "lagged") { # Freely estimate
      pv <- paste0("start(", c(input$Psi[lower.tri(input$Psi)]), ")")
    } else if (input$constraints == "residuals" ||
      input$constraints == "within") { # Constrain over time
      pv <- "rcov"
    } else if (input$constraints == "stationarity") { # Label
      pv <- paste0("rcov", 2:input$time_points)
    }
    free <- TRUE

    # Data generation
  } else {
    pv <- c(input$Psi[lower.tri(input$Psi)])
    free <- FALSE
  }
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_ME <- function(input) {
  lhs <- rhs <- c(unlist(input$name_obs))
  op <- rep("~~", times = length(input$name_obs))
  pv <- rep("0", times = length(input$name_obs))
  con <- "*"
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_within_cor <- function(input) {
  if (input$estimation &&
    input$constraints == "stationarity") {
    lhs <- paste0("cor", 2:input$time_points)
    op <- ":="
    pv <- con <- ""
    rhs1 <- paste0("alpha*delta + beta*gamma + alpha*gamma*cor", 1:(input$time_points - 1))
    rhs2 <- paste0(" + beta*delta*cor", 1:(input$time_points - 1))
    rhs3 <- paste0(" + rcov", 2:input$time_points)
    rhs <- paste0(rhs1, rhs2, rhs3)
    free <- FALSE
    return(cbind.data.frame(lhs, op, pv, con, rhs, free,
      stringsAsFactors = FALSE
    ))
  }
}

lav_stationarity <- function(input) {
  if (input$estimation &&
    input$constraints == "stationarity") {
    lhs <- c(
      paste0("rvarA", 2:input$time_points),
      paste0("rvarB", 2:input$time_points)
    )
    op <- "=="
    rhs <- c(
      paste0("1 - (alpha^2 + beta^2 + 2*alpha*beta*cor", 1:(input$time_points - 1), ")"),
      paste0("1 - (delta^2 + gamma^2 + 2*delta*gamma*cor", 1:(input$time_points - 1), ")")
    )
    pv <- con <- ""
    free <- FALSE
    return(cbind.data.frame(lhs, op, pv, con, rhs, free,
      stringsAsFactors = FALSE
    ))
  }
}
