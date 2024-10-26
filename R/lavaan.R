#' @title
#' Create `lavaan` Syntax for Data Generation and Model Estimation
#'
#' @description
#' \code{create_lavaan()} creates a \pkg{lavaan} parameter table and model syntax for data generation and estimation of the specified random intercept cross-lagged panel model.
#'
#' @param condition A list, containing values for factors that define an experimental condition (i.e., sample size, number of time points, ICC, and reliability), as well as population values for parameters that depend on these (e.g., the random intercept variances, and covariance between them is a function of `ICC`).
#'
#' @return The `condition` list, extended with a `lavaan` population model table and syntax, and `lavaan` estimation model table and syntax.
#'
#' @details
#' \subsection{Naming conventions}{Details on the naming conventions can be found in the "Details" section of \code{\link{powRICLPM}}.}
#'
#' @noRd
create_lavaan <- function(condition) {

  # Generate default variable names
  name_var <- LETTERS[1:2]

  # Create matrix of names for observed variable, within, and between components
  name_obs <- sapply(name_var, paste0, 1:condition[["time_points"]])
  name_within <- sapply(name_var, function(x) {
    paste0("w", x, 1:condition[["time_points"]])
  })
  name_RI <- paste0("RI_", name_var)

  # Create population parameter table
  pop_tab <- rbind(
    lav_RI(condition, name_RI, name_obs),
    pop_RI_var(condition, name_RI),
    pop_RI_cor(condition, name_RI),
    pop_within(condition, name_within, name_obs),
    pop_lagged(condition, name_within),
    pop_within_var1(condition, name_within),
    pop_within_cov1(condition, name_within),
    pop_within_var2(condition, name_within),
    pop_within_cov2(condition, name_within),
    pop_ME(condition, name_obs)
  )
  rownames(pop_tab) <- NULL

  # Create population model syntax
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

  # Create estimation parameter table
  est_tab <- rbind(
    lav_RI(condition, name_RI, name_obs),
    est_RI_var(condition, name_RI),
    est_RI_cor(condition, name_RI),
    est_within(condition, name_within, name_obs),
    est_lagged(condition, name_within),
    est_within_var1(condition, name_within),
    est_within_cov1(condition, name_within),
    est_within_var2(condition, name_within),
    est_within_cov2(condition, name_within),
    if (condition[["estimate_ME"]]) {
      create_estimate_ME(condition, name_obs)
    },
    if (condition[["constraints"]] == "stationarity") {
      rbind(
        lav_within_cor(condition),
        lav_stationarity(condition)
      )
    }
  )
  rownames(est_tab) <- NULL

  # Create estimation model syntax
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
    sample_size = condition[["sample_size"]],
    time_points = condition[["time_points"]],
    ICC = condition[["ICC"]],
    reliability = condition[["reliability"]],
    RI_var = condition[["RI_var"]],
    RI_cov = condition[["RI_cov"]],
    pop_synt = pop_synt,
    pop_tab = pop_tab,
    est_synt = est_synt,
    est_tab = est_tab,
    estimate_ME = condition[["estimate_ME"]],
    skewness = condition[["skewness"]],
    kurtosis = condition[["kurtosis"]],
    significance_criterion = condition[["significance_criterion"]],
    estimates = NA,
    MCSEs = NA,
    estimation_information = NA,
    reps = NA,
    condition_id = condition[["condition_id"]]
  )
}

lav_RI <- function(condition, name_RI, name_obs) {
  lhs <- rep(name_RI, each = condition[["time_points"]])
  op <- rep("=~", times = 2 * condition[["time_points"]])
  pv <- rep("1", times = 2 * condition[["time_points"]])
  con <- rep("*", times = 2 * condition[["time_points"]])
  rhs <- c(unlist(name_obs))
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_RI_var <- function(condition, name_RI) {
  lhs <- rhs <- name_RI
  op <- "~~"
  con <- "*"
  pv <- condition[["RI_var"]]
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_RI_var <- function(condition, name_RI) {
  lhs <- rhs <- name_RI
  op <- "~~"
  con <- "*"
  pv <- paste0("start(", condition[["RI_var"]], ")")
  free <- TRUE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_RI_cor <- function(condition, name_RI) {
  lhs <- name_RI[1]
  rhs <- name_RI[2]
  op <- "~~"
  con <- "*"
  pv <- condition[["RI_cov"]]
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_RI_cor <- function(condition, name_RI) {
  lhs <- name_RI[[1]]
  rhs <- name_RI[[2]]
  op <- "~~"
  con <- "*"
  pv <- paste0("start(", condition[["RI_cov"]], ")")
  free <- TRUE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_within <- function(condition, name_within, name_obs) {
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

est_within <- function(condition, name_within, name_obs) {
  op <- "=~"
  if (condition[["constraints"]] == "stationarity") {
    lhs <- c(name_within, name_within)
    pv <- c(rep("NA", times = length(name_within)), rep("start(1)", times = length(name_within)))
    free <- TRUE
    rhs <- c(name_obs, name_obs)
  } else {
    lhs <- c(name_within)
    pv <- "1"
    free <- FALSE
    rhs <- c(name_obs)
  }
  con <- "*"
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_lagged <- function(condition, name_within) {
  lhs <- rep(c(t(name_within))[-(1:2)], each = 2)
  op <- "~"
  con <- "*"
  pv <- c(t(condition[["Phi"]][[1]]))
  free <- FALSE
  rhs <- c(apply(name_within[-condition[["time_points"]], ], 1, rep, times = 2))
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_lagged <- function(condition, name_within) {
  op <- "~"
  con <- "*"
  if (condition[["constraints"]] == "lagged" ||
      condition[["constraints"]] == "within" ||
      condition[["constraints"]] == "stationarity") {
    lhs <- rep(rep(c(t(name_within))[-(1:2)], each = 2), times = 2)
    pv <- c(
      rep(c("a", "b", "c", "d"), times = condition[["time_points"]] - 1), # Labels for constraints
      rep(c(paste0("start(", t(condition[["Phi"]][[1]]), ")")), times = condition[["time_points"]] - 1) # Starting values
    )
    rhs <- rep(
      c(apply(name_within[-condition[["time_points"]], ], 1, rep, times = 2)),
      times = 2
    )
  } else {
    lhs <- rep(c(t(name_within))[-(1:2)], each = 2)
    pv <- paste0("start(", c(t(condition[["Phi"]][[1]])), ")")
    rhs <- c(apply(name_within[-condition[["time_points"]], ], 1, rep, times = 2))
  }
  free <- TRUE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_within_var1 <- function(condition, name_within) {
  lhs <- rhs <- c(t(name_within[1, ]))
  op <- "~~"
  con <- "*"
  pv <- "1"
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_within_var1 <- function(condition, name_within) {
  lhs <- rhs <- c(t(name_within[1, ]))
  op <- "~~"
  con <- "*"
  if (condition[["constraints"]] == "stationarity") {
    pv <- "1"
    free <- FALSE
  } else {
    pv <- paste0("start(1)")
    free <- TRUE
  }
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_within_cov1 <- function(condition, name_within) {
  lhs <- name_within[1, "A"]
  rhs <- name_within[1, "B"]
  op <- "~~"
  con <- "*"
  pv <- c(condition[["within_cor"]]) # Get covariances
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_within_cov1 <- function(condition, name_within) {
  op <- "~~"
  con <- "*"
  if (condition[["constraints"]] == "stationarity") { # Label
    lhs <- c(name_within[1, 1], name_within[1, 1])
    rhs <- c(name_within[1, 2], name_within[1, 2])
    pv <- c("cor1", paste0("start(", condition[["within_cor"]], ")"))
  } else { # Freely estimate
    lhs <- name_within[1, 1]
    rhs <- name_within[1, 2]
    pv <- paste0("start(", c(condition[["within_cor"]]), ")")
  }
  free <- TRUE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_within_var2 <- function(condition, name_within) {
  lhs <- rhs <- c(name_within[-1, ])
  op <- "~~"
  con <- "*"
  pv <- rep(diag(condition[["Psi"]][[1]]), each = (condition[["time_points"]] - 1))
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_within_var2 <- function(condition, name_within) {
  op <- "~~"
  con <- "*"
  if (condition[["constraints"]] == "residuals" ||
      condition[["constraints"]] == "within") { # Constrain over time
    lhs <- rhs <- c(name_within[-1, ], name_within[-1, ])
    pv <- c(
      rep(c("rvarA", "rvarB"), each = (condition[["time_points"]] - 1)),
      paste0("start(", rep(diag(condition[["Psi"]][[1]]), each = (condition[["time_points"]] - 1)), ")")
    )
  } else if (condition[["constraints"]] == "stationarity") {
    lhs <- rhs <- c(name_within[-1, ], name_within[-1, ])
    pv <- c(
      paste0("rvarA", 2:condition[["time_points"]]),
      paste0("rvarB", 2:condition[["time_points"]]),
      paste0("start(", rep(diag(condition[["Psi"]][[1]]), each = (condition[["time_points"]] - 1)), ")")
    )
  } else { # Freely estimate
    lhs <- rhs <- c(name_within[-1, ])
    pv <- paste0("start(", rep(diag(condition[["Psi"]][[1]]), each = (condition[["time_points"]] - 1)), ")")
  }
  free <- TRUE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_within_cov2 <- function(condition, name_within) {
  lhs <- name_within[-1, 1]
  rhs <- name_within[-1, 2]
  op <- "~~"
  con <- "*"
  pv <- c(condition[["Psi"]][[1]][lower.tri(condition[["Psi"]][[1]])])
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

est_within_cov2 <- function(condition, name_within) {
  op <- "~~"
  con <- "*"
  if (condition[["constraints"]] == "residuals" ||
      condition[["constraints"]] == "within") { # Constrain over time
    lhs <- c(name_within[-1, 1], name_within[-1, 1])
    rhs <- c(name_within[-1, 2], name_within[-1, 2])
    pv <- c(
      rep("rcov", times = condition[["time_points"]] - 1),
      rep(paste0("start(", c(condition[["Psi"]][[1]][lower.tri(condition[["Psi"]][[1]])]), ")"), times = condition[["time_points"]] - 1)
    )
  } else if (condition[["constraints"]] == "stationarity") { # Label
    lhs <- c(name_within[-1, 1])
    rhs <- c(name_within[-1, 2])
    pv <- paste0("rcov", 2:condition[["time_points"]])
  } else { # Freely estimate
    lhs <- name_within[-1, 1]
    rhs <- name_within[-1, 2]
    pv <- paste0("start(", c(condition[["Psi"]][[1]][lower.tri(condition[["Psi"]][[1]])]), ")")
  }
  free <- TRUE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

pop_ME <- function(condition, name_obs) {
  lhs <- rhs <- c(name_obs)
  op <- "~~"
  pv <- condition[["ME_var"]]
  free <- FALSE
  con <- "*"
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

create_estimate_ME <- function(condition, name_obs) {
  op <- "~~"
  con <- "*"
  if (condition[["constraints"]] == "stationarity" ||
      condition[["constraints"]] == "ME") {
    lhs <- rhs <- c(name_obs, name_obs)
    pv <- c(
      rep(c("MEvarA", "MEvarB"), each = condition[["time_points"]]),
      rep(paste0("start(", condition[["ME_var"]], ")"), times = 2*condition[["time_points"]])
    )
    free <- TRUE
  } else { # Freely estimate ME
    lhs <- rhs <- c(name_obs)
    pv <- paste0("start(", condition[["ME_var"]], ")")
    free <- TRUE
  }
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

estimate_ME0 <- function(condition, name_obs) {
  lhs <- rhs <- c(name_obs)
  op <- "~~"
  pv <- 0
  free <- FALSE
  con <- "*"
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_within_cor <- function(condition) {
  lhs <- paste0("cor", 2:(condition[["time_points"]] - 1))
  op <- ":="
  pv <- con <- ""
  rhs1 <- paste0("a*c + b*d + a*d*cor", 1:(condition[["time_points"]] - 2))
  rhs2 <- paste0(" + b*c*cor", 1:(condition[["time_points"]] - 2))
  rhs3 <- paste0(" + rcov", 2:(condition[["time_points"]] - 1))
  rhs <- paste0(rhs1, rhs2, rhs3)
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}

lav_stationarity <- function(condition) {
  lhs <- c(
    paste0("rvarA", 2:condition[["time_points"]]),
    paste0("rvarB", 2:condition[["time_points"]])
  )
  op <- "=="
  rhs <- c(
    paste0("1 - (a^2 + b^2 + 2*a*b*cor", 1:(condition[["time_points"]] - 1), ")"),
    paste0("1 - (c^2 + d^2 + 2*c*d*cor", 1:(condition[["time_points"]] - 1), ")")
  )
  pv <- con <- ""
  free <- FALSE
  return(cbind.data.frame(lhs, op, pv, con, rhs, free,
    stringsAsFactors = FALSE
  ))
}
