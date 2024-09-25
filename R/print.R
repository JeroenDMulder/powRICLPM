#' @title
#' Print \code{powRICLPM} Object
#'
#' @description
#' \code{print.powRICLPM} prints a table listing all experimental conditions contained in the \code{powRICLPM} object, as well as the frequency of the estimation problems that occurred in each.
#'
#' @param x A \code{powRICLPM} object.
#' @param ... (don't use)
#'
#' @return No return value, called for side effects.
#'
#' @method print powRICLPM
#' @export
print.powRICLPM <- function(x, ...) {

  # Collect condition table
  df_conditions <- do.call(rbind, lapply(x$conditions, function(condition) {
    data.frame(
      sample_size = condition$sample_size,
      time_points = condition$time_points,
      ICC = condition$ICC,
      reliability = condition$reliability,
      stringsAsFactors = FALSE
    )
  }))
  df_conditions <- cbind(condition = 1:length(x$conditions), df_conditions)

  # Print header
  cat("powRICLPM (", as.character(x$session$powRICLPM_version), ") simulated power for ", length(x$conditions), " experimental conditions:", sep = "")

  # Format condition table
  print(
    knitr::kable(
      df_conditions,
      format = "simple",
      align = rep("r", times = length(colnames(df_conditions))),
      col.names = c("Condition", "Sample size", "Time points", "ICC", "Reliability")
    )
  )
}



#' Print Summary Call powRICLPM
#'
#' @noRd
print.summary.powRICLPM <- function(x, ..., powRICLPM_version) {
  cat("powRICLPM (", as.character(powRICLPM_version), ") simulated power for ", nrow(x), " experimental conditions.", sep = "")
  cat("\n")
  print(
    knitr::kable(
      x,
      align = "rrrrrrr",
      format = "simple",
      caption = "SUMMARY OF ANALYSIS PER EXPERIMENTAL CONDITION"
    )
  )
}

#' Print Summary Condition Call powRICLPM
#'
#' @noRd
print.summary.powRICLPM.condition <- function(x, ...) {
  cat("\n")
  print(
    knitr::kable(
      x$replications,
      align = "lr",
      format = "simple",
      caption = "SUMMARY OF ANALYSIS"
    )
  )
  cat("\n")
  print(
    knitr::kable(
      x$summary_condition,
      align = "lr",
      format = "simple",
      caption = "SUMMARY OF SIMULATION CONDITION"
    )
  )
  cat("\n")
  print(
    knitr::kable(
      x$results,
      format = "simple",
      align = "lrrrrrrrr",
      escape = FALSE,
      caption = "SIMULATION RESULTS"
    )
  )
}

#' Print Summary Parameter Call powRICLPM
#'
#' @noRd
print.summary.powRICLPM.parameter <- function(x, ..., parameter) {
  print(
    knitr::kable(
      x,
      format = "simple",
      align = rep("r", times = length(colnames(x))),
      caption = paste0("SIMULATION RESULTS FOR ", parameter)
    )
  )
}

#' Print Mplus Call powRICLPM
#'
#' @noRd
print.powRICLPM.Mplus <- function(x, ..., save_path) {

  # Collect condition table
  df_conditions <- do.call(rbind, lapply(x, function(condition) {
    data.frame(
      sample_size = condition$sample_size,
      time_points = condition$time_points,
      ICC = condition$ICC,
      reliability = condition$reliability,
      stringsAsFactors = FALSE
    )
  }))
  df_conditions <- cbind(condition = 1:length(x), df_conditions)

  cli::cli_alert_info("Mplus input files for power analysis have been saved to {.path {save_path}}.\n
                      The conditions numbers correspond to the following conditions:")

  # Format condition table
  print(
    knitr::kable(
      df_conditions,
      format = "simple",
      align = rep("r", times = length(colnames(df_conditions))),
      col.names = c("Condition", "Sample size", "Time points", "ICC", "Reliability")
    )
  )
}



