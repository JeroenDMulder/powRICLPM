#' @title
#' Print \code{powRICLPM} object
#'
#' @description
#' \code{print.powRICLPM} prints a textual summary of the setup of the power analysis within the \code{powRICLPM} object. Specifically, it prints the characteristics of the different experimental conditions, as well as session info (information that applies to every condition).
#'
#' @param x A \code{powRICLPM} object.
#' @param ... Argument not in use.
#'
#' @method print powRICLPM
#' @export
print.powRICLPM <- function(x, ...) {
  cat("powRICLPM (", as.character(x$session$version), ") simulated power for ", length(x$conditions), " experimental conditions:", sep = "")
  print(
    knitr::kable(
      give(x, "estimation_problems"),
      align = "crrr",
      format = "simple",
      col.names = c(
        "Sample size", "Time points", "ICC",
        "Errors", "Non-convergence", "Inadmissible results"
      )
    )
  )
}
