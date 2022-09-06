#' @title
#' Print \code{powRICLPM} object
#'
#' @description
#' \code{print.powRICLPM} prints a table listing all experimental conditions contained in the \code{powRICLPM} object, as well as the frequency of the estimation problems that occurred in each.
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
