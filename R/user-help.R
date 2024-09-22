#' Check Interpretation `Phi` Argument
#'
#' Write a textual interpretation of the values in `Phi`. This can be used to check if `Phi` has been correctly specified.
#'
#' @inheritParams powRICLPM
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @examples
#' # Correctly specified `Phi`
#' Phi1 <- matrix(c(.4, .1, .2, .3), ncol = 2, byrow = TRUE)
#' check_Phi(Phi1)
#'
#' # `Phi` with too large standardized effects
#' Phi2 <- matrix(c(.6, .5, .4, .7), ncol = 2, byrow = TRUE)
#' Phi2 <- check_Phi(Phi2)
check_Phi <- function(Phi) {

  # Check argument type
  if (!is.matrix(Phi)) {
    stop(rlang::format_error_bullets(c(
      "`Phi` must be a matrix:",
      x = paste0("Your `Phi` is a `", typeof(Phi), "`.")
    )))
  }

  # Interpretation cross-lagged effects
  writeLines(
    rlang::format_error_bullets(c(
      "According to this `Phi`, the lagged effects are:",
      "*" = paste0("Autoregressive effect of A: ", Phi[1, 1]),
      "*" = paste0("Autoregressive effect of B: ", Phi[2, 2]),
      "*" = paste0("Cross-lagged effect of A -> B: ", Phi[2, 1]),
      "*" = paste0("Cross-lagged effect of B -> A: ", Phi[1, 2])
    ))
  )

  # Check if positive definite
  if (!is_unit(Phi)) {
    writeLines(
        rlang::format_error_bullets(c(
        "\nHowever, lagged effects in `Phi` must specify a stationary process:",
        i = "This is checked by testing if the eigenvalues of `Phi` lie within unit circle.",
        x = "The eigenvalues of `Phi` are not within unit circle. Try out smaller lagged effects?"
      ))
    )
  }
}
