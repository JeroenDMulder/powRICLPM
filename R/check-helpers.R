#' Check if matrix is positive definite
#'
#' \code{is_PD()} determines if matrix \code{x} is positive definite by checking if the eigenvalues are non-negative. The function also handles complex eigenvalues.
#'
#' @param x A square matrix.
#'
#' @noRd
is_PD <- function(x) {
  e <- eigen(x, only.values = TRUE)$values
  if (is.complex(e)) {
    if (any(Re(e) < 0) || any(Im(e) < 0)) {
      return(FALSE)
    }
  } else {
    if (any(e < 0)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Check if the implied process is stationary
#'
#' \code{is_unit()} checks if the process implied by the regression matrix \code{x} is stationary, by testing if its eigenvalues lie within the unit circle.
#'
#' @param x A square matrix.
#'
#' @noRd
is_unit <- function(x) {
  e <- eigen(x, only.values = TRUE)$values
  if (is.complex(e)) {
    if (any(sqrt((Re(e)^2) + (Im(e)^2)) > 1)) {
      return(FALSE)
    }
  } else {
    if (any(e >= 1)) {
      return(FALSE)
    }
  }
  return(TRUE)
}
