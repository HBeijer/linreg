#' Print a linreg object
#'
#' Prints the model call and its estimated coefficients.
#'
#' @param x An object of class \code{linreg}.
#' @param ... Additional arguments.
#'
#' @return Invisibly returns the original \code{linreg} object.
#' @export
print.linreg <- function(x, ...) {

  cat("\nCall:\n")
  print(x$call)

  cat("\nCoefficients:\n")
  print(coef(x))

  invisible(x)
}
