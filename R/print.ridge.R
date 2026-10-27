#' This method is printing the ridgereg regresssion coefs
#'
#' The input from the user and estimates coefficients from the model is presented
#'
#' @param x An object of class \code{linreg}.
#' @param ... Additional arguments.
#'
#' @return Invisibly returns the original \code{linreg} object.
#' @export
print.ridgereg <- function(x,...){
  cat("\nCall: \n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(drop(x$ridge_coefs)) 
  invisible(x)
}
