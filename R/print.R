#' This is an implementation of a print function for the linreg class. It
#' prints out the estimated coefficients.
#'
#' @param x is the object of the class of which the model is saved to.
#' @export

print.linreg <- function(x){
  cat("Coefficients:\n")
  print(x$beta_hat)
}
