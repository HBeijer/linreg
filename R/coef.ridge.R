#' Extract coefficients from a ridgereg object
#'
#' Returns the coefficients estimated by the ridge regression model.
#'
#' @param object An object of class \code{ridgereg}.
#' @param ... Additional arguments.
#'
#' @return A named numeric vector containing the coefficients.
#'   The covariate coefficients refer to the standardised variables.
#' @importFrom stats coef
#' @export
coef.ridgereg <- function(x, ...) {
  
  # Making thr coef matrix into a numeric vector
  coefs <- as.numeric(x$ridge_coefs)
  
  # Taking the names of the variables from the design matrix
  names(coefs) <- colnames(x$X)
  
  coefs
}
