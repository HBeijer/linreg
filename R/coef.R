#' Extract coefficients from a linreg object
#'
#' Returns the coefficients estimated by the linear regression model.
#'
#' @param object An object of class \code{linreg}.
#' @param ... Additional arguments.
#'
#' @return A named numeric vector containing the coefficients.
#' @importFrom stats coef
#' @export
coef.linreg <- function(object, ...) {

  # Convert the coefficient matrix into a numeric vector
  coefficients <- as.numeric(object$beta_hat)

  # Use the design-matrix column names as coefficient names
  names(coefficients) <- colnames(object$X)

  coefficients
}
