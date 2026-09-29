#' Extract residuals from a linreg object
#'
#' Returns the residuals estimated by the linear regression model.
#'
#' @param object An object of class \code{linreg}.
#' @param ... Additional arguments.
#'
#' @return A numeric vector containing the residuals.
#' @importFrom stats residuals
#' @export
residuals.linreg <- function(object, ...) {
  # Convert the n x 1 residual matrix into a numeric vector
  as.numeric(object$e_hat)
}
