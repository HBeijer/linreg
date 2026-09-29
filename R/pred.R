#' Extract predicted values
#'
#' Returns the fitted values estimated by a linreg model.
#'
#' @param object An object of class \code{linreg}.
#' @param ... Additional arguments.
#'
#' @return A numeric vector containing the predicted values.
#' @export
pred <- function(object, ...) {
  # Dispatch to a method based on the class of object
  UseMethod("pred")
}


#' @rdname pred
#' @export
pred.linreg <- function(object, ...) {
  # Convert the n x 1 fitted-value matrix into a numeric vector
  as.numeric(object$y_hat)
}
