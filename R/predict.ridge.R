#' Predict values from a ridgereg object
#'
#' @param object An object of class \code{ridgereg}.
#' @param newdata A data frame containing new observations.
#'   If omitted, the fitted values are returned.
#' @param ... Additional arguments.
#'
#' @return A numeric vector containing predicted values.
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {

  # Right now we just want to save the dataset without the response variable
  formula <- stats::delete.response(stats::terms(object$dataset))

  # Getting the original standardisation from the training data
  dataset <- stats::model.matrix(formula, object$dataset)
  dataset <- scale(
    dataset[, colnames(dataset) != "(Intercept)", drop = FALSE]
  )

  # Making a design matrix for the new data
  X <- stats::model.matrix(formula, newdata)

  # Use the training means and standard deviations to scale new data
  X[, colnames(X) != "(Intercept)"] <- scale(
    X[, colnames(X) != "(Intercept)", drop = FALSE],
    center = attr(dataset, "scaled:center"),
    scale = attr(dataset, "scaled:scale")
  )

  # Calculating predictions
  predictions <- X %*% object$ridge_coefs

  return(predictions)
}
