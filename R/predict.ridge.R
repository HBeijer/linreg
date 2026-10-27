#' Predict values from a ridgereg object
#'
#' @param object An object of class \code{ridgereg}.
#' @param newdata A data frame containing new observations.
#'   If omitted or NULL, fitted values for the retained training rows
#'   are returned.
#' @param ... Additional arguments.
#'
#' @details
#' New observations use the training predictor means and sample standard
#' deviations saved in the fitted object. The intercept is not standardised.
#' Training factor levels and contrasts are reused.
#'
#' @return A numeric vector containing one prediction per row of
#'   \code{newdata}, or the fitted training values when \code{newdata}
#'   is omitted. Missing predictor values produce missing predictions.
#'
#' @importFrom stats predict
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {

  # With no new data, return predictions for the training observations.
  if (is.null(newdata)) return(as.numeric(object$fitted_values))

  # Keep the training model terms but remove the response variable.
  model_terms <- stats::delete.response(stats::terms(object$dataset))

  # Find the category definitions used during training.
  # Example: training group has levels A, B and C, but newdata might contain only observations from group B.
  # We must still remember all three training levels.
  #
  # lapply() examines each column of the saved training model frame.
  # Inside function(x), x represents one entire column.
  xlevels <- lapply(object$dataset, function(x) {
    if (is.factor(x)) levels(x)
    else if (is.character(x)) levels(factor(x))
    else NULL})

  # Remove the NULL entries, keeping only categorical columns.
  # vapply(..., is.null, logical(1)) checks each list entry.
  # The result is a named list such as: list(group = c("A", "B", "C"))
  xlevels <- xlevels[!vapply(xlevels, is.null, logical(1))]

  # Collect the predictor variables required by the training formula. The response has already been removed from model_terms.
  # na.pass keeps rows containing missing predictor values.
  # Their positions are preserved, so they can receive NA predictions.
  # xlev supplies the training category definitions.
  # Even if newdata contains only B, its factor retains levels A, B, C.
  # A completely unknown category, such as D, produces an error.
  prediction_data <- stats::model.frame(
    model_terms,
    data = newdata,
    na.action = stats::na.pass,
    xlev = xlevels
  )

  # Convert the prepared predictors into numeric model columns. This includes the intercept and columns representing categories.
  #
  # With default treatment coding and training levels A, B, C:
  # A is the reference category: groupB = 0 and groupC = 0.
  # B is represented by: groupB = 1 and groupC = 0.
  # C is represented by: groupB = 0 and groupC = 1.
  #
  # attr(object$X, "contrasts") retrieves the training encoding rules.
  # Reusing them keeps category coding consistent with the fitted model.
  X <- stats::model.matrix(model_terms, prediction_data, contrasts.arg = attr(object$X, "contrasts"))

  # Arrange columns in exactly the same order as the training matrix.
  # Each column must line up with its corresponding fitted coefficient.
  # drop = FALSE keeps X as a matrix even when selecting one column.
  X <- X[, colnames(object$X), drop = FALSE]

  # Apply the training means and standard deviations to new observations.
  predictor_columns <- colnames(X) != "(Intercept)"

  if (any(predictor_columns)) {
    X[, predictor_columns] <- scale(X[, predictor_columns, drop = FALSE],
                                    center = object$centers, scale = object$scales)}

  # Multiply by the fitted coefficients and return a numeric vector.
  predictions <- X %*% object$ridge_coefs
  return(as.numeric(predictions))
}
