#' Ridge regression using QR decomposition
#'
#' Fits a ridge regression model with standardised covariates.
#' The intercept is included but is not penalised.
#'
#' @param formula A formula specifying the numeric response and covariates.
#'   The formula must include an intercept.
#' @param data A data frame containing the variables in the formula.
#' @param lambda A single finite, non-negative number specifying
#'   the ridge penalty. Larger values apply a stronger penalty.
#'
#' @details
#' Predictor columns are centred and divided by their sample standard
#' deviations. They must have finite, non-zero standard deviations.
#' The intercept column is not standardised.
#' The returned coefficients refer to the standardised predictors.
#'
#' Extra rows are added to the design matrix and response to represent
#' the ridge penalty. The augmented least-squares problem is solved
#' using QR decomposition, without explicitly computing a matrix inverse.
#'
#' When lambda is zero, the design matrix must have full column rank.
#' Fitted values are calculated for the observations retained in the
#' model frame, excluding the artificial penalty rows.
#'
#' The original training model frame, predictor means and standard
#' deviations are saved to support predictions on new observations
#' using the same standardisation as the training data.
#'
#' @return An object of class \code{ridgereg}, containing:
#' \describe{
#'   \item{call}{The function call.}
#'   \item{ridge_coefs}{A matrix of coefficients for the standardised
#'     predictors and the unpenalised intercept.}
#'   \item{fitted_values}{A one-column matrix of fitted response values
#'     for the original training observations.}
#'   \item{X}{The standardised design matrix, including the intercept
#'     and excluding the artificial penalty rows.}
#'   \item{y}{The response as a one-column matrix, excluding the
#'     artificial penalty rows.}
#'   \item{lambda}{The ridge penalty used.}
#'   \item{dataset}{The model frame used for fitting, including the response.}
#'   \item{centers}{A named numeric vector of training predictor means.}
#'   \item{scales}{A named numeric vector of training predictor sample
#'     standard deviations.}
#' }
#'
#' @examples
#' model <- ridgereg_qr(
#'   Petal.Length ~ Sepal.Length + Sepal.Width,
#'   data = iris,
#'   lambda = 2
#' )
#'
#' model$ridge_coefs
#' head(model$fitted_values)
#' model$centers
#' model$scales
#'
#' @export
ridgereg_qr <- function(formula, data, lambda) {

  # Save the function call so print() can display it later.
  call <- match.call()

  # Check that lambda is one finite, non-negative number.
  stopifnot("lambda must be a single finite, non-negative number" =
              is.numeric(lambda) &&
              !is.complex(lambda) &&
              length(lambda) == 1L &&
              is.finite(lambda) &&
              lambda >= 0)

  # Build the model frame first so X and y use the same observations.
  dataset <- stats::model.frame(formula, data)

  # This implementation expects an intercept as the first column of X.
  stopifnot("The formula must include an intercept" = attr(stats::terms(dataset), "intercept") == 1L)

  X <- stats::model.matrix(formula, dataset)
  y <- as.matrix(stats::model.response(dataset))
  stopifnot("The response must be a single finite numeric variable" =
              is.numeric(y) &&
              !is.complex(y) &&
              ncol(y) == 1L &&
              all(is.finite(y)))

  # Identify predictor columns, leaving out the intercept.
  predictor_columns <- colnames(X) != "(Intercept)"

  # Standardize predictors while preserving their matrix structure.
  scaled_predictors <- scale(X[, predictor_columns, drop = FALSE])

  # Save the training means and standard deviations for prediction.
  centers <- attr(scaled_predictors, "scaled:center")
  scales <- attr(scaled_predictors, "scaled:scale")

  # Constant predictors cannot be standardized by division.
  stopifnot("Predictors must have finite, non-zero standard deviations" = all(is.finite(scales) & scales > 0))

  # Replace predictor values with their standardized versions.
  # The intercept column remains unchanged.
  X[, predictor_columns] <- scaled_predictors

  # With no penalty, unique coefficients require full column rank.
  if (lambda == 0 && qr(X)$rank < ncol(X)) stop("The design matrix must have full column rank when lambda is zero.")

  # Create the penalty matrix.
  # Zero in the first position leaves the intercept unpenalized.
  I <- diag(ncol(X))
  I[1, 1] <- 0

  # Add artificial rows that represent the ridge penalty.
  # sqrt(lambda) is used because least squares squares the residuals.
  # Keep X unchanged: it still represents the original observations.
  X_aug <- rbind(X, sqrt(lambda) * I)

  # The artificial penalty rows have response values of zero.
  # Add one zero for each extra row in X_aug.
  y_aug <- rbind(y, matrix(0, nrow = ncol(X), ncol = 1))

  # Solve the augmented least-squares problem using QR decomposition.
  decomp <- qr(X_aug)
  ridge_coefs <- qr.coef(decomp, y_aug)

  # Calculate fitted values for the original observations only.
  # Using X_aug here would also produce values for the artificial rows.
  fitted_values <- X %*% ridge_coefs

  # Store the results and the information needed for later prediction.
  results <- list(
    call = call,
    ridge_coefs = ridge_coefs,
    fitted_values = fitted_values,
    X = X,
    y = y,
    lambda = lambda,
    dataset = dataset,
    centers = centers,
    scales = scales
  )

  class(results) <- "ridgereg"
  return(results)
}
