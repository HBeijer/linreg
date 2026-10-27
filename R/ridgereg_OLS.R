#' Ridge regression using least squares
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
#' The ridge coefficients are calculated by solving the penalised
#' normal equations without explicitly computing a matrix inverse.
#'
#' When lambda is zero, the design matrix must have full column rank.
#' Fitted values are calculated for the observations retained in the
#' model frame.
#'
#' The training model frame, predictor means and standard deviations
#' are saved to support predictions on new observations using the
#' same standardisation as the training data.
#'
#' @return An object of class \code{ridgereg}, containing:
#' \describe{
#'   \item{call}{The function call.}
#'   \item{ridge_coefs}{A matrix of coefficients for the standardised
#'     predictors and the unpenalised intercept.}
#'   \item{fitted_values}{A one-column matrix of fitted response values.}
#'   \item{X}{The standardised design matrix, including the intercept.}
#'   \item{y}{The response as a one-column matrix.}
#'   \item{lambda}{The ridge penalty used.}
#'   \item{dataset}{The model frame used for fitting, including the response.}
#'   \item{centers}{A named numeric vector of training predictor means.}
#'   \item{scales}{A named numeric vector of training predictor sample
#'     standard deviations.}
#' }
#'
#' @examples
#' model <- ridgereg(
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
ridgereg <- function(formula,data,lambda){
  call <- match.call()

  # Input check.
  stopifnot("lambda must be a single finite, non-negative number" =
              is.numeric(lambda) &&
              !is.complex(lambda) &&
              length(lambda) == 1L &&
              is.finite(lambda) &&
              lambda >= 0)

  # Preparing data for modelling
  dataset <- stats::model.frame(formula,data)
  X <- stats::model.matrix(formula, dataset)
  y <- as.matrix(stats::model.response(dataset))

  # Identify predictor columns, excluding the intercept.
  predictor_columns <- colnames(X) != "(Intercept)"

  # Standardize predictors and retain the extra information.
  scaled_predictors <- scale(X[, predictor_columns, drop = FALSE])

  # Save the training means and standard deviations.
  centers <- attr(scaled_predictors, "scaled:center")
  scales <- attr(scaled_predictors, "scaled:scale")

  # Put the standardized values back into X.
  X[, predictor_columns] <- scaled_predictors

  # Determine which coefficients receive the penalty.
  I <- diag(ncol(X))
  I[1, 1] <- 0 # Exclude the intercept.

  # Calculate the ridge coefficients.
  ridge_coefs <- solve(crossprod(X) + lambda * I, crossprod(X, y))

  # Calculate fitted values.
  fitted_values <- X %*% ridge_coefs

  # Saving values and making a "ridge" class
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

  # Lastly returning the regression results
  return(results)
}
