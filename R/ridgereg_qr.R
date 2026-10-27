#' Ridge regression using QR decomposition
#'
#' Fits a ridge regression model with standardised covariates.
#' The intercept is included but is not penalised.
#'
#' @param formula A formula specifying the response and covariates.
#'   The formula must include an intercept.
#' @param data A data frame containing the variables in the formula.
#' @param lambda A single finite, non-negative number specifying
#'   the ridge penalty. Larger values give more shrinkage.
#'
#' @details
#' Covariates are centred and divided by their sample standard deviations.
#' They must have finite, non-zero standard deviations.
#' The returned coefficients refer to the standardised covariates.
#'
#' Extra rows are added to the design matrix and response to include
#' the ridge penalty. The resulting least-squares problem is solved
#' using QR decomposition.
#'
#' When lambda is zero, the design matrix must have full column rank.
#' Fitted values are calculated for the observations retained in the
#' model frame, excluding the extra penalty rows.
#'
#' @return An object of class \code{ridgereg}, containing:
#' \describe{
#'   \item{call}{The function call.}
#'   \item{ridge_coefs}{A matrix of coefficients for the standardised covariates
#'     and the unpenalised intercept.}
#'   \item{fitted_values}{A one-column matrix of fitted response values.}
#'   \item{X}{The standardised design matrix, including the intercept.}
#'   \item{y}{The response as a one-column matrix.}
#'   \item{lambda}{The ridge penalty used.}
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
#'
#' @export
ridgereg_qr <- function(formula,data,lambda){
  call <- match.call()
  
  # Preparing data for modelling
  dataset <- stats::model.frame(formula,data)
  X <- stats::model.matrix(formula, dataset)
  y <- as.matrix(stats::model.response(model_data)) 
  
  # Standardising variables (removing intercept: we dont want to scale it!)
  X[,colnames(X) != "(Intercept)"] <- scale(X[,colnames(X) != "(Intercept)"])
  
  # Performing regression 
  I <- diag(ncol(X))
  I[1,1] <- 0 
  
  # Preparing for the QR decomposition; I am adding extra rows so it works to include
  # the penalty term.
  X <- rbind(X,sqrt(lambda)*I)
  y <- rbind(y,matrix(0,nrow=ncol(X),ncol=1))
  

  # QR Decomposition
  decomp <- qr(X)
  
  # Claculating the ridge coefficients
  ridge_coefs <- qr.coef(decomp,y)
  
  # Estimating fitted values using ridge coefs  
  fitted_values <- X %*% ridge_coefs
  
  # Saving values and making a "ridge" class
  results <- list(
    call = call,
    ridge_coefs = ridge_coefs,
    fitted_values = fitted_values,
    X = X,
    y = y,
    lambda = lambda
  )
  
  class(results) <- "ridgereg"
  
  # Lastly returning the regression results
  return(results)
}