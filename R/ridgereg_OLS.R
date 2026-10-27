#' Ridge regression using least squares
#'
#' Fits a ridge regression model with standardised covariates.
#' The intercept is included but is not penalised.
#'
#' @param formula A formula specifying the response and covariates.
#'   The formula should include an intercept.
#' @param data A data frame containing the variables in the formula.
#' @param lambda A non-negative number specifying the ridge penalty.
#'   Larger values give more shrinkage of the covariate coefficients.
#'
#' @return An object of class \code{ridgereg}, containing the function call,
#'   ridge coefficients, fitted values, standardised design matrix,
#'   response vector and lambda.
#'
#' @details
#' The covariates are centred and divided by their sample standard deviations.
#' The returned coefficients therefore refer to the standardised covariates.
#' Covariates must have non-zero variance.
#' When lambda is zero, the design matrix must have full column rank.
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
ridgereg <- function(formula,data,lambda){
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
  
  # calculating the ridge coefficients
  ridge_coefs <- solve(crossprod(X)+lambda*I,crossprod(X,y))

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
