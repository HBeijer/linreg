#' MULTIPLE LINEAR REGRESSION WITH OLS
#'
#' This function is used to estimate a multiple linear regression model using the
#' OLS method for estimating the model parameters.
#'
#' @param formula is used to specify which regression model you wish to estimate.
#' @param data should be the data frame which you want to estimate the parameters from.
#'
#' @return The object \code{linreg} is returned in the last lines of codes. This includes
#' the estimated parameters such as the coefficients, fitted values, residuals, degrees of freedom
#' residual variance, variance of the coefficients, tvalues and pvalues.
#'
#' @export

linreg <- function(formula,data){
  model_call <- match.call()
  # We begin by getting det design matrix (intercept and variables) and also the
  # responsvariable.
  X <- stats::model.matrix(formula,data)
  y <- as.matrix(data[all.vars(formula)[1]])
  # REGRESSION COEFFICIENTS
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  # THE FITTED VALUES
  y_hat <- X %*% beta_hat
  # THE RESIDUALS
  e_hat <- y - y_hat
  # THE DEGREES OF FREEDOM
  n <- nrow(data)
  p <- ncol(X)
  df <- n-p
  # THE RESIDUAL VARIANCE
  sigma2_hat <- as.numeric((t(e_hat) %*% e_hat) / df)
  # THE VARIANCE OF THE REGRESSION COEFS (obs covmatrix)
  variance_of_Beta <- sigma2_hat * solve( t(X) %*% X)
  # T VALUES FOR EACH COEFS (use diag for bcs of the covariance matrix)
  t_value <- as.numeric(beta_hat) / sqrt(diag(variance_of_Beta))
  # P VALUES FOR T TEST
  p_values <- 2 * (1- stats::pt(abs(t_value),df=df))
  # SAVING EVERYTHING IN A LIST
  linreg <- list(
    call = model_call,
    beta_hat = beta_hat,
    variance_of_Beta = variance_of_Beta,
    t = t_value,
    p = p_values,
    y_hat = y_hat,
    e_hat = e_hat,
    df = df,
    sigma2_hat = sigma2_hat,
    X = X
  )
  # CHANGING FROM CLASS "list" TO "linreg"
  class(linreg) <- "linreg"
  # RETURN!
  return(linreg)
}

