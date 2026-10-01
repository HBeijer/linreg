#' Multiple linear regression with OLS
#'
#' Estimates a multiple linear regression model using QR decomposition.
#'
#' @param formula A formula specifying the regression model.
#' @param data A data frame containing the model variables.
#'
#' @return An object of class \code{linreg}, containing coefficients,
#' fitted values, residuals, residual degrees of freedom, residual variance,
#' the coefficient covariance matrix, t-values and p-values.
#'
#' @export

linreg <- function(formula, data) {
  model_call <- match.call()

  # Build X and y from the same observations.
  model_data <- stats::model.frame(formula, data)
  X <- stats::model.matrix(formula, model_data)
  y <- as.matrix(stats::model.response(model_data))

  # Number of observations, coefficients and residual degrees of freedom.
  n <- nrow(X)
  p <- ncol(X)
  df <- n - p

  if (df <= 0) stop("More observations than coefficients are required.")

  # QR decomposition stores the factors in X = QR.
  # Q has orthonormal columns (Q'Q = I), and R is upper triangular.
  QR <- qr(X)

  # Full column rank is needed for unique coefficients and invertible R.
  if (QR$rank < p) stop("The design matrix must have full column rank.")

  # Substituting X = QR into the normal equations gives:
  # R'R beta_hat = R'Q'y, which simplifies to R beta_hat = Q'y.
  # qr.coef() solves this by back substitution without forming X'X.
  beta_hat <- qr.coef(QR, y)

  # Fitted values and residuals.
  y_hat <- X %*% beta_hat
  e_hat <- y - y_hat

  # Estimate the residual variance: sum of squared residuals / df.
  sigma2_hat <- as.numeric(crossprod(e_hat) / df)

  # Since X'X = R'Q'QR = R'R, the estimated covariance matrix is
  # sigma2_hat * (R'R)^(-1).
  # We extract R, then use chol2inv(R) to calculate (R'R)^(-1).
  R <- qr.R(QR)
  variance_of_Beta <- sigma2_hat * chol2inv(R)

  # Default qr() preserves column order for full column rank.
  dimnames(variance_of_Beta) <- list(colnames(X), colnames(X))

  # Standard errors are the square roots of the covariance diagonal.
  standard_errors <- sqrt(diag(variance_of_Beta))
  t_value <- as.numeric(beta_hat) / standard_errors

  # Two-sided p-values for testing whether each coefficient is zero.
  p_values <- 2 * stats::pt(abs(t_value), df = df, lower.tail = FALSE)

  # Store the results and assign the linreg class.
  result <- list(
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

  class(result) <- "linreg"
  return(result)
}
