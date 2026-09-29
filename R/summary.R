#' Summarize a linreg model
#'
#' Creates a summary containing the estimated coefficients, standard errors,
#' t-values, p-values, residual standard error and degrees of freedom.
#'
#' @param object An object of class \code{linreg}.
#' @param ... Additional arguments.
#'
#' @return An object of class \code{summary.linreg}.
#' @export
summary.linreg <- function(object, ...) {
  # Check that object belongs to the linreg class
  if (!inherits(object, "linreg")) stop("object must be an object of class 'linreg'")

  # Convert the coefficient matrix into a regular numeric vector
  estimates <- as.numeric(object$beta_hat)

  # Obtain the coefficient names from the rows of beta_hat
  coefficient_names <- rownames(object$beta_hat)

  # Use the column names of the design matrix if beta_hat has no row names
  if (is.null(coefficient_names)) coefficient_names <- colnames(object$X)

  # Calculate the standard error of each coefficient.
  # The diagonal of the covariance matrix contains the coefficient variances.
  standard_errors <- sqrt(diag(object$variance_of_Beta))

  # Extract the previously calculated t-values and p-values
  t_values <- as.numeric(object$t)
  p_values <- as.numeric(object$p)

  # Combine the coefficient statistics into one matrix
  coefficient_table <- cbind(
    Estimate = estimates,
    `Std. Error` = standard_errors,
    `t value` = t_values,
    `Pr(>|t|)` = p_values
  )

  # Add the coefficient names as row names
  rownames(coefficient_table) <- coefficient_names

  # Store the information needed for the printed summary
  summary_object <- list(
    call = object$call,
    coefficients = coefficient_table,

    # sigma2_hat is the residual variance, so its square root is
    # the residual standard error
    sigma = sqrt(object$sigma2_hat),

    # Residual degrees of freedom
    df = object$df
  )

  # Give the result its own S3 class
  class(summary_object) <- "summary.linreg"

  return(summary_object)
}


#' Print a summary of a linreg model
#'
#' Displays the model call, coefficient table, residual standard error
#' and residual degrees of freedom.
#'
#' @param x An object of class \code{summary.linreg}.
#' @param digits The number of significant digits to display. Default is 4.
#' @param ... Additional arguments.
#'
#' @return Invisibly returns the supplied \code{summary.linreg} object.
#' @export
print.summary.linreg <- function(x, digits = 4, ...) {

  # Display the original call to linreg()
  cat("Call:\n")
  print(x$call)

  # Display the coefficient statistics
  cat("\nCoefficients:\n")
  print(x$coefficients, digits = digits)

  # Display the residual standard error and degrees of freedom
  cat("\nResidual standard error:",
      format(x$sigma, digits = digits), # cat() does not accept a digits-argument, so we have to do this
      "on", x$df, "degrees of freedom\n")

  # Return the summary object without printing it again
  invisible(x)
}
