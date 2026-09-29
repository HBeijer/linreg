#' The method coef() is used to obtain the coefficients which was estimated in the model
#'
#' @param x is the object of the class of which the model is saved to.
#' @export

coef.linreg <- function(x){
  # Wrote the code in a dumb way from the beginning, change some things for it to
  # actually be a vector...
  coefficients <- as.vector(model$beta_hat)
  names(coefficients) <- rownames(model$beta_hat)

  return(coefficients)
}
