#' The method resid() is used to obtain the residuals which was estimated in the model
#'
#' @param x is the object of the class of which the model is saved to.
#' @export

resid.linreg <- function(x){
  return(x$e_hat)
}
