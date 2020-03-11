#' @title Summary for Locally Stationary Time Series
#'
#' @description Function used to produce summaries of the results to Whittle
#' estimator to Locally Stationary Time Series (\code{\link{LS.whittle}}
#' function).
#'
#' @details
#' ** COMPLETE **
#'
#' @param object (type: COMPLETE) \code{\link{LS.whittle}} function
#'
#' @examples
#' fit_whittle <- ls_whittle(
#'   series = malleco, start = c(1, 1, 1, 1),
#'   order = c(p = 1, q = 0), ar.order = 1, sd.order = 1, N = 180, n.ahead = 10
#' )
#'
#' ls_summary(fit_whittle)
#' 
#' @return
#' **COMPLETE**
#' 
#' A list with the following components:
#' \item{summary}{a resume table with estimate, std. error, z-value and p-value
#' of the model.}
#' \item{aic}{AIC of the model.}
#' \item{npar}{number of parameters in the model.}
#'
#' @seealso \code{\link{ls_whittle}}
#'
#' @importFrom stats pnorm
#'
#' @export
ls_summary <- function(object) {
  aux1 <- object$coef
  aux2 <- sqrt(diag(object$var.coef))
  aux3 <- aux1 / aux2
  aux4 <- 2 * (1 - pnorm(abs(aux3)))
  Table <- cbind(aux1, aux2, aux3, aux4)
  colnames(Table) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
  list(summary = round(Table, 4), aic = object$aic, npar = length(aux1))
}

#' Summary for Locally Stationary Time Series
#' @description \code{ls_summary()} replaces this function
#' @param ... old parameters
#' @export
LS.summary <- function(...) {
  .Deprecated("")
}
