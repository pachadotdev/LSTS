#' @title Summary for Locally Stationary Time Series
#'
#' @description Function used to produce summaries of the results to Whittle
#' estimator to Locally Stationary Time Series (\code{\link{lsts_wl}}
#' function).
#'
#' @details
#' ** COMPLETE **
#'
#' @param object (type: COMPLETE) \code{\link{lsts_wl}} function
#'
#' @examples
#' # Examples for CRAN checks:
#' # Executable in < 5 sec
#' @return
#' A list with the following components:
#' \item{summary}{a resume table with estimate, std. error, z-value and p-value
#' of the model.}
#' \item{aic}{AIC of the model.}
#' \item{npar}{number of parameters in the model.}
#'
#' @importFrom stats pnorm
#'
#' @export

lsts_summary <- function(object) {
  aux1 <- object$coef

  aux2 <- sqrt(diag(object$var.coef))

  aux3 <- aux1 / aux2

  aux4 <- 2 * (1 - stats::pnorm(abs(aux3)))

  Table <- cbind(aux1, aux2, aux3, aux4)

  colnames(Table) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")

  list(summary = round(Table, 4), aic = object$aic, npar = length(aux1))
}
