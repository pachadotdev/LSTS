#' @title Summary for Locally Stationary Time Series
#'
#' @description Produces a summary of the results to Whittle
#' estimator to Locally Stationary Time Series (\code{\link{ls_whittle}}
#' function).
#'
#' @details
#' Calls the output from \code{\link{ls_whittle}} and computes the standard
#' error and p-values to provide a detailed summary.
#'
#' @param object (type: list) the output of \code{\link{ls_whittle}} function
#'
#' @examples
#' fit_whittle <- ls_whittle(
#'   series = malleco, start = c(1, 1, 1, 1),
#'   order = c(p = 1, q = 0), ar_order = 1, sd_order = 1, w = 180, n_ahead = 10
#' )
#'
#' ls_summary(fit_whittle)
#' @return
#' A list with the estimates, AIC and number of parameters
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
