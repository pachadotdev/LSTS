#' @title Diagnostic Plots for Time Series fits
#'
#' @description Plot time-series diagnostics.
#'
#' @details
#' This function plot the residuals, the autocorrelation function of the residuals (ACF) and the p-values
#' of the Ljung-Box Test for all lags up to \code{lag}.
#'
#' @param x (type: numeric) residuals of the fitted time series model.
#'
#' @param lag (type: numeric) maximum lag at which to calculate the acf and Ljung-Box test.
#' By default set to 10.
#'
#' @param cex (type: numeric) optional argument of \code{\link[graphics]{par}}.
#' By default set to 0.5.
#'
#' @examples
#' # Examples for CRAN checks:
#' # Executable in < 5 sec
#'
#' z <- rnorm(500)
#' Box.Ljung.Test(z, lag = 15)
#' ts.diag(z)
#' @return
#' ** COMPLETE **
#'
#' @seealso \code{\link{Box.Ljung.Test}}
#'
#' @export

ts.diag <- function(x, lag = 10, cex = 0.5) {
  Z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

  op <- par(mfrow = c(3, 1), cex = cex)

  plot(Z ~ time(Z),
    ylim = c(-3.5, 3.5), type = "h", lwd = 1,
    main = expression("Standardized Residuals"), xlab = "", ylab = "", bty = "n", las = 1
  )

  abline(h = 0)
  abline(h = +2, col = 4, lwd = 1, lty = 2)
  abline(h = -2, col = 4, lwd = 1, lty = 2)
  abline(h = +3, col = 4, lwd = 1, lty = 3)
  abline(h = -3, col = 4, lwd = 1, lty = 3)

  acf(x,
    lag.max = lag, main = expression("ACF of Residuals"), lwd = 1, las = 1, col = 1,
    bty = "n", na.action = na.pass
  )

  Box.Ljung.Test(x, lag = lag)

  return(par(op))
}
