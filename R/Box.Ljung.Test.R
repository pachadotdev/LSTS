#' @title Ljung-Box Test Plot
#'
#' @description Plots the p-values Ljung-Box test.
#'
#' @details
#' The Ljung-Box test is used to check if exists autocorrelation in a time
#' series. The statistic is
#' \deqn{Q = n(n+2)\cdot\sum_{j=1}^h \hat{\rho}(j)^2/(n-j)} with \emph{n} the
#' number of observations and \eqn{\hat{\rho}(j)} the autocorrelation
#' coefficient in the sample when the lag is \emph{j}. \code{lsts_lbtp}
#' computes \eqn{Q} and returns the p-values graph with lag \emph{j}.
#'
#' @param z COMPLETE
#' @param lag COMPLETE
#' @param main COMPLETE
#'
#' @references
#' For more information on theoretical foundations and estimation methods see
#'
#' \insertRef{brockwell2002introduction}{lsts}
#'
#' \insertRef{ljung1978measure}{lsts}
#'
#' @examples
#' z <- rnorm(500)
#' Box.Ljung.Test(z, lag = 15)
#' 
#' @return
#' ** COMPLETE **
#'
#' @seealso \code{\link{periodogram}}, \code{\link[graphics]{persp}}
#'
#' @importFrom stats acf na.pass pchisq
#' @importFrom graphics plot abline
#'
#' @export
Box.Ljung.Test <- function(z, lag = NULL, main = NULL) {
  if (is.null(lag)) {
    lag <- 10
  }
  k <- lag
  n <- length(z)
  aux <- acf(z, plot = FALSE, lag.max = k, na.action = na.pass)
  p.value <- vector("numeric")
  Q <- vector("numeric")
  for (j in 1:k) {
    rho <- aux$acf[2:(j + 1), , 1]
    Q[j] <- sum(n * (n + 2) * rho^2 / (n - 1:j))
    p.value[j] <- 1 - pchisq(Q[j], df = j)
  }
  if (is.null(main)) {
    main <- expression("p values for Ljung-Box statistic")
  }
  plot(p.value ~ c(1:k), ylim = c(0, 1), bty = "n", las = 1, lwd = 2, xlim = c(0, k), main = main, xlab = "Lag", ylab = "p-value", pch = 20)
  abline(h = 0.05, lty = 2, col = "blue")
}
