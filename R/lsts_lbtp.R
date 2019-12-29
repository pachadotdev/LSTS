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
#' lsts_lbtp(z, lag = 15)
#' lsts_diagnostics(z)
#' @return
#' ** COMPLETE **
#'
#' @seealso \code{\link{lsts_periodogram}}, \code{\link[graphics]{persp}}
#'
#' @importFrom stats acf na.pass
#' @importFrom graphics plot abline
#'
#' @export

lsts_lbtp <- function(z, lag = 10, main = NULL) {
  # sanity checks ----
  if (!is.numeric(z)) {
    stop("z must be numeric")
  }

  if (lag <= 0 | lag != as.integer(lag)) {
    stop("lag must be a positive integer")
  }

  if (is.null(main)) {
    main <- "P-Values for Ljung-Box Statistic"
  } else {
    if (!is.character(main)) {
      stop("main must be character")
    }
  }

  n <- length(z)

  aux <- stats::acf(z, plot = FALSE, lag.max = lag, na.action = stats::na.pass)

  iteration_seq <- 1:max(1, lag)

  p_value <- sapply(
    iteration_seq,
    function(i) {
      rho <- aux$acf[2:(i + 1), , 1]
      Q <- sum(n * (n + 2) * rho^2 / (n - 1:i))
      p_value <- 1 - stats::pchisq(Q, df = i)
      return(p_value)
    }
  )

  graphics::plot(p_value ~ iteration_seq,
    ylim = c(0, 1), bty = "n", las = 1, lwd = 2,
    xlim = c(0, lag), main = main, xlab = "Lag", ylab = "p-value", pch = 20
  )

  return(
    graphics::abline(h = 0.05, lty = 2, col = "blue")
  )
}
