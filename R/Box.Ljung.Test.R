#' @title Ljung-Box Test Plot
#'
#' @description Plots the p-values Ljung-Box test.
#'
#' @details
#' The Ljung-Box test is used to check if exists autocorrelation in a time series. The statistic is
#' \deqn{Q = n(n+2)\cdot\sum_{j=1}^h \hat{\rho}(j)^2/(n-j)} with \emph{n} the number of observations
#' and \eqn{\hat{\rho}(j)} the autocorrelation coefficient in the sample when the lag is \emph{j}.
#' \code{Box.Ljung.Test} computes \eqn{Q} and returns the p-values graph with lag \emph{j}.
#'
#' @param y (type: numeric): data vector
#'
#' @param x (type: numeric): optional vector, if \code{x = NULL} then the function uses
#' \eqn{(1,\ldots,n)} where \code{n} is the length of \code{y}. More details in \code{y} argument
#' from \code{\link[graphics]{persp}} function.
#'
#' @param n (type: numeric): value corresponding to the length of the window to compute periodogram.
#' If \code{N=NULL} then the function will use \eqn{N = \textmd{trunc}(n^{0.8})}, see
#' \insertCite{dahlhaus1998optimal;textual}{lsts} where \eqn{n} is the length of the \code{y} vector.
#'
#' @param s (type: numeric): value corresponding to the lag with which will be taking the blocks or
#' windows to calculate the periodogram.
#'
#' @param p (type: numeric): value used if it is desired that \code{S} is proportional to \code{N}.
#' By default \code{p=0.25}, if \code{S} and \code{N} are not entered.
#'
#' @param spar.freq (type: numeric) smoothing parameter, typically (but not necessarily) in \eqn{(0,1]}.
#'
#' @param spar.time (type: numeric) smoothing parameter, typically (but not necessarily) in \eqn{(0,1]}.
#'
#' @param theta (type: numeric) angle defining the viewing direction, ives the azimuthal direction.
#'
#' @param phi (type: numeric) angle defining the viewing direction, gives the colatitude.
#'
#' @param xlim (type: numeric) x-axis limits, \code{NULL} by default and optional parameter.
#'
#' @param ylim (type: numeric) y-axis limits, \code{NULL} by default and optional parameter.
#'
#' @param zlim (type: numeric) z-axis limits, \code{NULL} by default and optional parameter.
#'
#' @param ylab (type: character) title for the y-axis. By default is \code{ylab="Time"} and is an
#' optional parameter.
#'
#' @param palette.col (type: character) colors palette.
#'
#' @references
#' For more information on theoretical foundations and estimation methods see
#'
#' \insertRef{brockwell2002introduction}{lsts}
#'
#' \insertRef{ljung1978measure}{lsts}
#'
#' @examples
#' # Example for CRAN checks:
#' # Executable in < 5 sec
#'
#' z <- rnorm(500)
#' Box.Ljung.Test(z, lag = 15)
#' ts.diag(z)
#' @return
#' ** COMPLETE **
#'
#' @seealso \code{\link{periodogram}}, \code{\link[graphics]{persp}}
#'
#' @importFrom stats acf na.pass
#' @importFrom graphics plot abline
#' 
#' @export

Box.Ljung.Test <- function(z, lag = NULL, main = NULL) {
  if (is.null(lag)) {
    lag <- 10
  }

  k <- lag

  n <- length(z)

  aux <- stats::acf(z, plot = FALSE, lag.max = k, na.action = stats::na.pass)

  p.value <- vector("numeric")

  Q <- vector("numeric")

  for (j in 1:k) {
    rho <- aux$acf[2:(j + 1), , 1]
    Q[j] <- sum(n * (n + 2) * rho^2 / (n - 1:j))
    p.value[j] <- 1 - stats::pchisq(Q[j], df = j)
  }

  if (is.null(main)) {
    main <- expression("p values for Ljung-Box statistic")
  }

  graphics::plot(p.value ~ c(1:k), ylim = c(0, 1), bty = "n", las = 1, lwd = 2, xlim = c(0, k), main = main, xlab = "Lag", ylab = "p-value", pch = 20)

  return(
    graphics::abline(h = 0.05, lty = 2, col = "blue")
  )
}
