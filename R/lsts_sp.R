#' @title Smoothing periodogram
#'
#' @description This function returns the smoothing periodogram of a stationary
#' time serie, its plot and its Fourier frequency.
#'
#' @details
#' \code{lsts_sp} computes the periodogram from \code{y} vector and
#' then smooth it with \emph{smoothing spline} method, which basically
#' approximates a curve using a cubic spline (see more details in
#' \code{\link[stats]{smooth.spline}}). \eqn{\lambda} is the Fourier frequency
#' obtained through \code{\link{lsts_periodogram}}.
#'
#' It must have caution with the minimum length of \code{y}, because
#' \code{smooth.spline} requires the entered vector has at least length 4 and
#' the length of \code{y} does not equal to the length of the data of the
#' periodogram that \code{smooth.spline} receives.
#'
#' If it presents problems with tol (\bold{tol}erance), see
#' \code{\link[stats]{smooth.spline}}.
#'
#' @param y (type: numeric) data vector.
#'
#' @param plot (type: logical) logical argument which allows to plot the
#' periodogram.
#'
#' @param spar (type: numeric) smoothing parameter, typically (but not
#' necessarily) in \eqn{(0,1].}
#'
#' @examples
#' # Examples for CRAN checks
#' # Executable in < 5 sec
#'
#' # AR(1) simulated
#'
#' set.seed(1776)
#'
#' ts.sim <- arima.sim(n = 1000, model = list(order = c(1, 0, 0), ar = 0.7))
#'
#' lsts_periodogram(ts.sim)
#'
#' aux <- lsts_sp(ts.sim, plot = FALSE, spar = .7)
#'
#' lines(aux$lsts_sp ~ aux$lambda, lwd = 2, col = "orange")
#'
#' lines(lsts_sd(ar = 0.7, lambda = aux$lambda) ~ aux$lambda,
#'   col = "red"
#' )
#' @return
#' ** COMPLETE **
#'
#' @seealso \code{\link{smooth.spline}}, \code{\link{lsts_periodogram}}
#'
#' @importFrom stats smooth.spline
#' @importFrom graphics axis
#'
#' @export

lsts_sp <- function(y, plot = TRUE, spar = 0) {
  aux <- lsts_periodogram(y, plot = FALSE)

  lsts_sp <- stats::smooth.spline(aux$periodogram, spar = spar)$y

  lambda <- aux$lambda

  if (plot == TRUE) {
    plot(lsts_sp ~ lambda,
      bty = "n", las = 1,
      xlab = "Frequency", ylab = "Smooth Periodogram",
      xaxt = "n", type = "l"
    )

    graphics::axis(1,
      at = seq(0, pi, pi / 4),
      labels = expression(0, pi / 4, pi / 2, 3 * pi / 4, pi)
    )
  }

  return(list(lsts_sp = lsts_sp, lambda = lambda))
}
