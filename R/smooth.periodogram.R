#' @title Smoothing periodogram
#'
#' @description This function returns the smoothing periodogram of a stationary time serie,
#' its plot and its Fourier frequency.
#'
#' @details
#' \code{smooth.periodogram} computes the periodogram from \code{y} vector and then smooth
#' it with \emph{smoothing spline} method, which basically approximates a curve using a cubic
#' spline (see more details in \code{\link[stats]{smooth.spline}}). \eqn{\lambda} is the
#' Fourier frequency obtained through \code{\link{periodogram}}.
#'
#' It must have caution with the minimum length of \code{y}, because \code{smooth.spline} requires
#' the entered vector has at least length 4 and the length of \code{y} does not equal to the
#' length of the data of the periodogram that \code{smooth.spline} receives.
#'
#' If it presents problems with tol (\bold{tol}erance), see \code{\link[stats]{smooth.spline}}.
#'
#' @param y (type: numeric) data vector.
#'
#' @param plot (type: logical) logical argument which allows to plot the periodogram.
#'
#' @param spar (type: numeric) smoothing parameter, typically (but not necessarily) in \eqn{(0,1].}
#'
#' @examples
#' # Examples for CRAN checks:
#' # Executable in < 5 sec
#'
#' # AR(1) simulated
#' set.seed(1776)
#' ts.sim <- arima.sim(n = 1000, model = list(order = c(1, 0, 0), ar = 0.7))
#' periodogram(ts.sim)
#' aux <- smooth.periodogram(ts.sim, plot = FALSE, spar = .7)
#' lines(aux$smooth.periodogram ~ aux$lambda, lwd = 2, col = "orange")
#' lines(fdensity(ar = 0.7, lambda = aux$lambda) ~ aux$lambda, col = "red")
#' @return
#' ** COMPLETE **
#'
#' @seealso \code{\link{smooth.spline}}, \code{\link{periodogram}}
#'
#' @export

smooth.periodogram <- function(y, plot = TRUE, spar = 0) {
  aux <- periodogram(y, plot = FALSE)
  smooth.periodogram <- smooth.spline(aux$periodogram, spar = spar)$y
  lambda <- aux$lambda
  if (plot == TRUE) {
    plot(smooth.periodogram ~ lambda, bty = "n", las = 1, xlab = expression("Frequency"), ylab = expression("Smooth Periodogram"), xaxt = "n", type = "l")
    axis(1, at = seq(0, pi, pi / 4), labels = expression(0, pi / 4, pi / 2, 3 * pi / 4, pi))
  }
  return(list(smooth.periodogram = smooth.periodogram, lambda = lambda))
}
