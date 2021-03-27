#' @title Smoothing periodogram
#' @description This function returns the smoothing periodogram of a stationary
#' time serie, its plot and its Fourier frequency.
#' @details
#' \code{smooth_periodogram} computes the periodogram from \code{y} vector and
#' then smooth it with \emph{smoothing spline} method, which basically
#' approximates a curve using a cubic spline (see more details in
#' \code{\link[stats]{smooth.spline}}). \eqn{\lambda} is the Fourier frequency
#' obtained through \code{\link{periodogram}}.
#' It must have caution with the minimum length of \code{y}, because
#' \code{smooth.spline} requires the entered vector has at least length 4 and
#' the length of \code{y} does not equal to the length of the data of the
#' periodogram that \code{smooth.spline} receives.
#' If it presents problems with tol (\bold{tol}erance), see
#' \code{\link[stats]{smooth.spline}}.
#' @param y (type: numeric) data vector.
#' @param plot (type: logical) logical argument which allows to plot the
#' periodogram. Defaults to TRUE.
#' @param spar (type: numeric) smoothing parameter, typically (but not
#' necessarily) in \eqn{(0,1].}
#' @examples
#' # AR(1) simulated
#' require(ggplot2)
#' set.seed(1776)
#' ts.sim <- arima.sim(n = 1000, model = list(order = c(1, 0, 0), ar = 0.7))
#' per <- periodogram(ts.sim)
#' aux <- smooth_periodogram(ts.sim, plot = FALSE, spar = .7)
#' sm_p <- data.frame(x = aux$lambda, y = aux$smooth_periodogram)
#' sp_d <- data.frame(
#'   x = aux$lambda,
#'   y = spectral_density(ar = 0.7, lambda = aux$lambda)
#' )
#' g <- per$plot
#' g +
#'   geom_line(data = sm_p, aes(x, y), color = "#ff7f0e") +
#'   geom_line(data = sp_d, aes(x, y), color = "#d31244")
#' @return A list with with the smooth periodogram and the lambda values
#' @seealso \code{\link{smooth.spline}}, \code{\link{periodogram}}
#' @importFrom stats smooth.spline
#' @importFrom ggplot2 geom_line
#' @importFrom scales math_format
#' @export
smooth_periodogram <- function(y, plot = TRUE, spar = 0) {
  aux <- periodogram(y, plot = FALSE)
  smooth_periodogram <- smooth.spline(aux$periodogram, spar = spar)$y
  lambda <- aux$lambda
  if (plot == TRUE) {
    pi_scales <- math_format(.x * pi, format = function(x) x / pi)
    g <- ggplot(data.frame(x = lambda, y = smooth_periodogram)) +
      geom_line(aes(x = as.numeric(x), y = as.numeric(y)), color = "#1f77b4") +
      labs(x = "Frequency", y = "Smooth Periodogram") +
      scale_x_continuous(labels = pi_scales, breaks = seq(0, pi, pi / 4)) +
      theme_minimal()
  } else {
    g <- NULL
  }
  return(list(smooth_periodogram = smooth_periodogram, lambda = lambda, plot = g))
}
