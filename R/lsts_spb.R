#' @title Smooth Periodogram by Blocks
#'
#' @description Plots in 3D the smoothing periodogram of a time series, by
#' blocks or windows.
#'
#' @details
#' The number of windows of the function is \eqn{M = \textmd{trunc}((n-N)/S+1)},
#' where \code{\link[base]{trunc}} truncates de entered value and \emph{n} is
#' the length of the vector \code{y}. All windows are of the same length
#' \code{N}, if this value isn't entered by user then is computed as
#' \eqn{N=\textmd{trunc}(n^{0.8})} (Dahlhaus).
#'
#' \code{lsts_spb} computes the periodogram in each of the
#' \emph{M} windows and then smoothes it two times with
#' \code{\link[stats]{smooth.spline}} function; the first time using
#' \code{spar_freq} parameter and the second time with \code{spar_time}. These
#' windows overlap between them.
#'
#' The surface is then viewed by looking at the origin from a direction defined
#' by \code{theta} and \code{phi}. If \code{theta} and \code{phi} are both zero
#' the viewing direction is directly down the negative y axis. Changing
#' \code{theta} will vary the azimuth and changing \code{phi} the colatitude.
#'
#' @param y (type: numeric) data vector
#'
#' @param x (type: numeric) optional vector, if \code{x = NULL} then the
#' function uses \eqn{(1,\ldots,n)} where \code{n} is the length of \code{y}.
#' More details in \code{y} argument from \code{\link[graphics]{persp}}
#' function.
#'
#' @param N (type: numeric) value corresponding to the length of the window to
#' compute periodogram.
#' If \code{N=NULL} then the function will use
#' \eqn{N = \textmd{trunc}(n^{0.8})}, see
#' \insertCite{dahlhaus1998optimal;textual}{lsts} where \eqn{n} is the length of
#' the \code{y} vector.
#'
#' @param S (type: numeric) value corresponding to the lag with which will be
#' taking the blocks or windows to calculate the periodogram.
#'
#' @param p (type: numeric) value used if it is desired that \code{S} is
#' proportional to \code{N}. By default \code{p=0.25}, if \code{S} and \code{N}
#' are not entered.
#'
#' @param spar_freq (type: numeric) smoothing parameter, typically (but not
#' necessarily) in \eqn{(0,1]}.
#'
#' @param spar_time (type: numeric) smoothing parameter, typically (but not
#' necessarily) in \eqn{(0,1]}.
#'
#' @param theta (type: numeric) angle defining the viewing direction, gives the
#' azimuthal direction.
#'
#' @param phi (type: numeric) angle defining the viewing direction, gives the
#' colatitude.
#'
#' @param xlim (type: numeric) x-axis limits, \code{NULL} by default and
#' optional parameter.
#'
#' @param ylim (type: numeric) y-axis limits, \code{NULL} by default and
#' optional parameter.
#'
#' @param zlim (type: numeric) z-axis limits, \code{NULL} by default and
#' optional parameter.
#'
#' @param ylab (type: character) title for the y-axis. By default is
#' \code{ylab='Time'} and is an optional
#' parameter.
#'
#' @param palette_col (type: character) colors palette.
#'
#' @references
#' For more information on theoretical foundations and estimation methods see
#'
#' \insertRef{dahlhaus1997fitting}{lsts}
#'
#' \insertRef{dahlhaus1998optimal}{lsts}
#'
#' @examples
#' set.seed(1776)
#' ts.sim <- arima.sim(
#'   n = 1000, model = list(order = c(2, 0, 0), ar = c(1.3, -0.6))
#' )
#' @return
#' ** COMPLETE **
#'
#' @seealso \code{\link{arima.sim}}
#'
#' @importFrom stats smooth.spline
#' @importFrom graphics persp
#' @importFrom grDevices colorRampPalette
#'
#' @export

lsts_spb <- function(y, x = NULL, N = NULL, S = NULL, p = 0.25,
                     spar_freq = 0, spar_time = 0, theta = 0, phi = 0, xlim = NULL, ylim = NULL,
                     zlim = NULL, ylab = "Time", palette_col = NULL) {
  T. <- length(y)

  if (is.null(N)) {
    N <- trunc(T.^0.8)
  }

  if (is.null(S)) {
    S <- trunc(p * N)
  }

  M <- trunc((T. - N) / S + 1)

  aux <- matrix(NA, ncol = M, nrow = trunc(N / 2))

  for (j in 1:M) {
    aux[, j] <- lsts_periodogram(y[(S * (j - 1) + 1):(S * (j - 1) + N)],
      plot = FALSE
    )$periodogram
  }

  lambda <- lsts_periodogram(y[(S * (j - 1) + 1):(S * (j - 1) + N)],
    plot = FALSE
  )$lambda

  aux2 <- aux

  for (j in 1:M) {
    aux2[, j] <- stats::smooth.spline(aux[, j], spar = spar_freq)$y
  }

  aux3 <- aux

  for (i in 1:(dim(aux)[1])) {
    aux3[i, ] <- stats::smooth.spline(aux2[i, ], spar = spar_time)$y
  }

  aux <- aux3

  nrz <- nrow(aux)

  ncz <- ncol(aux)

  if (is.null(palette_col)) {
    palette_col <- c("green", "lightgreen", "yellow", "orange", "darkred")
  }

  jet.colors <- grDevices::colorRampPalette(palette_col)

  nbcol <- 100

  color <- jet.colors(nbcol)

  z <- aux

  zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]

  facetcol <- cut(zfacet, nbcol)

  j <- 1:M

  t <- S * (j - 1) + N / 2

  if (is.null(x)) {
    t <- t
  } else {
    t <- x[t]
  }

  if (is.null(xlim)) {
    xlim <- range(lambda)
  }

  if (is.null(ylim)) {
    ylim <- range(t)
  }

  if (is.null(zlim)) {
    zlim <- range(aux, na.rm = TRUE)
  }

  return(
    graphics::persp(
      x = lambda, y = t, z = aux, theta = theta, phi = phi,
      col = color[facetcol], zlab = "Smooth Periodogram", xlab = "Frequency",
      ylab = ylab, expand = 0.5, ticktype = "detailed", ylim = ylim,
      zlim = zlim, xlim = xlim
    )
  )
}
