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
#' @param spar.freq (type: numeric) smoothing parameter, typically (but not
#' necessarily) in \eqn{(0,1]}.
#'
#' @param spar.time (type: numeric) smoothing parameter, typically (but not
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
#' @param palette.col (type: character) colors palette.
#'
#' @references
#' For more information on theoretical foundations and estimation methods see
#'
#' \insertRef{dahlhaus1997fitting}{lsts}
#'
#' \insertRef{dahlhaus1998optimal}{lsts}
#'
#' @examples
#' block.smooth.periodogram(malleco)
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
block.smooth.periodogram <- function(y, x = NULL, N = NULL, S = NULL, p = 0.25, spar.freq = 0, spar.time = 0, theta = 0, phi = 0, xlim = NULL, ylim = NULL, zlim = NULL, ylab = "Time", palette.col = NULL) {
  if (is.null(N)) {
    N <- trunc(length(y)^0.8)
  }
  
  if (is.null(S)) {
    S <- trunc(p * N)
  }
  
  M <- trunc((length(y) - N) / S + 1)
  
  aux <- sapply(seq_len(M),
                function(j) {
                  periodogram(y[(S * (j - 1) + 1):(S * (j - 1) + N)], plot = FALSE)$periodogram
                })
  
  lambda <- periodogram(y[(S * (M - 1) + 1):(S * (M - 1) + N)], plot = FALSE)$lambda

  aux2 <- sapply(seq_len(M),
                 function(j) {
                   smooth.spline(aux[, j], spar = spar.freq)$y
                  })

  aux3 <- t(sapply(seq_len(dim(aux)[1]), function(i) smooth.spline(aux2[i, ], spar = spar.time)$y))
  
  if (is.null(palette.col)) {
    palette.col <- c("green", "lightgreen", "yellow", "orange", "darkred")
  }
  
  jet.colors <- colorRampPalette(palette.col)
  nbcol <- 100
  color <- jet.colors(nbcol)
  facetcol <- cut(aux3[-1, -1] + aux3[-1, -ncol(aux3)] + aux3[-nrow(aux3), -1] + aux3[-nrow(aux3), -ncol(aux3)], nbcol)
  
  if (is.null(x)) {
    t <- S * (seq_len(M) - 1) + N / 2
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
    zlim <- range(aux3, na.rm = TRUE)
  }
  
  persp(x = lambda, y = t, z = aux3, theta = theta, phi = phi, col = color[facetcol], zlab = "Smooth Periodogram", xlab = "Frequency", ylab = ylab, expand = 0.5, ticktype = "detailed", ylim = ylim, zlim = zlim, xlim = xlim)
}
