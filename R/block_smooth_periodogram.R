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
#' @references
#' For more information on theoretical foundations and estimation methods see
#'
#' \insertRef{dahlhaus1997fitting}{lsts}
#'
#' \insertRef{dahlhaus1998optimal}{lsts}
#'
#' @examples
#' block_smooth_periodogram(malleco)
#' 
#' @return
#' ** COMPLETE **
#'
#' @seealso \code{\link{arima.sim}}
#'
#' @importFrom stats smooth.spline reshape
#' @importFrom ggplot2 ggplot aes after_stat geom_contour scale_color_viridis_c 
#'  labs theme_minimal
#'
#' @export
block_smooth_periodogram <- function(y, x = NULL, N = NULL, S = NULL, p = 0.25,
                                     spar.freq = 0, spar.time = 0, theta = 0,
                                     phi = 0) {
  len_y <- length(y)
  
  if (is.null(N)) {
    N <- trunc(len_y^0.8)
  }
  if (is.null(S)) {
    S <- trunc(p * N)
  }
  
  M <- trunc((len_y - N) / S + 1)
  aux <- matrix(NA, ncol = M, nrow = trunc(N / 2))
  
  for (j in 1:M) {
    aux[, j] <- periodogram(y[(S * (j - 1) + 1):(S * (j - 1) + N)], plot = FALSE)$periodogram
  }
  
  lambda <- periodogram(y[(S * (j - 1) + 1):(S * (j - 1) + N)], plot = FALSE)$lambda

  j <- seq_len(M)
  t <- S * (j - 1) + N / 2
  
  if (is.null(x)) {
    t <- t
  } else {
    t <- x[t]
  }
  
  d <- as.data.frame(aux)
  d$x <- lambda
  d <- reshape(d, 
                 direction = "long",
                 varying = list(names(d)[1:(ncol(d) - 1)]),
                 v.names = "z",
                 idvar = "y",
                 timevar = "key")
  d$y <- NULL
  
  d <- merge(x = d, y = data.frame(key = seq_along(t), y = t), by = "key", all.x = TRUE)
  d <- d[ , c("x","y", "z")]
  d <- round(d, 5)
 
  g <- ggplot(data = d, aes(x = x, y = y, z = z)) +
    geom_contour(binwidth = 0.005, aes(colour = stat(level))) + 
    scale_color_viridis_c(name = "Smooth Periodogram", option = "C") +
    labs(x = "Frequency", y = "Time", title = "Smooth Periodogram 2d Contours") +
    theme_minimal()
  
  return(g)
}

#' Smooth Periodogram by Blocks
#' @description \code{block_smooth_periodogram()} replaces this function
#' @param ... old parameters
#' @export
block.smooth.periodogram <- function(...) {
  .Deprecated("")
}
