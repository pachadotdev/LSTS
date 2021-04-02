#' @title Smooth Periodogram by Blocks
#' @description Plots the contour plot of the smoothing periodogram of a time series, by
#' blocks or windows.
#' @details The number of windows of the function is \eqn{m = \textrm{trunc}((n-N)/S+1)},
#' where \code{\link[base]{trunc}} truncates de entered value and \emph{n} is
#' the length of the vector \code{y}. All windows are of the same length
#' \code{N}, if this value isn't entered by user then is computed as
#' \eqn{N=\textrm{trunc}(n^{0.8})} (Dahlhaus).
#' \code{LSTS_spb} computes the periodogram in each of the
#' \emph{M} windows and then smoothes it two times with
#' \code{\link[stats]{smooth.spline}} function; the first time using
#' \code{spar.freq} parameter and the second time with \code{spar.time}. These
#' windows overlap between them.
#' @param y (type: numeric) data vector
#' @param x (type: numeric) optional vector, if \code{x = NULL} then the
#' function uses \eqn{(1,\ldots,n)} where \code{n} is the length of \code{y}.
#' @param N (type: numeric) value corresponding to the length of the window to
#' compute periodogram.
#' If \code{N=NULL} then the function will use
#' \eqn{N = \textrm{trunc}(n^{0.8})}, see
#' \insertCite{dahlhaus1998optimal;textual}{LSTS} where \eqn{n} is the length of
#' the \code{y} vector.
#' @param S (type: numeric) value corresponding to the lag with which will be
#' taking the blocks or windows to calculate the periodogram.
#' @param p (type: numeric) value used if it is desired that \code{S} is
#' proportional to \code{N}. By default \code{p=0.25}, if \code{S} and \code{N}
#' are not entered.
#' @param spar.freq (type: numeric) smoothing parameter, typically (but not
#' necessarily) in \eqn{(0,1]}.
#' @param spar.time (type: numeric) smoothing parameter, typically (but not
#' necessarily) in \eqn{(0,1]}.
#' @references
#' For more information on theoretical foundations and estimation methods see
#' \insertRef{dahlhaus1997fitting}{LSTS}
#' \insertRef{dahlhaus1998optimal}{LSTS}
#' @examples
#' block.smooth.periodogram(malleco)
#' @return
#' A ggplot object.
#' @seealso \code{\link{arima.sim}}
#' @importFrom stats smooth.spline reshape
#' @importFrom ggplot2 ggplot aes stat geom_contour scale_color_viridis_c
#'  labs theme_minimal
#' @export
block.smooth.periodogram <- function(y, x = NULL, N = NULL, S = NULL, p = 0.25,
                                     spar.freq = 0, spar.time = 0) {
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
    timevar = "key"
  )
  d$y <- NULL

  d <- merge(x = d, y = data.frame(key = seq_along(t), y = t), by = "key", all.x = TRUE)
  d <- d[, c("x", "y", "z")]
  d <- round(d, 5)

  g <- ggplot(data = d, aes(x = x, y = y, z = z)) +
    geom_contour(binwidth = 0.005, aes(colour = stat(level))) +
    scale_color_viridis_c(name = "Smooth\nPeriodogram", option = "C") +
    labs(x = "Frequency", y = "Time", title = "Smooth Periodogram 2D Contours") +
    theme_minimal()

  return(g)
}
