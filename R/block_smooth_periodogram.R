#' @title Smooth Periodogram by Blocks
#'
#' @description Plots the contour plot of the smoothing periodogram of a time series, by
#' blocks or windows.
#'
#' @details
#' The number of windows of the function is \eqn{m = \text{trunc}((n-w)/s+1)},
#' where \code{\link[base]{trunc}} truncates de entered value and \emph{n} is
#' the length of the vector \code{y}. All windows are of the same length
#' \code{w}, if this value isn't entered by user then is computed as
#' \eqn{w=\text{trunc}(n^{0.8})} (Dahlhaus).
#'
#' \code{lsts2_spb} computes the periodogram in each of the
#' \emph{M} windows and then smoothes it two times with
#' \code{\link[stats]{smooth.spline}} function; the first time using
#' \code{spar_freq} parameter and the second time with \code{spar_time}. These
#' windows overlap between them.
#'
#' @param y (type: numeric) data vector
#'
#' @param x (type: numeric) optional vector, if \code{x = NULL} then the
#' function uses \eqn{(1,\ldots,n)} where \code{n} is the length of \code{y}.
#'
#' @param w (type: numeric) value corresponding to the length of the window to
#' compute periodogram.
#' If \code{w=NULL} then the function will use
#' \eqn{w = \text{trunc}(n^{0.8})}, see
#' \insertCite{dahlhaus1998optimal;textual}{lsts2} where \eqn{n} is the length of
#' the \code{y} vector.
#'
#' @param s (type: numeric) value corresponding to the lag with which will be
#' taking the blocks or windows to calculate the periodogram.
#'
#' @param p (type: numeric) value used if it is desired that \code{s} is
#' proportional to \code{w}. By default \code{p=0.25}, if \code{s} and \code{w}
#' are not entered.
#'
#' @param spar_freq (type: numeric) smoothing parameter, typically (but not
#' necessarily) in \eqn{(0,1]}.
#'
#' @param spar_time (type: numeric) smoothing parameter, typically (but not
#' necessarily) in \eqn{(0,1]}.
#'
#' @references
#' For more information on theoretical foundations and estimation methods see
#'
#' \insertRef{dahlhaus1997fitting}{lsts2}
#'
#' \insertRef{dahlhaus1998optimal}{lsts2}
#'
#' @examples
#' block_smooth_periodogram(malleco)
#' @return
#' A ggplot object.
#'
#' @seealso \code{\link{arima.sim}}
#'
#' @importFrom stats smooth.spline reshape
#' @importFrom ggplot2 ggplot aes stat geom_contour scale_color_viridis_c
#'  labs theme_minimal
#'
#' @export
block_smooth_periodogram <- function(y, x = NULL, w = NULL, s = NULL, p = 0.25,
                                     spar_freq = 0, spar_time = 0) {
  len_y <- length(y)

  if (is.null(w)) {
    w <- trunc(len_y^0.8)
  }
  if (is.null(s)) {
    s <- trunc(p * w)
  }

  M <- trunc((len_y - w) / s + 1)
  aux <- matrix(NA, ncol = M, nrow = trunc(w / 2))

  for (j in 1:M) {
    aux[, j] <- periodogram(y[(s * (j - 1) + 1):(s * (j - 1) + w)], plot = FALSE)$periodogram
  }

  lambda <- periodogram(y[(s * (j - 1) + 1):(s * (j - 1) + w)], plot = FALSE)$lambda

  j <- seq_len(M)
  t <- s * (j - 1) + w / 2

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
