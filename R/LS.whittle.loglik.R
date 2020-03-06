#' @title Locally Stationary Whittle log-likelihood Function
#'
#' @description This function computes Whittle estimator for LS-ARMA and
#' LS-ARFIMA models, in data with mean zero. If mean is not zero, then it is
#' subtracted to data.
#'
#' @details
#' The estimation of the time-varying parameters can be carried out by means of
#' the Whittle log-likelihood function proposed by Dahlhaus (1997),
#' \deqn{L_n(\theta) = \frac{1}{4\pi}\frac{1}{M} \int_{-\pi}^{\pi}
#' \bigg\{log f_{\theta}(u_j,\lambda) +
#' \frac{I_N(u_j, \lambda)}{f_{\theta}(u_j,\lambda)}\bigg\}\,d\lambda}
#' where \eqn{M} is the number of blocks, \eqn{N} the length of the series per
#' block, \eqn{n =S(M-1)+N}, \eqn{S} is the shift from block to block,
#' \eqn{u_j =t_j/n}, \eqn{t_j =S(j-1)+N/2}, \eqn{j =1,\ldots,M} and
#' \eqn{\lambda} the Fourier frequencies in the block
#' (\eqn{2\,\pi\,k/N}, \eqn{k = 1,\ldots, N}).
#'
#' @param x COMPLETE
#'
#' @param series (type: numeric) univariate time series.
#'
#' @param order (type: numeric) vector corresponding to \code{ARMA} model
#' entered.
#'
#' @param ar.order (type: numeric) AR polimonial order.
#'
#' @param ma.order (type: numeric) MA polimonial order.
#'
#' @param sd.order (type: numeric) polinomial order noise scale factor.
#'
#' @param d.order (type: numeric) \code{d} polinomial order, where \code{d} is
#' the \code{ARFIMA} parameter.
#'
#' @param include.d (type: numeric) logical argument for \code{ARFIMA} models.
#' If \code{include.d=FALSE} then the model is an ARMA process.
#'
#' @param N (type: numeric) value corresponding to the length of the window to
#' compute periodogram. If \code{N=NULL} then the function will use
#' \eqn{N = \textmd{trunc}(n^{0.8})}, see Dahlhaus (1998) where \eqn{n} is the
#' length of the \code{y} vector.
#'
#' @param S (type: numeric) value corresponding to the lag with which will go
#' taking the blocks or windows.
#'
#' @param include.taper (type: logical) logical argument that by default is
#' \code{TRUE}. See \code{\link{periodogram}}.
#'
#' @references
#' For more information on theoretical foundations and estimation methods see
#'
#' \insertRef{brockwell2002introduction}{lsts}
#'
#' \insertRef{palma2010efficient}{lsts}
#'
#' @examples
#' 
#' @return
#' # COMPLETE
#'
#' @seealso \code{\link[stats]{nlminb}}, \code{\link{LS.kalman}}
#'
#' @importFrom stats na.omit
#'
#' @export
LS.whittle.loglik <- function(x, series, order = c(p = 0, q = 0), ar.order = NULL, ma.order = NULL, sd.order = NULL, d.order = NULL, include.d = FALSE, N = NULL, S = NULL, include.taper = TRUE) {
  y <- series
  T. <- length(y)

  if (is.null(N)) {
    N <- trunc(T.^0.8)
  }
  if (is.null(S)) {
    S <- trunc(0.2 * N)
  }

  M <- trunc((T. - N) / S + 1)

  if (is.null(ar.order)) {
    ar.order <- rep(0, order[1])
  }
  if (is.null(ma.order)) {
    ma.order <- rep(0, order[2])
  }
  if (is.null(sd.order)) {
    sd.order <- 0
  }
  if (is.null(d.order)) {
    d.order <- 0
  }


  p <- na.omit(c(ar.order, ma.order, sd.order))
  if (include.d == TRUE) {
    p <- na.omit(c(ar.order, ma.order, d.order, sd.order))
  }

  if (length(x) != sum(p + 1)) {
    stop("error in the number of parameters")
  }

  else {
    lik <- 0
    for (j in 1:M) {
      u <- (N / 2 + S * (j - 1)) / T.
      aux <- periodogram(y[(1 + S * (j - 1)):(N + S * (j - 1))], include.taper = TRUE, plot = FALSE)
      I <- aux$periodogram

      X <- numeric()
      k <- 1
      for (i in 1:length(p)) {
        X[i] <- sum(x[k:(k + p[i])] * u^(0:p[i]))
        k <- k + p[i] + 1
      }

      phi <- numeric()
      k <- 1
      if (order[1] > 0) {
        phi[is.na(ar.order) == 1] <- 0
        phi[is.na(ar.order) == 0] <- X[k:(length(na.omit(ar.order)))]
        k <- length(na.omit(ar.order)) + 1
      }

      theta <- numeric()
      if (order[2] > 0) {
        theta[is.na(ma.order) == 1] <- 0
        theta[is.na(ma.order) == 0] <- X[k:(length(na.omit(ma.order)) + k - 1)]
        k <- length(na.omit(ma.order)) + k
      }

      d <- 0
      if (include.d == TRUE) {
        d <- X[k]
        k <- k + 1
      }

      sigma <- X[k]

      f <- fdensity(ar = phi, ma = theta, d = d, sd = sigma, lambda = aux$lambda)

      lik <- sum(log(f) + I / f) / N + lik
    }

    lik <- lik / M

    if (is.na(sigma) | sigma <= 0) {
      lik <- Inf
    }

    lik
  }
}
