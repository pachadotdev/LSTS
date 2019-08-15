#' @title Spectral Density
#'
#' @description Returns theoretical spectral density evaluated in ARMA and
#' ARFIMA processes.
#'
#' @details
#' The spectral density of an ARFIMA(p,d,q) processes is
#' \deqn{f(\lambda) = \frac{\sigma^2}{2\pi} \cdot \bigg(2\,
#' \textmd{sin}(\lambda/2)\bigg)^{-2d} \cdot
#' \frac{\bigg|\theta\bigg(\textmd{exp}\bigg(-i\lambda\bigg)\bigg)\bigg|^2}
#' {\bigg|\phi\bigg(\textmd{exp}\bigg(-i\lambda\bigg)\bigg)\bigg|^2}}
#'
#' With \eqn{-\pi \le \lambda \le \pi} and \eqn{-1 < d < 1/2}. \eqn{|x|} is the
#' \code{\link[base]{Mod}} of \emph{x}. \code{spectral_density} returns the
#' values corresponding to \eqn{f(\lambda)}. When \code{d} is zero, the spectral
#' density corresponds to an ARMA(p,q).
#'
#' @param ar (type: numeric) AR vector. If the time serie doesn't have AR term
#' then omit it. For more details see the examples.
#'
#' @param ma (type: numeric) MA vector. If the time serie doesn't have MA term
#' then omit it. For more details see the examples.
#'
#' @param d (type: numeric) Long-memory parameter. If d is zero, then the
#' process is ARMA(p,q).
#'
#' @param sd (type: numeric) Noise scale factor, by default is 1.
#'
#' @param lambda (type: numeric) \eqn{\lambda} parameter on which the spectral
#' density is calculated/computed. If \code{lambda=NULL} then it is considered a
#' sequence between 0 and \eqn{\pi}.
#'
#' @references
#' For more information on theoretical foundations and estimation methods see
#'
#' \insertRef{brockwell2002introduction}{lsts}
#'
#' \insertRef{palma2007long}{lsts}
#'
#' @examples
#' # Examples for CRAN checks:
#' # Executable in < 5 sec
#'
#' ## Example 1: Spectral Density AR(1)
#' lambda <- seq(0, pi, 0.01)
#' f <- spectral_density(ar = 0.5, lambda = lambda)
#' plot(f ~ lambda,
#'   bty = "n", type = "l", las = 1, xlab = expression("Frequency"),
#'   ylab = expression("Spectral Density")
#' )
#'
#' ## Example 2: Spectral Density AR(2)
#' lambda <- seq(0, pi, 0.01)
#' f <- spectral_density(ar = c(1.3, -0.6), lambda = lambda, sd = 10)
#' plot(f ~ lambda,
#'   bty = "n", type = "l", las = 1, xlab = expression("Frequency"),
#'   ylab = expression("Spectral Density")
#' )
#'
#' ## Spectral Density ARMA(1,1)
#' lambda <- seq(0, pi, 0.01)
#' f <- spectral_density(ar = 0.5, ma = 0.8, lambda = lambda)
#' plot(f ~ lambda,
#'   bty = "n", type = "l", las = 1, xlab = expression("Frequency"),
#'   ylab = expression("Spectral Density")
#' )
#'
#' ## Spectral Density ARFIMA(1,d,1)
#' lambda <- seq(0, pi, 0.01)
#' f <- spectral_density(ar = 0.5, ma = 0.8, d = 0.2, lambda = lambda)
#' plot(f ~ lambda,
#'   bty = "n", type = "l", las = 1, xlab = expression("Frequency"),
#'   ylab = expression("Spectral Density")
#' )
#' @return
#' ** COMPLETE **
#'
#' @seealso \code{\link{periodogram}}, \code{\link[graphics]{persp}}
#'
#' @export

spectral_density <- function(ar = numeric(), ma = numeric(), d = 0, sd = 1,
  lambda = NULL) {
  p <- length(ar)

  q <- length(ma)

  phi <- c(1, -ar)

  theta <- c(1, +ma)

  if (is.null(lambda)) {
    lambda <- seq(0, pi, 0.01)
  }

  n <- length(lambda)

  aux <- c()

  for (k in 1:n) {
    aux[k] <- (((2 * sin(lambda[k] / 2))^(-2))^d) *
      (
        sum((theta * exp(-1i * lambda[k] * c(0:q)))) *
        sum((theta * exp(+1i * lambda[k] * c(0:q))))
      ) /
      (
        sum((phi * exp(-1i * lambda[k] * c(0:p)))) *
        sum((phi * exp(+1i * lambda[k] * c(0:p))))
      )
  }

  sigma <- sd

  aux <- sigma^2 * Re(aux) / (2 * pi)

  return(aux)
}
