#' @title Spectral Density
#'
#' @description Returns theoretical spectral density evaluated in ARMA and
#' ARFIMA processes.
#'
#' @details
#' The spectral density of an ARFIMA(p,d,q) processes is
#' \deqn{f(\lambda) = \frac{\sigma^2}{2\pi} \cdot \bigg(2\,
#' \sin(\lambda/2)\bigg)^{-2d} \cdot
#' \frac{\bigg|\theta\bigg(\exp\bigg(-i\lambda\bigg)\bigg)\bigg|^2}
#' {\bigg|\phi\bigg(\exp\bigg(-i\lambda\bigg)\bigg)\bigg|^2}}
#'
#' With \eqn{-\pi \le \lambda \le \pi} and \eqn{-1 < d < 1/2}. \eqn{|x|} is the
#' \code{\link[base]{Mod}} of \emph{x}. \code{lsts_sd} returns the
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
#' # Spectral Density AR(1)
#'
#' lambda <- seq(0, pi, 0.01)
#'
#' f <- lsts_sd(ar = 0.5, lambda = lambda)
#'
#' plot(f ~ lambda,
#'   bty = "n", type = "l", las = 1, xlab = expression("Frequency"),
#'   ylab = expression("Spectral Density")
#' )
#' 
#' @return
#' ** COMPLETE **
#'
#' @seealso \code{\link{lsts_periodogram}}, \code{\link[graphics]{persp}}
#'
#' @export

lsts_sd <- function(ar = numeric(), ma = numeric(), d = 0, sd = 1, lambda = NULL) {
  p <- length(ar)
  q <- length(ma)
  
  phi <- c(1, -ar)
  theta <- c(1, +ma)
  
  if (is.null(lambda)) {
    lambda <- seq(0, pi, 0.01)
  }
  
  aux <- sapply(
    seq_along(lambda),
    function(k) {
      (((2 * sin(lambda[k]/2))^(-2))^d) * (sum((theta * exp(-1i*lambda[k]*c(0:q)))) * sum((theta * exp(+1i*lambda[k]*c(0:q))))) / (sum((phi * exp(-1i*lambda[k]*c(0:p))))*sum((phi * exp(+1i*lambda[k]*c(0:p)))))
    }
  )
  
  sigma <- sd
  aux <- sigma^2 * Re(aux) / (2 * pi)
  return(aux)
}
