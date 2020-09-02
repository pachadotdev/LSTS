#' @title Hessian Matrix
#'
#' @description Numerical aproximation of the Hessian of a function.
#'
#' @details
#' Computes the numerical approximation of the Hessian of \code{f}, evaluated at
#' \code{x0}.
#' Usually needs to pass additional parameters (e.g. data).  N.B. this uses no
#' numerical sophistication.
#'
#' @param f (type: numeric) name of function that defines log likelihood
#' (or negative of it).
#'
#' @param x0 (type: numeric) scalar or vector of parameters that give the point
#' at which you want the hessian estimated (usually will be the mle).
#'
#' @param ... Additional arguments to be passed to the function.
#'
#' @examples
#' # Variance of the maximum likelihood estimator for mu parameter in
#' # gaussian data
#'
#' loglik <- function(series, x, sd = 1) {
#'   -sum(log(dnorm(series, mean = x, sd = sd)))
#' }
#'
#' sqrt(c(var(malleco) / length(malleco), diag(solve(hessian(
#'   f = loglik, x = mean(malleco), series = malleco,
#'   sd = sd(malleco)
#' )))))
#' @return
#' An \code{n x n} matrix of 2nd derivatives, where \emph{n} is the length of
#' \code{x0}.
#'
#' @seealso \code{\link{arima.sim}}
#'
#' @export
hessian <- function(f, x0, ...) {
  n <- length(x0)
  grad <- rep(0, n)
  mdelta <- matrix(0., nrow = n, ncol = n)
  hess <- mdelta
  delta <- 0.0001 * (sign(x0) + (x0 == 0.)) * pmax(abs(x0), 0.01)
  diag(mdelta) <- delta
  f0 <- f(x0, ...)
  for (i in 1.:n) {
    grad[i] <- f(x0 + mdelta[, i], ...)
  }
  for (i in 1.:n) {
    for (j in i:n) {
      hess[i, j] <- f(x0 + mdelta[, i] + mdelta[, j], ...) - grad[i] - grad[j]
      hess[j, i] <- hess[i, j]
    }
  }
  (hess + f0) / outer(delta, delta, "*")
}
