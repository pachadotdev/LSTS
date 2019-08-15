#' @title Hessian Matrix
#'
#' @description Plots in 3D the smoothing periodogram of a time series,
#' by blocks or windows.
#'
#' @details
#' Computes numerical approximation to Hessian of \code{f}, evaluated at
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
#' # Examples for CRAN checks:
#' # Executable in < 5 sec
#'
#' ## Ex 1: Variance of the maximum likelihood estimator for mu parameter in
#' ## gaussian data
#'
#' loglik <- function(series, x, sd = 1) {
#'   -sum(log(dnorm(series, mean = x, sd = sd)))
#' }
#'
#' n <- 500
#'
#' series <- rnorm(500, mean = 10, sd = 2)
#'
#' sqrt(c(var(series) / n, diag(solve(hessian(
#'   f = loglik, x = mean(series), series = series,
#'   sd = sd(series)
#' )))))
#'
#' ## Ex 2: Variance of the maximum likelihood estimator for phi parameter AR(1)
#' ## in gaussian data
#'
#' loglik <- function(series, x, sd = 1) {
#'   n <- length(series)
#'   -(sum(log(dnorm(series[2:n], mean = x * series[1:(n - 1)], sd = sd))) +
#'     log(dnorm(series[1], mean = 0, sd = sqrt(sd^2 / (1 - x^2)))))
#' }

#' n <- 1000
#'
#' series <- arima.sim(n, model = list(c(1, 0, 0), ar = 0.7))
#'
#' fit <- arima(series, c(1, 0, 0), include.mean = FALSE)
#'
#' round(
#'  c(fit$var.coef, diag(solve(hessian(
#'    f = loglik, x = fit$coef, series = series,
#'    sd = sqrt(fit$sigma2)
#'  )))), 6
#' )
#'
#' ## Ex 3:  Variance of the whittle maximum likelihood estimator for phi
#' ## parameter AR(1) in gaussian data
#'
#' loglik <- function(series, x, sd = 1) {
#'  n <- length(series)
#'  aux <- periodogram(series, plot = FALSE)
#'  lambda <- aux$lambda
#'  I <- aux$periodogram
#'  f <- spectral_density(ar = x, sd = sd, lambda = lambda)
#'  lik <- sum(log(f) + I / f)
#'  lik <- lik / n
#' }
#'
#' n <- 500
#' series <- arima.sim(n, model = list(c(1, 0, 0), ar = 0.7))
#' fit <- arima(series, c(1, 0, 0), include.mean = FALSE)
#' round(c(fit$var.coef, diag(solve(hessian(
#'   f = loglik, x = fit$coef, series = series,
#'   sd = sqrt(fit$sigma2)
#' ))) / n), 4)
#'
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
