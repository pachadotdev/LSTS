#' @title Whittle estimator to Locally Stationary Time Series
#'
#' @description This function computes Whittle estimator to LS-ARMA and
#' LS-ARFIMA models.
#'
#' @details
#' This function estimates the parameters in models: LS-ARMA
#' \deqn{\Phi(t/T, \, B)\, Y_{t, T} = \Theta(t/T,\, B)\,\sigma(t/T)\,
#' \varepsilon_t} and LS-ARFIMA \deqn{\Phi(t/T, \, B)\, Y_{t, T} =
#' \Theta(t/T,\, B)\, (1-B)^{-d(t/T)}\, \sigma(t/T)\, \varepsilon_t,}
#' with infinite moving average expansion
#' \deqn{Y_{t, T} = \sigma(t/T)\, \sum_{j=0}^{\infty}
#' \psi(t/T)\,\varepsilon_t,} for \eqn{t = 1,\ldots, T}, where for
#' \eqn{u = t/T \in [0,1]}, \eqn{\Phi(u,B)=1+\phi_1(u)B +\cdots+\phi_p(u)B^p}
#' is an autoregressive polynomial,
#' \eqn{\Theta(u, B) = 1 + \theta_1(u)B + \cdots  + \theta_q(u)B^q} is a
#' moving average polynomial, \eqn{d(u)} is a long-memory parameter,
#' \eqn{\sigma(u)} is a noise scale factor and \eqn{\{\varepsilon_t \}} is a
#' Gaussian white noise sequence with zero mean and unit variance. This class
#' of models extends the well-known ARMA and ARFIMA process, which is obtained
#' when the components \eqn{\Phi(u, B)}, \eqn{\Theta(u, B)}, \eqn{d(u)} and
#' \eqn{\sigma(u)} do not depend on \eqn{u}.
#' The evolution of these models can be specified in terms of a general class
#' of functions. For example, let \eqn{\{g_j(u)\}}, \eqn{j = 1, 2, \ldots}, be
#' a basis for a space of smoothly varying functions and let
#' \eqn{d_{\theta}(u)} be the time-varying long-memory parameter in model
#' LS-ARFIMA. Then we could write \eqn{d_{\theta}(u)} in terms of the basis
#' \eqn{\{g_j(u) = u^j\}} as follows
#' \eqn{d_{\theta}(u) = \sum_{j=0}^{k} \alpha_j\,g_j(u)}
#' for unknown values of \eqn{k} and
#' \eqn{\theta = (\alpha_0,\,\alpha_1,\,\ldots, \,\alpha_k)^{\prime}}.
#' In this situation, estimating \eqn{\theta} involves determining \eqn{k} and
#' estimating the coefficients \eqn{\alpha_0,\,\alpha_1,\,\ldots, \,\alpha_k}.
#' \code{ls_whittle} optimizes \code{\link{ls_whittle_loglik}} as objective
#' function using \code{\link[stats]{nlminb}} function, for both LS-ARMA
#' (\code{include.d=FALSE}) and LS-ARFIMA (\code{include.d=TRUE}) models.
#' Also computes Kalman filter with \code{\link{ls_kalman}} and this values
#' are given in \code{var.coef} in the output.
#'
#' @param series (type: numeric) univariate time series.
#'
#' @param start (type: numeric) numeric vector, initial values for parameters to
#' run the model.
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
#' @param control (type: list) A list of control parameters. More details in
#' \code{\link[stats]{nlminb}} .
#'
#' @param lower (type: numeric) lower bound, replicated to be as long as
#' \code{start}. If unspecified, all parameters are assumed to be lower
#' unconstrained.
#'
#' @param upper (type: numeric) upper bound, replicated to be as long as
#' \code{start}. If unspecified, all parameters are assumed to be upper
#' unconstrained.
#'
#' @param m (type: numeric) truncation order of the MA infinity process, by
#' default \eqn{m = 0.25*n^{0.8}}. Parameter used in \code{ls_kalman}.
#'
#' @param n.ahead (type: numeric) The number of steps ahead for which prediction
#' is required. By default is zero.
#'
#' @examples
#' # Examples for CRAN checks:
#' # Executable in < 5 sec
#'
#' @return
#' A list with the following components:
#' \item{coef }{The best set of parameters found.}
#' \item{var.coef }{covariance matrix approximated for maximum likelihood
#' estimator \eqn{\hat{\theta}} of
#' \eqn{\theta:=(\theta_1,\ldots,\theta_k)^{\prime}}. This matrix is
#' approximated by \eqn{H^{-1}/n}, where \eqn{H} is the Hessian matrix
#' \eqn{[\partial^2 \ell(\theta)/\partial\theta_i
#' \partial\theta_j]_{i,j=1}^{k}}.}
#' \item{loglik }{log-likelihood of \code{coef}, calculated with
#' \code{\link{ls_whittle_loglik}}.}
#' \item{aic }{Akaike's `An Information Criterion', for one fitted model LS-ARMA
#' or LS-ARFIMA. The formula is -2*log-likelihood + 2*npar/n, where \emph{npar}
#' represents the number of parameters in the fitted model and \emph{n} equal to
#' the length of the \code{series}.}
#' \item{series }{original time serie.}
#' \item{residuals }{standard residuals.}
#' \item{fitted.values }{model fitted values.}
#' \item{pred }{predictions of the model.}
#' \item{se}{the estimated standard errors.}
#' \item{model }{A list representing the fitted model.}
#'
#' @seealso \code{\link[stats]{nlminb}}, \code{\link{ls_kalman}}
#'
#' @importFrom stats nlminb
#'
#' @export

ls_whittle <- function(series, start, order = c(p = 0, q = 0), ar.order = NULL,
  ma.order = NULL, sd.order = NULL, d.order = NULL, include.d = FALSE, N = NULL,
  S = NULL, include.taper = TRUE, control = list(), lower = -Inf, upper = Inf,
  m = NULL, n.ahead = 0) {
  series <- c(series, rep(NA, n.ahead))

  aux <- stats::nlminb(start = start, objective = ls_whittle_loglik,
    series = series, order = order, ar.order = ar.order, ma.order = ma.order,
    sd.order = sd.order, d.order = d.order, include.d = include.d, N = N, S = S,
    include.taper = include.taper, lower = lower, upper = upper,
    control = control)

  T. <- length(series)
  
  loglik <- -aux$objective
  
  npar <- length(aux$par)
  
  aic <- -2 * loglik + 2 * npar / T.

  aux. <- ls_kalman(series = series - mean(series, na.rm = TRUE),
    start = aux$par, order = order, ar.order = ar.order, ma.order = ma.order,
    sd.order = sd.order, d.order = d.order, include.d = include.d, m = m)

  x <- aux$par
  k <- npar

  if (is.null(sd.order)) {
    sd.order <- 0
  }

  if (is.null(d.order)) {
    d.order <- 0
  }

  G1 <- hessian(f = ls_whittle_loglik_theta, x0 = x[1:(k - d.order - 1)],
    series = series, order = order, ar.order = ar.order, ma.order = ma.order,
    sd.order = sd.order, d.order = d.order, include.d = include.d, N = N, S = S,
    include.taper = include.taper, sd.par = x[(k - d.order):k])

  G2 <- hessian(f = ls_whittle_loglik_sd, x0 = x[(k - d.order):k],
    series = series, order = order, ar.order = ar.order, ma.order = ma.order,
    sd.order = sd.order, d.order = d.order, include.d = include.d, N = N, S = S,
    include.taper = include.taper, theta.par = x[1:(k - d.order - 1)])

  G <- matrix(0, ncol = k, nrow = k)
  G[1:(k - d.order - 1), 1:(k - d.order - 1)] <- G1
  G[(k - d.order):k, (k - d.order):k] <- G2
  G <- solve(G) / T.

  fitted.values <- aux.$fitted.values + mean(series, na.rm = TRUE)
  fitted.values[is.na(series) == 1] <- NA

  pred <- aux.$fitted.values + mean(series, na.rm = TRUE)
  pred[is.na(series) == 0] <- NA

  se <- sqrt(aux.$delta)
  se[is.na(series) == 0] <- NA

  list(coef = aux$par, var.coef = G, loglik = loglik, aic = aic,
    series = series, residuals = aux.$residuals, fitted.values = fitted.values,
    pred = pred, se = se, model = list(order = order, ar.order = ar.order,
    ma.order = ma.order, sd.order = sd.order, d.order = d.order,
    include.d = include.d, include.taper = include.taper))
}
