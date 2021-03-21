#' @title Kalman filter for locally stationary processes
#'
#' @description This function run the state-space equations for expansion
#' infinite of moving average in processes LS-ARMA or LS-ARFIMA.
#'
#' @details
#' The model fit is done using the Whittle likelihood, while the generation of
#' innovations is through Kalman Filter.
#' Details about \code{ar_order, ma_order, sd_order} and \code{d_order} can be
#' viewed in \code{\link{ls_whittle}}.
#'
#' @param series (type: numeric) univariate time series.
#'
#' @param start (type: numeric) numeric vector, initial values for parameters to
#' run the model.
#'
#' @param order (type: numeric) vector corresponding to \code{ARMA} model
#' entered.
#'
#' @param ar_order (type: numeric) AR polimonial order.
#'
#' @param ma_order (type: numeric) MA polimonial order.
#'
#' @param sd_order (type: numeric) polinomial order noise scale factor.
#'
#' @param d_order (type: numeric) \code{d} polinomial order, where \code{d} is
#' the \code{ARFIMA} parameter.
#'
#' @param include_d (type: numeric) logical argument for \code{ARFIMA} models.
#' If \code{include_d=FALSE} then the model is an ARMA process.
#'
#' @param m (type: numeric) truncation order of the MA infinity process. By
#' default \eqn{m = 0.25n^{0.8}} where \code{n} the length of \code{series}.
#'
#' @references
#' For more information on theoretical foundations and estimation methods see
#'
#' \insertRef{brockwell2002introduction}{lsts}
#'
#' \insertRef{palma2007long}{lsts}
#'
#' \insertRef{palma2013estimation}{lsts}
#'
#' @examples
#' fit_kalman <- ls_kalman(malleco, start(malleco))
#' 
#' @return
#' A list with:
#' \item{residuals }{standard residuals.}
#' \item{fitted_values }{model fitted values.}
#' \item{delta }{variance prediction error.}
#'
#' @importFrom stats na.omit ARMAtoMA
#'
#' @export
ls_kalman <- function(series, start, order = c(p = 0, q = 0), ar_order = NULL, ma_order = NULL, sd_order = NULL, d_order = NULL, include_d = FALSE, m = NULL) {
  x <- start
  T. <- length(series)

  if (is.null(ar_order)) {
    ar_order <- rep(0, order[1])
  }
  if (is.null(ma_order)) {
    ma_order <- rep(0, order[2])
  }
  if (is.null(sd_order)) {
    sd_order <- 0
  }
  if (is.null(d_order)) {
    d_order <- 0
  }
  if (is.null(m)) {
    m <- trunc(0.25 * T.^0.8)
  }

  M <- m + 1
  u <- (1:T.) / T.

  p <- na.omit(c(ar_order, ma_order, sd_order))
  if (include_d == TRUE) {
    p <- na.omit(c(ar_order, ma_order, d_order, sd_order))
  }

  phi. <- numeric()
  theta. <- numeric()
  sigma. <- numeric()
  d. <- numeric()

  for (j in 1:length(u)) {
    X <- numeric()
    k <- 1
    for (i in 1:length(p)) {
      X[i] <- sum(x[k:(k + p[i])] * u[j]^(0:p[i]))
      k <- k + p[i] + 1
    }

    phi <- numeric()
    k <- 1
    if (order[1] > 0) {
      phi[is.na(ar_order) == 1] <- 0
      phi[is.na(ar_order) == 0] <- X[k:(length(na.omit(ar_order)))]
      k <- length(na.omit(ar_order)) + 1
      phi. <- rbind(phi., phi)
    }


    theta <- numeric()
    if (order[2] > 0) {
      theta[is.na(ma_order) == 1] <- 0
      theta[is.na(ma_order) == 0] <- X[k:(length(na.omit(ma_order)) + k - 1)]
      k <- length(na.omit(ma_order)) + k
      theta. <- rbind(theta., theta)
    }

    d <- 0
    if (include_d == TRUE) {
      d <- X[k]
      k <- k + 1
      d. <- c(d., d)
    }

    sigma <- X[k]
    sigma. <- c(sigma., sigma)
  }

  sigma <- sigma.

  Omega <- matrix(0, nrow = M, ncol = M)
  diag(Omega) <- 1

  X <- rep(0, M)
  delta <- vector("numeric")
  hat.y <- vector("numeric")
  for (i in 1:T.) {
    if (is.null(dim(phi.)) == 1 & is.null(dim(theta.)) == 1) {
      psi <- c(1, ARMAtoMA(ar = numeric(), ma = numeric(), lag.max = m))
    }
    if (is.null(dim(phi.)) == 1 & is.null(dim(theta.)) == 0) {
      psi <- c(1, ARMAtoMA(ar = numeric(), ma = theta.[i, ], lag.max = m))
    }
    if (is.null(dim(phi.)) == 0 & is.null(dim(theta.)) == 1) {
      psi <- c(1, ARMAtoMA(ar = phi.[i, ], ma = numeric(), lag.max = m))
    }
    if (is.null(dim(phi.)) == 0 & is.null(dim(theta.)) == 0) {
      psi <- c(1, ARMAtoMA(ar = phi.[i, ], ma = theta.[i, ], lag.max = m))
    }
    psi. <- numeric()
    if (include_d == TRUE) {
      eta <- gamma(0:m + d.[i]) / (gamma(0:m + 1) * gamma(d.[i]))
      for (k in 0:m) {
        psi.[k + 1] <- sum(psi[1:(k + 1)] * rev(eta[1:(k + 1)]))
      }
      psi <- psi.
    }
    g <- sigma[i] * rev(psi)
    aux <- Omega %*% g
    delta[i] <- g %*% aux
    F. <- matrix(0, M - 1, M - 1)
    diag(F.) <- 1
    F. <- cbind(0, F.)
    F. <- rbind(F., 0)
    Theta <- c(F. %*% aux)
    Q <- matrix(0, M, M)
    Q[M, M] <- 1
    if (is.na(series[i])) {
      Omega <- F. %*% Omega %*% t(F.) + Q
      hat.y[i] <- t(g) %*% X
      X <- F. %*% X
    }
    else {
      Omega <- F. %*% Omega %*% t(F.) + Q - Theta %*% solve(delta[i]) %*% Theta
      hat.y[i] <- t(g) %*% X
      X <- F. %*% X + Theta %*% solve(delta[i]) %*% (series[i] - hat.y[i])
    }
  }
  residuals <- (series - hat.y) / sqrt(delta[1:T.])
  fitted.values <- hat.y
  return(list(residuals = residuals, fitted.values = fitted.values, delta = delta))
}
