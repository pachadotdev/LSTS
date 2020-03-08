#' @title Kalman filter for locally stationary processes
#'
#' @description This function run the state-space equations for expansion
#' infinite of moving average in processes LS-ARMA or LS-ARFIMA.
#'
#' @details
#' The model fit is done using the Whittle likelihood, while the generation of
#' innovations is through Kalman Filter.
#' Details about \code{ar.order, ma.order, sd.order} and \code{d.order} can be
#' viewed in \code{\link{LS.whittle}}.
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
#' @param m (type: numeric) truncation order of the MA infinity process. By
#' default \eqn{m = 0.25*n^{0.8}} where \code{n} the length of \code{series}.
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
#' LS.kalman(malleco, start(malleco))
#' @return
#' A list with:
#' \item{residuals }{standard residuals.}
#' \item{fitted_values }{model fitted values.}
#' \item{delta }{variance prediction error.}
#'
#' @importFrom stats na.omit ARMAtoMA
#'
#' @export
LS.kalman <- function(series, start, order = c(p = 0, q = 0), ar.order = NULL, ma.order = NULL, sd.order = NULL, d.order = NULL, include.d = FALSE, m = NULL) {
  x <- start
  len_s <- length(series)

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
  if (is.null(m)) {
    m <- trunc(0.25 * len_s^0.8)
  }

  M <- m + 1
  u <- seq_len(len_s) / len_s

  p <- na.omit(c(ar.order, ma.order, sd.order))
  if (include.d == TRUE) {
    p <- na.omit(c(ar.order, ma.order, d.order, sd.order))
  }

  phi. <- numeric()
  theta. <- numeric()
  sigma. <- numeric()
  d. <- numeric()

  for (j in 1:length(u)) {
    k <- sapply(
      seq_along(p),
      function(i) {
        k0 <- 1
        k1 <- k0 + p[i] + 1
        
        if (i == 1) {
          k <- numeric()
          k[i] <- k1
        } else {
          k[i] <- k[i - 1] + p[i] + 1
        }
        
        return(k[i])
      }
    )
    
    X <- sapply(
      seq_along(p),
      function(i) {
        if (i == 1) {
          sum(x[1:(1 + p[i])] * u[j]^(0:p[i]))
        } else {
          sum(x[k[i - 1]:(k[i - 1] + p[i])] * u[j]^(0:p[i]))
        }
      }
    )

    phi <- numeric()
    l <- 1
    if (order[1] > 0) {
      phi[is.na(ar.order) == 1] <- 0
      phi[is.na(ar.order) == 0] <- X[l:(length(na.omit(ar.order)))]
      l <- length(na.omit(ar.order)) + 1
      phi. <- rbind(phi., phi)
    }

    theta <- numeric()
    if (order[2] > 0) {
      theta[is.na(ma.order) == 1] <- 0
      theta[is.na(ma.order) == 0] <- X[l:(length(na.omit(ma.order)) + l - 1)]
      l <- length(na.omit(ma.order)) + l
      theta. <- rbind(theta., theta)
    }

    d <- 0
    if (include.d == TRUE) {
      d <- X[l]
      l <- l + 1
      d. <- c(d., d)
    }

    sigma <- X[l]
    sigma. <- c(sigma., sigma)
  }

  sigma <- sigma.

  Omega <- matrix(0, nrow = M, ncol = M)
  diag(Omega) <- 1

  X <- rep(0, M)
  delta <- vector("numeric")
  hat.y <- vector("numeric")
  
  for (i in seq_len(len_s)) {
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
    
    if (include.d == TRUE) {
      eta <- gamma(0:m + d.[i]) / (gamma(0:m + 1) * gamma(d.[i]))
      
      psi <- sapply(
        seq_len(m+1),
        function(i) {
          k0 <- psi[1] * eta[1]
          
          if (i == 1) {
            k <- numeric()
            k[i] <- k0
          } else {
            k[i] <- sum(psi[1:i] * rev(eta[1:i]))
          }
          
          return(k[i])
        })
    }
    
    g <- sigma[i] * rev(psi)
    aux <- Omega %*% g
    delta[i] <- g %*% aux
    N <- matrix(0, M - 1, M - 1)
    diag(N) <- 1
    N <- cbind(0, N)
    N <- rbind(N, 0)
    Theta <- c(N %*% aux)
    Q <- matrix(0, M, M)
    Q[M, M] <- 1
    
    if (is.na(series[i])) {
      Omega <- N %*% Omega %*% t(N) + Q
      hat.y[i] <- t(g) %*% X
      X <- N %*% X
    }
    else {
      Omega <- N %*% Omega %*% t(N) + Q - Theta %*% solve(delta[i]) %*% Theta
      hat.y[i] <- t(g) %*% X
      X <- N %*% X + Theta %*% solve(delta[i]) %*% (series[i] - hat.y[i])
    }
  }
  
  residuals <- (series - hat.y) / sqrt(delta[seq_len(len_s)])
  fitted.values <- hat.y
  return(list(residuals = residuals, fitted.values = fitted.values, delta = delta))
}
