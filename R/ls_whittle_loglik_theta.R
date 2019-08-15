#' @title Locally Stationary Whittle Log-likelihood theta
#'
#' @description Calculate the log-likelihood with \eqn{\sigma} known, through
#' \code{ls_whittle_loglik} function.
#'
#' @details
#' This function computes \code{\link{ls_whittle_loglik}} with \code{x} as
#' \code{x = c(x, sd.par)}.
#'
#' @param x (type: numeric) parameter vector.
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
#' @param sd.par (type: numeric) value corresponding to known variance.
#'
#' @examples
#' # Examples for CRAN checks:
#' # Executable in < 5 sec
#' @return
#' ** COMPLETE **
#'
#' @export

ls_whittle_loglik_theta <- function(x, series, order = c(p = 0, q = 0),
  ar.order = NULL, ma.order = NULL, sd.order = NULL, d.order = NULL,
  include.d = FALSE, N = NULL, S = NULL, include.taper = TRUE, sd.par = 1) {
  x <- c(x, sd.par)
  ls_whittle_loglik(x = x, series = series, order = order, ar.order = ar.order,
    ma.order = ma.order, sd.order = sd.order, d.order = d.order,
    include.d = include.d, N = N, S = S, include.taper = include.taper)
}
