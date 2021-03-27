#' @title Locally Stationary Whittle Log-likelihood theta
#' @description Calculate the log-likelihood with \eqn{\sigma} known, through
#' \code{ls_whittle_loglik} function.
#' @details
#' This function computes \code{\link{ls_whittle_loglik}} with \code{x} as
#' \code{x = c(x, sd_par)}.
#' @param x (type: numeric) parameter vector.
#' @param series (type: numeric) univariate time series.
#' @param order (type: numeric) vector corresponding to \code{ARMA} model
#' entered.
#' @param ar_order (type: numeric) AR polimonial order.
#' @param ma_order (type: numeric) MA polimonial order.
#' @param sd_order (type: numeric) polinomial order noise scale factor.
#' @param d_order (type: numeric) \code{d} polinomial order, where \code{d} is
#' the \code{ARFIMA} parameter.
#' @param include_d (type: numeric) logical argument for \code{ARFIMA} models.
#' If \code{include_d=FALSE} then the model is an ARMA process.
#' @param w (type: numeric) value corresponding to the length of the window to
#' compute periodogram. If \code{w=NULL} then the function will use
#' \eqn{w = \textrm{trunc}(n^{0.8})}, see Dahlhaus (1998) where \eqn{n} is the
#' length of the \code{y} vector.
#' @param s (type: numeric) value corresponding to the lag with which will go
#' taking the blocks or windows.
#' @param include_taper (type: logical) logical argument that by default is
#' \code{TRUE}. See \code{\link{periodogram}}.
#' @param sd_par (type: numeric) value corresponding to known variance.
#' @export
ls_whittle_loglik_theta <- function(x, series, order = c(p = 0, q = 0), ar_order = NULL, ma_order = NULL, sd_order = NULL, d_order = NULL, include_d = FALSE, w = NULL, s = NULL, include_taper = TRUE, sd_par = 1) {
  x <- c(x, sd_par)
  ls_whittle_loglik(x = x, series = series, order = order, ar_order = ar_order, ma_order = ma_order, sd_order = sd_order, d_order = d_order, include_d = include_d, w = w, s = s, include_taper = include_taper)
}
