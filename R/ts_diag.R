#' @title Diagnostic Plots for Time Series fits
#'
#' @description Plot time-series diagnostics.
#'
#' @details
#' This function plot the residuals, the autocorrelation function of the
#' residuals (ACF) and the p-values of the Ljung-Box Test for all lags up to
#' \code{lag}.
#'
#' @param x (type: numeric) residuals of the fitted time series model.
#'
#' @param lag (type: numeric) maximum lag at which to calculate the acf and
#' Ljung-Box test. By default set to 10.
#' 
#' @examples
#' ts_diag(malleco)
#' 
#' @return
#' ** COMPLETE **
#'
#' @seealso \code{\link{box_ljung_test}}
#'
#' @importFrom stats sd acf na.pass time
#' @importFrom ggplot2 ggplot aes geom_col geom_segment geom_hline labs
#'  theme_minimal
#' @importFrom patchwork plot_layout
#'
#' @export
ts_diag <- function(x, lag = 10) {
  Z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  x_acf <- acf(x, plot = FALSE)
  x_acf <- with(x_acf, data.frame(lag, acf))

  g1 <- ggplot() +
    geom_col(data = data.frame(x = time(Z), y = Z), aes(x = x, y = y)) +
    geom_hline(yintercept = 0, linetype="solid", color = "blue") +
    geom_hline(yintercept = c(-2,2), linetype="dashed", color = "blue") +
    geom_hline(yintercept = c(-3,3), linetype="dotted", color = "blue") +
    labs(x = "time(Z)", y = "Z", title = "Standardized Residuals") +
    theme_minimal()
  
  g2 <- ggplot(data = x_acf, mapping = aes(x = lag, y = acf)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(yintercept = 0, linetype="solid", color = "blue") +
    geom_hline(yintercept = c(-0.05,0.05), linetype="dashed", color = "blue") +
    labs(x = "Lag", y = "ACF", title = "ACF of Residuals") +
    theme_minimal()
  
  g3 <- box_ljung_test(x, lag = lag)
  
  g <- g1 + g2 + g3 + plot_layout(nrow = 3, byrow = FALSE)
  return(g)
}

#' Diagnostic Plots for Time Series fits
#' @description \code{ts_diag()} replaces this function
#' @param ... old parameters
#' @export
ts.diag <- function(...) {
  .Deprecated("")
}
