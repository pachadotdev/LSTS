#' @title Diagnostic Plots for Time Series fits
#' @description Plot time-series diagnostics.
#' @details
#' This function plot the residuals, the autocorrelation function of the
#' residuals (ACF) and the p-values of the Ljung-Box Test for all lags up to
#' \code{lag}.
#' @param x (type: numeric) residuals of the fitted time series model.
#' @param lag (type: numeric) maximum lag at which to calculate the acf and
#' Ljung-Box test. By default set to 10.
#' @param band (type: numeric) absolute value for bandwidth in the the ACF plot.
#' By default set to `qnorm(0.975)/sqrt(n)` which approximates to 0.07 for 
#' malleco data (n = 734)
#' @examples
#' ts.diag(malleco)
#' @return
#' A ggplot object.
#' @seealso \code{\link{Box.Ljung.Test}}
#' @importFrom stats sd acf na.pass time
#' @importFrom ggplot2 ggplot aes geom_col geom_segment geom_hline labs
#'  theme_minimal
#' @importFrom patchwork plot_layout
#' @export
ts.diag <- function(x, lag = 10, band = qnorm(0.975) / sqrt(length(x))) {
  Z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  x_acf <- acf(x, plot = FALSE, lag.max = lag, na.action = na.pass)
  x_acf <- with(x_acf, data.frame(lag, acf))

  g1 <- ggplot() +
    geom_col(data = data.frame(x = as.numeric(time(Z)), y = as.numeric(Z)), aes(x = x, y = y)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "blue") +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "blue") +
    geom_hline(yintercept = c(-3, 3), linetype = "dotted", color = "blue") +
    labs(x = "time(Z)", y = "Z", title = "Standardized Residuals") +
    theme_minimal()

  g2 <- ggplot(data = x_acf, mapping = aes(x = lag, y = acf)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "blue") +
    geom_hline(yintercept = c(-1 * band, band), linetype = "dashed", color = "blue") +
    scale_x_continuous(limits = c(0, lag), breaks = 0:lag) +
    labs(x = "Lag", y = "ACF", title = "ACF of Residuals") +
    theme_minimal()

  g3 <- Box.Ljung.Test(x, lag = lag)

  g <- g1 + g2 + g3 + plot_layout(nrow = 3, byrow = FALSE)

  return(g)
}
