#' @title Ljung-Box Test Plot
#'
#' @description Plots the p-values Ljung-Box test.
#'
#' @details
#' The Ljung-Box test is used to check if exists autocorrelation in a time
#' series. The statistic is
#' \deqn{Q = n(n+2)\cdot\sum_{j=1}^h \hat{\rho}(j)^2/(n-j)} with \emph{n} the
#' number of observations and \eqn{\hat{\rho}(j)} the autocorrelation
#' coefficient in the sample when the lag is \emph{j}. \code{lsts_lbtp}
#' computes \eqn{Q} and returns the p-values graph with lag \emph{j}.
#'
#' @param z (type: numeric) data vector
#' @param lag (type: numeric) the number of periods for the autocorrelation
#' @param main (type: character) a title for the returned plot
#'
#' @references
#' For more information on theoretical foundations and estimation methods see
#'
#' \insertRef{brockwell2002introduction}{lsts}
#'
#' \insertRef{ljung1978measure}{lsts}
#'
#' @examples
#' box_ljung_test(malleco, lag = 5)
#' 
#' @return
#' A ggplot object
#'
#' @seealso \code{\link{periodogram}}, \code{\link[graphics]{persp}}
#'
#' @importFrom stats acf na.pass pchisq
#' @importFrom ggplot2 aes ggplot geom_point geom_hline scale_x_continuous
#'  scale_y_continuous labs theme_minimal
#'
#' @export
box_ljung_test <- function(z, lag = NULL, main = NULL) {
  if (is.null(lag)) {
    lag <- 10
  }
  k <- lag
  n <- length(z)
  aux <- acf(z, plot = FALSE, lag.max = k, na.action = na.pass)
  p.value <- vector("numeric")
  Q <- vector("numeric")
  for (j in 1:k) {
    rho <- aux$acf[2:(j + 1), , 1]
    Q[j] <- sum(n * (n + 2) * rho^2 / (n - 1:j))
    p.value[j] <- 1 - pchisq(Q[j], df = j)
  }
  if (is.null(main)) {
    main <- expression("p values for Ljung-Box statistic")
  }

  g <- ggplot(data = data.frame(x = 1:k, y = p.value), aes(x = x, y = y)) +
    geom_point() +
    geom_hline(yintercept = 0.05, linetype="dashed", color = "blue") +
    scale_x_continuous(limits = c(0,k), breaks = 1:k) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = "Lag", y = "p-value", title = "p-values for Ljung-Box statistic") +
    theme_minimal()
  
  return(g)
}
