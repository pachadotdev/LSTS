\name{hessian}
\alias{hessian}

\title{
Hessian Matrix
}

\description{
Computes numerical approximation to Hessian of \code{f}, evaluated at \code{x0}. Usually need to pass additional parameters (e.g. data).  N.B. this uses no numerical sophistication.
}

\usage{
hessian(f, x0, ...)
}

\arguments{
  \item{f}{
name of function that defines log likelihood (or negative of it).
}
  \item{x0}{
scalar or vector of parameters that give the point at which you want the hessian estimated (usually will be the mle).
}

  \item{...}{
additional arguments passed to \code{f}.
}
}

\value{
An \code{n x n} matrix of 2nd derivatives, where \emph{n} is the length of \code{x0}. 
}

\author{
Wilfredo Palma <wilfredo@mat.puc.cl>
}

\examples{
## Ex 1:  Variance of the maximum likelihood estimator for mu parameter in gaussian data
loglik = function(series, x, sd = 1){
-sum(log(dnorm(series, mean = x, sd = sd)))
}
n = 500
series = rnorm(500, mean = 10, sd = 2)
sqrt(c(var(series)/n,diag(solve(hessian(f = loglik, x = mean(series), series = series, 
                          sd = sd(series))))))


## Ex 2:  Variance of the maximum likelihood estimator for phi parameter AR(1)
## in gaussian data
loglik = function(series, x, sd = 1){
n = length(series)
-(sum(log(dnorm(series[2:n], mean = x * series[1:(n-1)], sd = sd))) + 
log(dnorm(series[1], mean = 0, sd = sqrt(sd^2/(1-x^2)))))
}
n = 1000
series = arima.sim(n, model = list(c(1,0,0), ar = 0.7))
fit = arima(series, c(1,0,0), include.mean = FALSE)
round(c(fit$var.coef,diag(solve(hessian(f = loglik, x = fit$coef, series = series,
      sd = sqrt(fit$sigma2))))),6)


## Ex 3:  Variance of the whittle maximum likelihood estimator for phi parameter
## AR(1) in gaussian  data
loglik = function(series, x, sd = 1){
n = length(series)
aux = periodogram(series, plot = FALSE)
lambda = aux$lambda
I = aux$periodogram
f = fdensity(ar = x, sd = sd, lambda = lambda)
lik = sum(log(f)+I/f)
lik = lik/n
}
n = 500
series = arima.sim(n, model = list(c(1,0,0), ar = 0.7))
fit = arima(series, c(1,0,0), include.mean = FALSE)
round(c(fit$var.coef,diag(solve(hessian(f = loglik, x = fit$coef, series = series,
      sd = sqrt(fit$sigma2))))/n),4)
}

\keyword{ hessian }
\keyword{ timeseries }