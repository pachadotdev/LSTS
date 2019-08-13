\name{LS.whittle.loglik}
\alias{LS.whittle.loglik}
\alias{loglik}

\title{
Locally Stationary Whittle log-likelihood Function
}

\description{
This function computes Whittle estimator for LS-ARMA and LS-ARFIMA models, in data with mean zero. If mean is not zero, then it is subtracted to data.
} 

\usage{
LS.whittle.loglik(x, series, order = c(p = 0, q = 0), ar.order = NULL,
                  ma.order = NULL, sd.order = NULL, d.order = NULL, 
                  include.d = FALSE, N = NULL, S = NULL, include.taper = TRUE)
}

\arguments{
  \item{x}{
parameter vector.
}
  \item{series}{
univariate time series.
}
  \item{order}{
vector with the specification of the ARMA model: the two integer components (p, q) are the AR order and the MA order.
}
  \item{ar.order, ma.order}{
AR and MA polimonial order, respectively. See details below.
}
  \item{sd.order}{
polinomial order noise scale factor.
}
  \item{d.order}{
\code{d} polinomial order, where \code{d} is the \code{ARFIMA} parameter.
}
  \item{include.d}{
logical argument for \code{ARFIMA} models. If \code{include.d=FALSE} then the model is an ARMA process.
}
  \item{N}{
value corresponding to the length of the window to compute periodogram. If \code{N=NULL} then the function will use \eqn{N = \textmd{trunc}(n^{0.8})}, see Dahlhaus (1998) where \eqn{n} is the length of the \code{y} vector.
}
  \item{S}{
value corresponding to the lag with which will go taking the blocks or windows.
}
  \item{include.taper}{
logical argument that by default is \code{TRUE}. See \code{\link{periodogram}}.
}
}

\details{
The estimation of the time-varying parameters can be carried out by means of the Whittle log-likelihood function proposed by Dahlhaus (1997),
\deqn{L_n(\theta) = \frac{1}{4\pi}\frac{1}{M} \int_{-\pi}^{\pi} \bigg\{log f_{\theta}(u_j,\lambda) + \frac{I_N(u_j, \lambda)}{f_{\theta}(u_j,\lambda)}\bigg\}\,d\lambda}
where \eqn{M} is the number of blocks, \eqn{N} the length of the series per block, \eqn{n =S(M-1)+N}, \eqn{S} is the shift from block to block, \eqn{u_j =t_j/n}, \eqn{t_j =S(j-1)+N/2}, \eqn{j =1,\ldots,M} and \eqn{\lambda} the Fourier frequencies in the block (\eqn{2\,\pi\,k/N}, \eqn{k = 1,\ldots, N}).
}

\references{
Dahlhaus, R. \emph{Fitting time series models to nonstationary processes}. The Annals of Statistics. 1997; Vol. 25, No. 1:1-37.

Olea R, Palma W. \emph{An efficient estimator for locally stationary gaussian long-memory processes}. The Annals of Statistics. 2010; Vol. 38, No. 5:2958-2997.
}

\author{
Ricardo Olea <raolea@uc.cl>
}


\keyword{ whittle }
\keyword{ loglik }
\keyword{ estimator }
\keyword{ timeseries }