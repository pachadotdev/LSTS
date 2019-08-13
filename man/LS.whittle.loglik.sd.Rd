\name{LS.whittle.loglik.sd}
\alias{LS.whittle.loglik.sd}

\title{
Locally Stationary Whittle Log-likelihood sigma
}

\description{
This function calculates log-likelihood with known \eqn{\theta}, through \code{LS.whittle.loglik} function.
}

\usage{
LS.whittle.loglik.sd(x, series, order = c(p = 0, q = 0), ar.order = NULL,
                     ma.order = NULL, sd.order = NULL, d.order = NULL,
                     include.d = FALSE, N = NULL, S = NULL, include.taper = TRUE,
                     theta.par = numeric())
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
AR and MA polimonial order, respectively.
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
  \item{theta.par}{
vector with the known parameters of the model.
}
}

\details{
This function computes \code{\link{LS.whittle.loglik}} with \code{x} as \code{x = c(theta.par, x)}.
}


\author{
Ricardo Olea <raolea@uc.cl>
}


\keyword{ whittle }
\keyword{ loglik }
\keyword{ estimator }
\keyword{ timeseries }