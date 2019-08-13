\name{Box.Ljung.Test}
\alias{Box.Ljung.Test}

\title{
Ljung-Box Test Plot
}

\description{
Returns plot of the p-values Ljung-Box test.
}

\usage{
Box.Ljung.Test(z, lag = NULL, main = NULL)
}

\arguments{
  \item{z}{
numeric vector or univariate time series.
}
  \item{lag}{
lag or \code{lag.max} used in the \code{\link[stats]{acf}} of the time serie. If \code{lag=NULL} then the function will use \code{lag=10}.
}
  \item{main}{
an overall title for the plot. Optional argument, by default will print \emph{p values for Ljung-Box statistic}.
}
}

\details{
The Ljung-Box test is used to check if exists autocorrelation in a time series. The statistic is 
\deqn{Q = n(n+2)\cdot\sum_{j=1}^h \hat{\rho}(j)^2/(n-j)} with \emph{n} the number of observations and \eqn{\hat{\rho}(j)} the autocorrelation coefficient in the sample when the lag is \emph{j}.
\code{Box.Ljung.Test} computes \eqn{Q} and returns the p-values graph with lag \emph{j}.
}


\references{
Brockwell, Peter J., and Richard A. Davis. \emph{Introduction to time series and forecasting}. 2002. ISBN-13: 978-0387953519.

Ljung, G.M. and Box, G.E.P. (1978), \emph{On a measure of lack of fit in time series models}, Biometrika, 65, 297-303.
}

\author{
Ricardo Olea <raolea@uc.cl>
}


\examples{
z = rnorm(500)
Box.Ljung.Test(z, lag=15)
ts.diag(z)
}

\keyword{ Ljung }
\keyword{ Box }
\keyword{ timeseries }