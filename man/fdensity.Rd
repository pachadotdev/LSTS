\name{fdensity}
\alias{fdensity}
\alias{spectral}

\title{
Spectral density
}

\description{
Returns theoretical spectral density evaluated in ARMA and ARFIMA processes.
}

\usage{
fdensity(ar = numeric(), ma = numeric(), d = 0, sd = 1, lambda = NULL)
}

\arguments{
  \item{ar, ma}{
AR or MA vector, respectively. If the time serie doesn't have AR (or MA) term then omit it. To more details see examples.
}
  \item{d}{
\code{d} is a long-memory parameter. If d is zero, then the process is ARMA(p,q)
}
  \item{sd}{
Noise scale factor, by default is 1.
}
  \item{lambda}{
\eqn{\lambda} parameter on which the spectral density is calculated/computed. If \code{lambda=NULL} then it is considered a sequence between 0 and \eqn{\pi}.
}
}

\details{
The spectral density of an ARFIMA(p,d,q) processes is
\deqn{f(\lambda) = \frac{\sigma^2}{2\pi} \cdot \bigg(2\, \textmd{sin}(\lambda/2)\bigg)^{-2d} \cdot \frac{\bigg|\theta\bigg(\textmd{exp}\bigg(-i\lambda\bigg)\bigg)\bigg|^2}{\bigg|\phi\bigg(\textmd{exp}\bigg(-i\lambda\bigg)\bigg)\bigg|^2}}

with \eqn{-\pi \le \lambda \le \pi} and \eqn{-1 < d < 1/2}. \eqn{|x|} is the \code{\link[base]{Mod}} of \emph{x}. \code{fdensity} returns the values corresponding to \eqn{f(\lambda)}. When \code{d} is zero, the spectral density corresponds to an ARMA(p,q).
}

\references{
Brockwell, Peter J., and Richard A. Davis. \emph{Introduction to time series and forecasting}. 2002. ISBN-13: 978-0387953519.

Palma W. \emph{Long-Memory Time Series. Theory and Methods}. 1st ed. New Jersey: John Wiley & Sons, Inc.; 2007. 285 p.
}

\author{
Ricardo Olea <raolea@uc.cl>
}

\examples{
## Example 1: Spectral Density AR(1)
lambda = seq(0,pi,0.01)
f = fdensity(ar = 0.5, lambda = lambda)
plot(f~lambda, bty = "n", type = "l", las = 1, xlab = expression("Frequency"),
     ylab = expression("Spectral Density"))


## Example 2: Spectral Density AR(2)
lambda = seq(0,pi,0.01)
f = fdensity(ar = c(1.3,-0.6), lambda = lambda, sd=10)
plot(f~lambda, bty = "n", type = "l", las = 1, xlab = expression("Frequency"),
     ylab = expression("Spectral Density"))


## Spectral Density ARMA(1,1)
lambda = seq(0,pi,0.01)
f = fdensity(ar = 0.5, ma = 0.8, lambda = lambda)
plot(f~lambda, bty = "n", type = "l", las = 1, xlab = expression("Frequency"),
     ylab = expression("Spectral Density"))


## Spectral Density ARFIMA(1,d,1)
lambda = seq(0,pi,0.01)
f = fdensity(ar = 0.5, ma = 0.8, d = 0.2, lambda = lambda)
plot(f~lambda, bty = "n", type = "l", las = 1, xlab = expression("Frequency"),
     ylab = expression("Spectral Density"))
}

\keyword{ density }
\keyword{ spectral }
\keyword{ timeseries }