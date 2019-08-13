\name{periodogram}
\alias{periodogram}
\alias{timeseries}
\alias{Fourier}

\title{
Periodogram function
}
\description{
This function computes the periodogram from a stationary time serie. Returns the periodogram, its graph and the Fourier frequency.
}

\usage{
periodogram(y, plot = TRUE, include.taper = FALSE)
}

\arguments{
  \item{y}{
data vector.
}
  \item{plot}{
logical argument which allows to plot the periodogram.
}
  \item{include.taper}{
logical argument which by default is \code{FALSE}. If \code{include.taper=TRUE} then \code{y} is multiplied by \eqn{0.5*(1 - cos(2\pi(n-1)/n)) (\emph{cosine bell})}.
}
}

\details{
The tapered periodogram it is given by 
\deqn{I(\lambda) = \frac{|D_n(\lambda)|^2}{2\pi H_{2,n}(0)}} with \eqn{D(\lambda) = \sum_{s=0}^{n-1} h\left(\frac{s}{N}\right) y_{s+1}\, e^{-i\,\lambda\,s}}, \eqn{H_{k,n} = \sum_{s=0}^{n-1}h\left(\frac{s}{N}\right)^k\, e^{-i\,\lambda\,s}} and \eqn{\lambda} are Fourier frequencies defined as \eqn{2\pi k/n}, with \eqn{k = 1,\,\ldots,\, n}.\\

The data taper used is the cosine bell function, \eqn{h(x) = \frac{1}{2}[1-cos(2\pi x)]}. If the series has missing data, these are replaced by the average of the data and \eqn{n} it is corrected by $n-N$, where \eqn{N} is the amount of missing values of serie.

The plot of the periodogram is \code{periodogram} values vs. \eqn{\lambda}.
}

\value{
\code{periodogram} is a vector with values of the periodogram of the serie, while \code{lambda} is a vector with values corresponding to Fourier frequency. The graph is \code{periodogram ~ lambda}.
}

\references{
Brockwell, Peter J., and Richard A. Davis. \emph{Introduction to time series and forecasting}. 2002. ISBN-13: 978-0387953519.

Dahlhaus, R. \emph{Fitting time series models to nonstationary processes}. The Annals of Statistics. 1997; Vol. 25, No. 1:1-37.
}

\author{
Ricardo Olea <raolea@uc.cl>
}

\seealso{
\code{\link[stats]{fft}}, \code{\link[base]{Mod}} to 'periodogram' definition and \code{\link{smooth.periodogram}} in this package.
}

\examples{
## Require "rdatamarket"
library(rdatamarket)

## Database
malleco = dmlist("22tn")
mammothcreek = dmlist("22r7")

periodogram(malleco$Value)
periodogram(mammothcreek$Value)
}

\keyword{ periodogram }
\keyword{ Fourier }
\keyword{ timeseries }