\name{smooth.periodogram}
\alias{smooth.periodogram}

\title{
Smoothing periodogram
}

\description{
This function returns the smoothing periodogram of a stationary time serie, its plot and its Fourier frequency.
}
\usage{
smooth.periodogram(y, plot = TRUE, spar = 0)
}

\arguments{
  \item{y}{
data vector.
}
  \item{plot}{
logical argument which allows to plot the periodogram.
}
  \item{spar}{
smoothing parameter, typically (but not necessarily) in (0,1].
}
}

\details{
\code{smooth.periodogram} computes the periodogram from \code{y} vector and then smooth it with \emph{smoothing spline} method, which basically approximates a curve using a cubic spline (see more details in \code{\link[stats]{smooth.spline}}).
\eqn{\lambda} is the Fourier frequency obtained through \code{\link{periodogram}}.

It must have caution with the minimum length of \code{y}, because \code{smooth.spline} requires the entered vector has at least length 4 and the length of \code{y} does not equal to the length of the data of the periodogram that \code{smooth.spline} receives.
If it presents problems with tol (\bold{tol}erance), see \code{\link[stats]{smooth.spline}}.
}

\value{
A list with the following components:
\item{smooth.periodogram }{vector with the smoothing periodogram values.}
\item{lambda }{vector coresponding to Fourier frequency. See details above.}
}

\author{
Ricardo Olea <raolea@uc.cl>
}

\seealso{
To more information please check \code{\link[stats]{smooth.spline}} and \code{\link{periodogram}}.
}


\examples{
## Require "rdatamarket"
library(rdatamarket)

malleco = dmlist("22tn")
mammothcreek = dmlist("22r7")

## Example 1: malleco
periodogram(malleco$Value)
aux = smooth.periodogram(malleco$Value, plot = FALSE, spar = .8)
lines(aux$smooth.periodogram ~ aux$lambda, lwd = 2, col = "orange")

## Example 2: mammothcreek$Value
periodogram(mammothcreek$Value)
aux = smooth.periodogram(mammothcreek$Value, plot = FALSE, spar = .4)
lines(aux$smooth.periodogram ~ aux$lambda, lwd = 2, col = "orange")

## Example 3: AR(1) simulated
ts.sim = arima.sim(n = 1000, model = list(order = c(1,0,0), ar = 0.7))
periodogram(ts.sim)
aux = smooth.periodogram(ts.sim, plot = FALSE, spar = .7)
lines(aux$smooth.periodogram ~ aux$lambda, lwd = 2, col = "orange")
lines(fdensity(ar = 0.7, lambda = aux$lambda)~aux$lambda, col = "red")

}

\keyword{ Fourier }
\keyword{ periodogram }
\keyword{ smooth }
\keyword{ timeseries }