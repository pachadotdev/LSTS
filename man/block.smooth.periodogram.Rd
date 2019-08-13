\name{block.smooth.periodogram}
\alias{block.smooth.periodogram}

\title{
Smooth Periodogram by blocks
}
\description{
Plot in 3D the smoothing periodogram of a time series, by blocks or windows.
}

\usage{
block.smooth.periodogram(y, x = NULL, N = NULL, S = NULL, p = 0.25,
                         spar.freq = 0, spar.time = 0, theta = 0, phi = 0,
                         xlim = NULL, ylim = NULL, zlim = NULL, ylab = "Time",
                         palette.col = NULL)
}

\arguments{
  \item{y}{
 data vector.
}
  \item{x}{
optional vector, if \code{x=NULL} then the function uses \code{x=1:n} where \code{n} is the length of \code{y}. More details in '\code{y}' argument from \code{\link[graphics]{persp}} function.
}
  \item{N}{
value corresponding to the length of the window to compute periodogram. If \code{N=NULL} then the function will use \eqn{N = \textmd{trunc}(n^{0.8})}, see Dahlhaus (1998) where \eqn{n} is the length of the \code{y} vector.
}
  \item{S}{
value corresponding to the lag with which will be taking the blocks or windows to calculate the periodogram.
}
  \item{p}{
value used if it is desired that \code{S} is proportional to \code{N}. By default \code{p=0.25}, if \code{S} and \code{N} are not entered.
}
  \item{spar.freq, spar.time}{
smoothing parameter, typically (but not necessarily) in (0,1].
}
  \item{theta, phi}{
angles defining the viewing direction. \code{theta} gives the azimuthal direction and \code{phi} the colatitude.
}
  \item{xlim, ylim, zlim}{
x-,y- and z-limits. They are \code{NULL} by default and they are optionals parameters.
}
  \item{ylab}{
title for 'y' axis. Optional argument, by default is \code{ylab="Time"}. This must be character strings.
}
  \item{palette.col}{
colors palette.
}
}

\details{
The number of windows of the function is \eqn{M = \textmd{trunc}((n-N)/S+1)}, where \code{\link[base]{trunc}} truncates de entered value and \emph{n} is the length of the vector \code{y}. All windows are of the same length \code{N}, if this value isn't entered by user then is computed as \eqn{N=\textmd{trunc}(n^{0.8})} (Dahlhaus).

\code{block.smooth.periodogram} computes the periodogram in each of the \emph{M} windows and then smoothes it two times with \code{\link[stats]{smooth.spline}} function; the first time using \code{spar.freq} parameter and the second time with \code{spar.time}. These windows overlap between them.

The surface is then viewed by looking at the origin from a direction defined by \code{theta} and \code{phi}. If \code{theta} and \code{phi} are both zero the viewing direction is directly down the negative y axis. Changing \code{theta} will vary the azimuth and changing \code{phi} the colatitude.
}

\references{
Dahlhaus, R. \emph{Fitting time series models to nonstationary processes}. The Annals of Statistics. 1997; Vol. 25, No. 1:1-37.

Dahlhaus, R. and Giraitis, L. \emph{On the optimal segment length for parameter estimates for locally stationary time series}. Journal of Time Series Analysis. 1998; 19(6):629-655.
}

\author{
Ricardo Olea <raolea@uc.cl>
}

\seealso{
See \code{\link{periodogram}} and \code{\link[graphics]{persp}} to more details.
}

\examples{
## Require "rdatamarket"
library(rdatamarket)

malleco = dmlist("22tn")
mammothcreek = dmlist("22r7")


## Example 1
block.smooth.periodogram(y = malleco$Value, x = malleco$Year, spar.freq = .7,
                         spar.time = .7, theta = 30, phi =  0, N = 300, S = 50, 
                         ylim = c(1200,2000), ylab = "Year")
block.smooth.periodogram(y = malleco$Value, x = malleco$Year, spar.freq = .7, 
                         spar.time = .7, theta = 45, phi = 45, N = 300, S = 50,
                         ylim = c(1200,2000), ylab = "Year")
block.smooth.periodogram(y = malleco$Value, x = malleco$Year, spar.freq = .7, 
                         spar.time = .7, theta = 90, phi =  0, N = 300, S = 50,
                         ylim = c(1200,2000), ylab = "Year")

## Example 2
block.smooth.periodogram(y = mammothcreek$Value, x = mammothcreek$Year, spar.freq = .7,
                         spar.time = .7, theta = 30, phi =  0, N = 400, S = 50,
                         ylim = c(-10,2000), ylab = "Year")
block.smooth.periodogram(y = mammothcreek$Value, x = mammothcreek$Year, spar.freq = .7,
                         spar.time = .7, theta = 45, phi = 45, N = 400, S = 50, 
                         ylim = c(-10,2000), ylab = "Year")
block.smooth.periodogram(y = mammothcreek$Value, x = mammothcreek$Year, spar.freq = .7, 
                         spar.time = .7, theta = 90, phi = 0, N = 400, S = 50, 
                         ylim = c(-10,2000), ylab = "Year")

## Example 3: Simulated AR(2)
set.seed(2015)
ts.sim = arima.sim(n = 1000, model = list(order = c(2,0,0), ar = c(1.3, -0.6)))
block.smooth.periodogram(y = ts.sim, spar.freq = .9, spar.time = .9, theta = 30, phi = 00, 
                         N = 500, S = 100, ylab = "Time")
block.smooth.periodogram(y = ts.sim, spar.freq = .9, spar.time = .9, theta = 00, phi = 00, 
                         N = 500, S = 100, ylab = "Time")
block.smooth.periodogram(y = ts.sim, spar.freq = .9, spar.time = .9, theta = 90, phi = 00,
                         N = 500, S = 100, ylab = "Time")
block.smooth.periodogram(y = ts.sim, spar.freq = .9, spar.time = .9, theta = 45, phi = 15, 
                         N = 500, S = 100, ylab = "Time")
block.smooth.periodogram(y = ts.sim, spar.freq = .9, spar.time = .9, theta = 45, phi = 15, 
                         N = 500, S = 100, ylab = "Time", 
                         palette.col = gray(level = seq(0.2,0.9,0.1 )))
}

\keyword{ Fourier }
\keyword{ smooth }
\keyword{ timeseries }