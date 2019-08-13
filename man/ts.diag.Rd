\name{ts.diag}
\alias{ts.diag}

\title{
Diagnostic Plots for Time Series fits
}

\description{
Plot time-series diagnostics.
}

\usage{
ts.diag(x, lag = 10, cex = 0.5)}

\arguments{
  \item{x}{
residuals of the fitted time series model.
}
  \item{lag}{
maximum lag at which to calculate the acf and Ljung-Box test.
}
  \item{cex}{
optional argument of \code{\link[graphics]{par}}.
}
}

\details{
This function plot the residuals, the autocorrelation function of the residuals (ACF) and the p-values of the Ljung-Box Test for all lags up to \code{lag}.
}


\value{
Diagnostics are plotted.
}

\author{
Ricardo Olea <raolea@uc.cl>
}

\examples{
z = rnorm(500)
Box.Ljung.Test(z, lag=15)
ts.diag(z)
}

\keyword{ diagnostic }
\keyword{ ljung }
\keyword{ acf }
\keyword{ residuals }
\keyword{ timeseries }