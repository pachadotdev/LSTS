\name{LS.kalman}
\alias{LS.kalman}
\alias{kalman}
\alias{filter}

\title{
Kalman filter for locally stationary processes
}

\description{
This function run the state-space equations for expansion infinite of moving average in processes LS-ARMA or LS-ARFIMA.
}

\usage{
LS.kalman(series, start, order = c(p = 0, q = 0), ar.order = NULL, ma.order = NULL,
	  sd.order = NULL, d.order = NULL, include.d = FALSE, m = NULL)}

\arguments{
  \item{series}{
univariate time series.
}
  \item{start}{
numeric vector, initial values for parameters to run the model.
}
  \item{order}{
vector corresponding to \code{ARMA} model entered.
}
  \item{ar.order, ma.order}{
AR and MA polimonial order respectively.
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
  \item{m}{
truncation order of the MA infinity process. By default \eqn{m = 0.25*n^{0.8}} where \code{n} the length of \code{series}.
}
}

\details{
The model fit is done using the Whittle likelihood, while the generation of innovations is through Kalman Filter.
Details about \code{ar.order, ma.order, sd.order} and \code{d.order} can be viewed in \code{\link{LS.whittle.loglik}}.
}

\value{
A list with:
\item{residuals }{standard residuals.}
\item{fitted.values }{model fitted values.}
\item{delta }{variance prediction error.}
}

\references{
Brockwell, Peter J., and Richard A. Davis. \emph{Introduction to time series and forecasting}. 2002. ISBN-13: 978-0387953519.

Palma W. \emph{Long-Memory Time Series. Theory and Methods}. 1st ed. New Jersey: John Wiley & Sons, Inc.; 2007. 285 p.

Palma W, Olea R, & Ferreira G. \emph{Estimation and forecasting of locally stationary processes}. Journal of Forecasting. 2013; VOl. 32, No. 1:86-96.
}

\author{
Ricardo Olea <raolea@uc.cl>
}


\keyword{ kalman }
\keyword{ filter }
\keyword{ locally }
\keyword{ timeseries }