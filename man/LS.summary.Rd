\name{LS.summary}
\alias{LS.summary}
\alias{summary}

\title{
Summary for Locally Stationary Time Series
}

\description{
Function used to produce summaries of the results to Whittle estimator to Locally Stationary Time Series (\code{\link{LS.whittle}} function).
}

\usage{
LS.summary(object)}

\arguments{
  \item{object}{
\code{\link{LS.whittle}} function.
}
}

\value{
A list with the following components:
\item{summary }{a resume table with estimate, std. error, z-value and p-value of the model.}
\item{aic }{AIC of the model.}
\item{npar }{number of parameters in the model.}
}


\author{
Ricardo Olea <raolea@uc.cl>
}


\keyword{ summary }
\keyword{ locally }
\keyword{ timeseries }