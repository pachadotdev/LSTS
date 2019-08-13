\name{LS.whittle}
\alias{LS.whittle}
\alias{whittle}
\alias{estimator}

\title{
Whittle estimator to Locally Stationary Time Series
}

\description{
This function computes Whittle estimator to LS-ARMA and LS-ARFIMA models.
}

\usage{
LS.whittle(series, start, order = c(p = 0, q = 0), ar.order = NULL,
           ma.order = NULL, sd.order = NULL, d.order = NULL, include.d = FALSE, 
           N = NULL, S = NULL, include.taper = TRUE, control = list(),
           lower = -Inf, upper = Inf, m = NULL, n.ahead = 0)
}

\arguments{
  \item{series}{
data vector.
}
  \item{start}{
numeric vector, initial values for the parameters to be optimized.
}
    \item{order}{
vector with the specification of the ARMA model: the two integers components (p, q) are the AR order and the MA order.
}
  \item{ar.order, ma.order}{
AR and MA polimonial order, respectively. See details below.
}
  \item{sd.order}{
polinomial order noise scale factor. See details below.
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
  \item{control}{
A list of control parameters. More details in \code{\link[stats]{nlminb}} .
}
  \item{lower, upper}{
vectors of lower and upper bounds, replicated to be as long as \code{start}. If unspecified, all parameters are assumed to be unconstrained.  
}
  \item{m}{
truncation order of the MA infinity process, by default \eqn{m = 0.25*n^{0.8}}. Parameter used in \code{LS.kalman}.
}
  \item{n.ahead}{
The number of steps ahead for which prediction is required. By default is zero.
}
}

\details{
This function estimates the parameters in models: LS-ARMA 
\deqn{\Phi(t/T, \, B)\, Y_{t, T} = \Theta(t/T,\, B)\,\sigma(t/T)\, \varepsilon_t} and LS-ARFIMA \deqn{\Phi(t/T, \, B)\, Y_{t, T} = \Theta(t/T,\, B)\, (1-B)^{-d(t/T)}\, \sigma(t/T)\, \varepsilon_t,} with infinite moving average expansion \deqn{Y_{t, T} = \sigma(t/T)\, \sum_{j=0}^{\infty} \psi(t/T)\,\varepsilon_t,} for \eqn{t = 1,\ldots, T}, where for \eqn{u = t/T \in [0,1]}, \eqn{\Phi(u,B)=1+\phi_1(u)B +\cdots+\phi_p(u)B^p} is an autoregressive polynomial, \eqn{\Theta(u, B) = 1 + \theta_1(u)B + \cdots  + \theta_q(u)B^q} is a moving average polynomial, \eqn{d(u)} is a long-memory parameter, \eqn{\sigma(u)} is a noise scale factor and \eqn{\{\varepsilon_t \}} is a Gaussian white noise sequence with zero mean and unit variance. This class of models extends the well-known ARMA and ARFIMA process, which is obtained when the components \eqn{\Phi(u, B)}, \eqn{\Theta(u, B)}, \eqn{d(u)} and \eqn{\sigma(u)} do not depend on \eqn{u}.

The evolution of these models can be specified in terms of a general class of functions. For example, let \eqn{\{g_j(u)\}}, \eqn{j = 1, 2, \ldots}, be a basis for a space of smoothly varying functions and let \eqn{d_{\theta}(u)} be the time-varying long-memory parameter in model LS-ARFIMA. Then we could write \eqn{d_{\theta}(u)} in terms of the basis \eqn{\{g_j(u) = u^j\}} as follows \eqn{d_{\theta}(u) = \sum_{j=0}^{k} \alpha_j\,g_j(u)}
for unknown values of \eqn{k} and \eqn{\theta = (\alpha_0,\,\alpha_1,\,\ldots, \,\alpha_k)^{\prime}}. In this situation, estimating \eqn{\theta} involves determining \eqn{k} and estimating the coefficients \eqn{\alpha_0,\,\alpha_1,\,\ldots, \,\alpha_k}.

\code{LS.whittle} optimizes \code{\link{LS.whittle.loglik}} as objective function using \code{\link[stats]{nlminb}} function, for both LS-ARMA (\code{include.d=FALSE}) and LS-ARFIMA (\code{include.d=TRUE}) models.
Also computes Kalman filter with \code{\link{LS.kalman}} and this values are given in \code{var.coef} in the output.
}


\value{
A list with the following components:
\item{coef }{The best set of parameters found.}
\item{var.coef }{covariance matrix approximated for maximum likelihood estimator \eqn{\hat{\theta}} of \eqn{\theta:=(\theta_1,\ldots,\theta_k)^{\prime}}. This matrix is approximated by \eqn{H^{-1}/n}, where \eqn{H} is the Hessian matrix \eqn{[\partial^2 \ell(\theta)/\partial\theta_i\partial\theta_j]_{i,j=1}^{k}}.}
\item{loglik }{log-likelihood of \code{coef}, calculated with \code{\link{LS.whittle.loglik}}.}
\item{aic }{Akaike's `An Information Criterion', for one fitted model LS-ARMA or LS-ARFIMA. The formula is -2*log-likelihood + 2*npar/n, where \emph{npar} represents the number of parameters in the fitted model and \emph{n} equal to the length of the \code{series}.}
\item{series }{original time serie.}
\item{residuals }{standard residuals.}
\item{fitted.values }{model fitted values.}
\item{pred }{predictions of the model.}
\item{se }{the estimated standard errors.}
\item{model }{A list representing the fitted model.}
}

\author{
Ricardo Olea <raolea@uc.cl>
}

\seealso{
\code{\link[stats]{nlminb}} for optimization.
\code{\link{LS.kalman}} for Kalman filter.
}

\examples{
## Require "rdatamarket"
library(rdatamarket)
malleco = dmlist("22tn")
y = malleco$Value

# Analysis by blocks of phi and sigma parameters
T. = length(y)
N = 200
S = 100
M = trunc((T. - N)/S + 1)
table = c()
for(j in 1:M){
x = y[(1 + S*(j-1)):(N + S*(j-1))]
table = rbind(table,nlminb(start = c(0.65, 0.15), N = N, objective = LS.whittle.loglik,
                           series = x, order = c(p = 1, q = 0))$par)
}
u = (N/2 + S*(1:M-1))/T.
table = as.data.frame(cbind(u, table))
colnames(table) = c("u", "phi", "sigma")

spar = 0.6
## start parameters
phi = smooth.spline(table$phi, spar = spar)$y
fit.1 = nls(phi ~ a0+a1*u, start = list(a0 = 0.65, a1 = 0.00))
sigma = smooth.spline(table$sigma, spar = spar)$y
fit.2 = nls(sigma ~ b0+b1*u, start = list(b0 = 0.65, b1 = 0.00))

fit = LS.whittle(series = y, start = c(coef(fit.1), coef(fit.2)), order = c(p = 1, q = 0),
                 ar.order = c(1), sd.order = 1, N = 180, n.ahead = 10)

names(fit)
fit$coef
fit$loglik
}

\keyword{ whittle }
\keyword{ loglik }
\keyword{ estimator }
\keyword{ timeseries }