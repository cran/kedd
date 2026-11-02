\name{kedd-data}
\docType{data}
\alias{kedd-data}
\alias{bimodal}
\alias{claw}
\alias{kurtotic}
\alias{outlier}
\alias{trimodal}
\title{Samples from Marron and Wand (1992) Normal Mixture Densities}
\description{
  Five samples of size 200 drawn from normal mixtures of Marron and Wand
  (1992), classical test cases for density (derivative) estimation and
  bandwidth selection.  Writing \eqn{N(\mu, \sigma^2)} for a normal density:
  \describe{
    \item{\code{kurtotic}}{kurtotic unimodal density (#4):
      \eqn{\frac23 N(0,1) + \frac13 N(0, (1/10)^2)}{2/3 N(0,1) + 1/3 N(0,(1/10)^2)}.}
    \item{\code{outlier}}{outlier density (#5):
      \eqn{\frac1{10} N(0,1) + \frac9{10} N(0, (1/10)^2)}{1/10 N(0,1) + 9/10 N(0,(1/10)^2)}.}
    \item{\code{bimodal}}{separated bimodal density (#7):
      \eqn{\frac12 N(-\frac32, (\frac12)^2) + \frac12 N(\frac32, (\frac12)^2)}{1/2 N(-3/2,(1/2)^2) + 1/2 N(3/2,(1/2)^2)}.}
    \item{\code{trimodal}}{trimodal density (#9):
      \eqn{\frac9{20} N(-\frac65, (\frac35)^2) + \frac9{20} N(\frac65, (\frac35)^2) + \frac1{10} N(0, (\frac14)^2)}{9/20 N(-6/5,(3/5)^2) + 9/20 N(6/5,(3/5)^2) + 1/10 N(0,(1/4)^2)}.}
    \item{\code{claw}}{claw density (#10):
      \eqn{\frac12 N(0,1) + \sum_{l=0}^{4} \frac1{10} N(\frac l2 - 1, (\frac1{10})^2)}{1/2 N(0,1) + sum_{l=0}^4 1/10 N(l/2 - 1, (1/10)^2)}.}
  }
}
\usage{
bimodal
claw
kurtotic
outlier
trimodal
}
\format{Each object is a numeric vector of length 200.}
\details{
  The datasets are loaded lazily: \code{claw} is available as soon as the
  package is attached (\code{data(claw)} also works).  The true densities are
  known, so the estimates \eqn{\hat f_h^{(r)}} and the selected bandwidths can
  be compared with the truth.
}
\source{
  Simulated samples, as in previous versions of \pkg{kedd}.
}
\references{
  Marron, J. S. and Wand, M. P. (1992).
  Exact mean integrated squared error.
  \emph{The Annals of Statistics}, \bold{20}, 712--736.
}
\seealso{\code{\link{dkde}}, \code{\link{h.ucv}}, \code{\link{kedd-package}}.}
\examples{
## the five samples
sapply(list(kurtotic = kurtotic, outlier = outlier, bimodal = bimodal,
            trimodal = trimodal, claw = claw), summary)

## claw density: estimate with an MCV bandwidth and the true density
bw <- h.mcv(claw)
f_claw <- function(x) 0.5 * dnorm(x) +
  rowSums(sapply(0:4, function(l) 0.1 * dnorm(x, l / 2 - 1, 0.1)))
p <- plot(dkde(data = claw, h = bw@h))
p + ggplot2::stat_function(fun = f_claw, linetype = "dashed")

## first derivative of the bimodal sample
plot(dkde(data = bimodal, h = h.ucv(bimodal, deriv.order = 1)@h, deriv.order = 1))
}
\keyword{datasets}
