\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{h.amise}
\alias{h.amise}
\alias{h.amise.default}
\alias{print.h.amise}
\title{
AMISE for Optimal Bandwidth Selectors
}
\description{
  The (S3) generic function \code{h.amise} evaluates the asymptotic 
  mean integrated squared error \bold{AMISE} for optimal smoothing
  parameters \eqn{h} of r'th derivative of kernel density 
  estimator one-dimensional.
}
\usage{
h.amise(x, \dots)
\method{h.amise}{default}(x, deriv.order = 0, lower = 0.1 * hos, upper = 2 * hos, 
         tol = 0.1 * lower, kernel = c("gaussian", "epanechnikov", "triweight", 
         "tricube", "biweight", "cosine"), \dots)
}
\arguments{
  \item{x}{vector of data values.}
  \item{deriv.order}{derivative order (scalar).}
  \item{lower, upper}{range over which to minimize. The default is
    almost always satisfactory. \code{hos} (Over-smoothing) is calculated internally
    from an \code{kernel}, see details.}
  \item{tol}{the convergence tolerance for \code{\link{optimize}}.}
  \item{kernel}{a character string giving the smoothing kernel to be used, with default
    \code{"gaussian"}.}
  \item{\dots}{further arguments for (non-default) methods.}
}
\details{
  \code{h.amise} asymptotic mean integrated squared error implements for choosing 
  the optimal bandwidth \eqn{h} of a r'th derivative kernel density estimator.\cr
  
  We Consider the following AMISE version of the r'th derivative of \eqn{f} the r'th 
  derivative of the kernel estimate (see Scott 1992, pp 131):
  \deqn{AMISE(h;r)= \frac{R\left(K^{(r)}\right)}{nh^{2r+1}} + \frac{1}{4} h^{4} \mu_{2}^{2}(K) R\left(f^{(r+2)}\right)}{AMISE(h;r)= R(K(x;r))/ n h^(2r+1) + 0.25 mu(K(x))^2 h^4 R(f(x;r+2))}
  The optimal bandwidth minimizing this function is:
  \deqn{h_{(r)}^{\ast} = \left[\frac{(2r+1)R\left(K^{(r)}\right)}{\mu_{2}^{2}(K) R\left(f^{(r+2)}\right)}\right]^{1/(2r+5)} n^{-1/(2r+5)}}{h(r) = ( ((2r+1)R(K(x;r)))/(mu(K(x))^2 R(f(x;r+2))) )^(1/(2r+5)) n^-1/(2r+5)}
  whereof
  \deqn{\inf_{h > 0} AMISE(h;r) = \frac{2r+5}{4} R\left(K^{(r)}\right)^{\frac{4}{(2r+5)}} \left[ \frac{\mu_{2}^{2}(K)R\left(f^{(r+2)}\right)}{2r+1} \right]^{\frac{2r+1}{2r+5}} n^{-\frac{4}{2r+5}}}{inf AMISE(h;r) = 2r+5/4 R(K(x;r))^(4/2r+5) ((mu(K(x))^2 R(f(x;r+2)))/(2r+1))^((2r+1)/(2r+5)) n^-4/2r+5}
  which is the smallest possible AMISE for estimation of \eqn{f^{(r)}(x)}{f(x;r)} using the kernel \eqn{K(x)}{K(x)}, 
  where \eqn{R\left(K^{(r)}\right) = \int_{R} K^{(r)}(x)^{2} dx}{R(K(x;r)) = int K(x;r)^2 dx} and \eqn{\mu_{2}(K) = \int_{R}x^{2} K(x) dx}{mu(K(x)) = int x^2 K(x) dx}.\cr
  
  The range over which to minimize is \code{hos} Oversmoothing bandwidth, the default is almost always 
  satisfactory. See George and Scott (1985), George (1990), Scott (1992, pp 165), Wand and Jones (1995, pp 61).
}
\value{
  \item{x}{data points - same as input.}
  \item{data.name}{the deparsed name of the \code{x} argument.}
  \item{n}{the sample size after elimination of missing values.}
  \item{kernel}{name of kernel to use}
  \item{deriv.order}{the derivative order to use.}
  \item{h}{value of bandwidth parameter.}
  \item{amise}{the AMISE value.}
}
\references{
  Bowman, A. W. and Azzalini, A. (1997).
  \emph{Applied Smoothing Techniques for
  Data Analysis: the Kernel Approach with 
  S-Plus Illustrations}.
  Oxford University Press, Oxford.
  
  Radhey, S. S. (1987).
  MISE of kernel estimates of a density and its derivatives.
  \emph{Statistics and Probability Letters}, \bold{5}, 153--159.
  
  Scott, D. W. (1992).
  \emph{Multivariate Density Estimation. Theory, Practice and Visualization}.
  New York: Wiley.
  
  Sheather, S. J. (2004).
  Density estimation.
  \emph{Statistical Science}, \bold{19}, 588--597.
  
  Silverman, B. W. (1986).
  \emph{Density Estimation for Statistics and Data Analysis}.
  Chapman & Hall/CRC. London.
 
  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing}.
  Chapman and Hall, London.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{plot.h.amise}}, see \code{\link[sm]{nmise}} in package \CRANpkg{sm} this function 
evaluates the mean integrated squared error of a density estimate (\code{deriv.order = 0}) 
which is constructed from data which follow a normal distribution.
}
\examples{
## Derivative order = 0

h.amise(kurtotic,deriv.order = 0)

## Derivative order = 1

h.amise(kurtotic,deriv.order = 1)
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
