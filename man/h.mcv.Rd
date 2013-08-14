\name{h.mcv}
\alias{h.mcv}
\alias{h.mcv.default}
\alias{print.h.mcv}
\title{
Modified Cross-Validation for Bandwidth Selection
}
\description{
  The (S3) generic function \code{h.mcv} computes the modified 
  cross-validation bandwidth selector of r'th derivative of 
  kernel density estimator one-dimensional.
}
\usage{
h.mcv(x, \dots)
\method{h.mcv}{default}(x, deriv.order = 0, lower = 0.1 * hos, upper = 2 * hos, 
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
  \code{h.mcv} modified cross-validation implements for choosing the bandwidth \eqn{h} 
  of a r'th derivative kernel density estimator.\cr
  
  Stute (1992) proposed a so-called modified cross-validation (MCV) in kernel 
  density estimator. This method can be extended to the estimation of derivative 
  of a density, the essential idea based on approximated the problematic term 
  by the aid of the Hajek projection (see Stute 1992). The minimization criterion is defined by:
  \deqn{MCV(h;r) = \frac{R\left(K^{(r)}\right)}{nh^{2r+1}} + \frac{(-1)^{r}}{n(n-1)h^{2r+1}}\sum_{i=1}^{n} \sum_{j=1;j \neq i}^{n} \varphi^{(r)} \left(\frac{X_{j}-X_{i}}{h}\right)}{MCV(h;r) =  R(K(x;r))/ n h^(2r+1) +  (-1)^r / n (n-1) h^(2r+1) sum( sum(varphi(x(j)-x(i)/h;r) ),i=1...n,j=1...n,j != i) }
  whit \deqn{\varphi^{(r)}(c) = \left(K^{(r)} \ast K^{(r)} - K^{(2r)} - \frac{\mu_{2}(K)}{2}K^{(2r+2)} \right)(c)}{varphi(c;r)= K(c;r)*K(c;r) - K(c;2r) - 0.5 mu(K(c)) K(c;2r+2)} 
  and \eqn{K^{(r)} \ast K^{(r)} (x)}{K(x;r)*K(x;r)} is the convolution of the r'th derivative kernel function \eqn{K^{(r)}(x)}{K(x;r)}
  (see \code{\link{kernel.conv}} and \code{\link{kernel.fun}}); \eqn{R\left(K^{(r)}\right) = \int_{R} K^{(r)}(x)^{2} dx}{R(K(x;r)) = int K(x;r)^2 dx} and \eqn{\mu_{2}(K) = \int_{R}x^{2} K(x) dx}{mu(K(x)) = int x^2 K(x) dx}.\cr
  
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
  \item{min.mcv}{the minimal MCV value.}
}
\references{
  Heidenreich, N. B., Schindler, A. and Sperlich, S. (2013).
  Bandwidth selection for kernel density estimation: 
  a review of fully automatic selectors.
  \emph{Advances in Statistical Analysis}.
  
  Stute, W. (1992).
  Modified cross validation in density estimation.
  \emph{Journal of Statistical Planning and Inference}, \bold{30}, 293--305.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{plot.h.mcv}}.
}
\examples{
## Derivative order = 0

h.mcv(kurtotic,deriv.order = 0)

## Derivative order = 1

h.mcv(kurtotic,deriv.order = 1)

}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}

