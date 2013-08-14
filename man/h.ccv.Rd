\name{h.ccv}
\alias{h.ccv}
\alias{h.ccv.default}
\alias{print.h.ccv}
\title{
Complete Cross-Validation for Bandwidth Selection
}
\description{
  The (S3) generic function \code{h.ccv} computes the complete 
  cross-validation bandwidth selector of r'th derivative of 
  kernel density estimator one-dimensional.
}
\usage{
h.ccv(x, \dots)
\method{h.ccv}{default}(x, deriv.order = 0, lower = 0.1 * hos, upper = hos, 
         tol = 0.1 * lower, kernel = c("gaussian", "triweight", 
         "tricube", "biweight", "cosine"), \dots)
}
%- maybe also 'usage' for other objects documented here.
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
  \code{h.ccv} complete cross-validation implements for choosing the bandwidth \eqn{h} 
  of a r'th derivative kernel density estimator.\cr
  
  Jones and Kappenman (1991) proposed a so-called complete cross-validation (CCV)
  in kernel density estimator. This method can be extended to the estimation of 
  derivative of the density, basing our estimate of integrated squared density 
  derivative (Peter and Marron 1987) on the \eqn{\bar{\theta}_{r}(h)}{bar(theta)(h;r)}'s, 
  we get the following, start from \eqn{R\left(\hat{f}_{h}^{(r)}\right) - \bar{\theta}_{r}(h)}{R(hat(f)(h;r)) - bar(theta)(h;r)} as an estimate 
  of MISE. Thus, \eqn{\hat{h}^{(r)}_{CCV}}{h(r)_(CCV)}, say, is the \eqn{h} that minimises:
  \deqn{CCV(h;r)=R\left(\hat{f}_{h}^{(r)}\right)-\bar{\theta}_{r}(h)+\frac{1}{2}\mu_{2}(K) h^{2} \bar{\theta}_{r+1}(h)+\frac{1}{24}\left(6\mu_{2}^{2}(K) -\delta(K)\right)h^{4}\bar{\theta}_{r+2}(h)}{CCV(h;r)= R(K(x;r))/ n h^(2r+1) + R(hat(f)(h;r))- bar(theta)(h;r) + 0.5 mu(K(x)) h^2 bar(theta)(h;r+1) + 1/24 (6 mu(K(x))^2 - delta(K(x))) h^4 bar(theta)(h;r+2)} 
  with 
  \deqn{R\left(\hat{f}_{h}^{(r)}\right) = \int \left(\hat{f}_{h}^{(r)}(x)\right)^{2} dx = \frac{R\left(K^{(r)}\right)}{nh^{2r+1}} + \frac{(-1)^{r}}{n (n-1) h^{2r+1}} \sum_{i=1}^{n}\sum_{j=1;j \neq i}^{n} K^{(r)} \ast K^{(r)} \left(\frac{X_{j}-X_{i}}{h}\right)}{R(hat(f)(h;r)) = int (hat(f)(x;r))^2 dx =R(k(x;r))/n h^(2r+1) + (-1)^r / n (n-1) h^(2r+1) sum(sum(K(.;r)*K(.;r)(x(j)-x(i)/h)), i=1...n, j=1...n, j != i)}
  and
  \deqn{\bar{\theta}_{r}(h)= \frac{(-1)^r}{n(n-1) h^{2r+1}} \sum_{i=1}^{n} \sum_{j=1;j \neq i}^{n} K^{(2r)} \left(\frac{X_{j}-X_{i}}{h}\right)}{bar(theta)(h;r) = (-1)^r / n(n-1) h^(2r+1) sum(sum(K((x(j)-x(i)/h);2r)), i=1...n, j=1...n, j != i)}
  and \eqn{K^{(r)} \ast K^{(r)} (x)}{K(x;r)*K(x;r)} is the convolution of the r'th derivative kernel function \eqn{K^{(r)}(x)}{K(x;r)}
  (see \code{\link{kernel.conv}} and \code{\link{kernel.fun}}); \eqn{R\left(K^{(r)}\right) = \int_{R} K^{(r)}(x)^{2} dx}{R(K(x;r)) = int K(x;r)^2 dx} and 
  \eqn{\mu_{2}(K) = \int_{R}x^{2} K(x) dx}{mu(K(x)) = int x^2 K(x) dx}, \eqn{\delta(K) = \int_{R}x^{4} K(x) dx}{delta(K(x)) = int x^4 K(x) dx}.\cr
  
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
  \item{min.ccv}{the minimal CCV value.}
}
\references{
  Jones, M. C. and Kappenman, R. F. (1991). 
  On a class of kernel density estimate bandwidth selectors. 
  \emph{Scandinavian Journal of Statistics}, \bold{19}, 337--349.

  Peter, H. and Marron, J.S. (1987).
  Estimation of integrated squared density derivatives.
  \emph{Statistics and Probability Letters}, \bold{6}, 109--115.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{plot.h.ccv}}.
}
\examples{
## Derivative order = 0

h.ccv(kurtotic,deriv.order = 0)

## Derivative order = 1

h.ccv(kurtotic,deriv.order = 1)
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
