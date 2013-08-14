\name{h.tcv}
\alias{h.tcv}
\alias{h.tcv.default}
\alias{print.h.tcv}
\title{
Trimmed Cross-Validation for Bandwidth Selection
}
\description{
  The (S3) generic function \code{h.tcv} computes the trimmed 
  cross-validation bandwidth selector of r'th derivative of 
  kernel density estimator one-dimensional.
}
\usage{
h.tcv(x, \dots)
\method{h.tcv}{default}(x, deriv.order = 0, lower = 0.1 * hos, upper = 2 * hos, 
         tol = 0.1 * lower, kernel = c("gaussian", "epanechnikov", "uniform", 
         "triangular", "triweight", "tricube", "biweight", "cosine"), \dots)
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
  \code{h.tcv} trimmed cross-validation implements for choosing the bandwidth \eqn{h} 
  of a r'th derivative kernel density estimator.\cr
  
  Feluch and Koronacki (1992) proposed a so-called trimmed cross-validation (TCV) in kernel 
  density estimator, a simple modification of the unbiased (least-squares)  cross-validation 
  criterion. We consider the following "trimmed" version of "unbiased", to be minimized with 
  respect to \eqn{h}:
  \deqn{\int \left(\hat{f}_{h}^{(r)}(x)\right)^{2} - 2 \frac{(-1)^{r}}{n(n-1) h^{2r+1}} \sum_{i=1}^{n}\sum_{j=1; j \neq i} K^{(2r)} \left(\frac{X_{j}-X_{i}}{h}\right)\chi\left(|X_{i}-X_{j}| > c_{n}\right)}{int (hat(f)(x;r))^2 - 2 (-1)^r / n (n-1)h^(2r+1) sum(sum(K(x(j)-x(i)/h;2r)),i=1...n,j=1...n, j!=i) chi(|X(i)-X(j)| > c(n))}
  where \eqn{\chi(.)}{chi(.)} denotes the indicator function and \eqn{c_{n}}{c(n)} is a sequence of positive 
  constants, \eqn{c_{n}/ h^{2r+1} \rightarrow 0}{c(n)/h^(2r+1) --> 0} as \eqn{n \rightarrow \infty}{n --> Inf}, and 
  \deqn{\int \left(\hat{f}_{h}^{(r)}(x)\right)^{2} = \frac{R\left(K^{(r)}\right)}{nh^{2r+1}} + \frac{(-1)^{r}}{n (n-1) h^{2r+1}} \sum_{i=1}^{n}\sum_{j=1;j \neq i}^{n} K^{(r)} \ast K^{(r)} \left(\frac{X_{j}-X_{i}}{h}\right)}{int (hat(f)(x;r))^2 dx = R(k(x;r))/n h^(2r+1) +  (-1)^r / n (n-1) h^(2r+1) sum(sum(K(.;r)*K(.;r)(x(j)-x(i)/h)), i=1...n, j=1...n, j != i)}
  the trimmed cross-validation function is defined by:
  \deqn{TCV(h;r) = \frac{R\left(K^{(r)}\right)}{nh^{2r+1}} + \frac{(-1)^{r}}{n(n-1)h^{2r+1}}\sum_{i=1}^{n} \sum_{j=1;j \neq i}^{n} \varphi^{(r)} \left(\frac{X_{j}-X_{i}}{h}\right)}{TCV(h;r) =  R(K(x;r))/ n h^(2r+1) +  (-1)^r / n (n-1) h^(2r+1) sum( sum(varphi(x(j)-x(i)/h;r) ),i=1...n,j=1...n,j != i) }
  whit \deqn{\varphi^{(r)}(c) = \left(K^{(r)} \ast K^{(r)} - 2 K^{(2r)} \chi\left(|c| > c_{n}/h^{2r+1}\right) \right)(c)}{varphi(c;r)= K(c;r)*K(c;r) - 2 K(c;2r) chi( |c| > c(n)/h^(2r+1) )}
  here we take \eqn{c_{n} = 1/n}{c(n) = 1/n}, for assure the convergence. Where \eqn{K^{(r)} \ast K^{(r)} (x)}{K(x;r)*K(x;r)} is the convolution of the r'th derivative kernel function \eqn{K^{(r)}(x)}{K(x;r)}
  (see \code{\link{kernel.conv}} and \code{\link{kernel.fun}}).\cr
  
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
  \item{min.tcv}{the minimal TCV value.}
}
\references{
  Feluch, W. and Koronacki, J. (1992).
  A note on modified cross-validation in density estimation.
  \emph{Computational Statistics and Data Analysis}, \bold{13}, 143--151.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{plot.h.tcv}}.
}
\examples{
## Derivative order = 0

h.tcv(kurtotic,deriv.order = 0)

## Derivative order = 1

h.tcv(kurtotic,deriv.order = 1)
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
