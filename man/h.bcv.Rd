\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{h.bcv}
\alias{h.bcv}
\alias{h.bcv.default}
\alias{print.h.bcv}
\title{
Biased Cross-Validation for Bandwidth Selection
}
\description{
  The (S3) generic function \code{h.bcv} computes the biased 
  cross-validation bandwidth selector of r'th derivative of 
  kernel density estimator one-dimensional.
}
\usage{
h.bcv(x, \dots)
\method{h.bcv}{default}(x, whichbcv = 1, deriv.order = 0, lower = 0.1 * hos, upper = 2 * hos, 
         tol = 0.1 * lower, kernel = c("gaussian","epanechnikov",
         "triweight","tricube","biweight","cosine"), \dots)
}
\arguments{
  \item{x}{vector of data values.}
  \item{whichbcv}{method selected, \code{1 = BCV1} or \code{2 = BCV2}, see details.}
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
  \code{h.bcv} biased cross-validation implements for choosing the bandwidth \eqn{h} of a 
  r'th derivative kernel density estimator. if \code{whichbcv = 1} then \bold{BCV1} is selected
  (Scott and George 1987), and if \code{whichbcv = 2} used \bold{BCV2} (Jones and Kappenman 1991).\cr
  
  Scott and George (1987) suggest a method which has as its immediate target the \bold{AMISE}
  (e.g. Silverman 1986, section 3.3). We denote \eqn{\hat{\theta}_{r}(h)}{hat(theta)(h;r)} and 
  \eqn{\bar{\theta}_{r}(h)}{bar(theta)(h;r)} (Peter and Marron 1987, Jones and Kappenman 1991) by:
  %%\deqn{\hat{\theta}_{r}(h) = (n-1)^{-1} n R\left(\hat{f}_{h}^{(r)}\right) - (n-1)^{-1} h^{-2r-1} R\left(K^{(r)}\right)}{}
  \deqn{\hat{\theta}_{r}(h)= \frac{(-1)^{r}}{n(n-1)h^{2r+1}} \sum_{i=1}^{n} \sum_{j=1;j \neq i}^{n} K^{(r)} \ast K^{(r)} \left(\frac{X_{j}-X_{i}}{h}\right)}{hat(theta)(h;r) = (-1)^r / n(n-1) h^(2r+1) sum(sum(K(.;r)*K(.;r)(x(j)-x(i)/h)), i=1...n, j=1...n, j != i)}
  and
  %%\deqn{\bar{\theta}_{r}(h) = (-1)^{r} n^{-1} \sum_{i=1}^{n} \hat{f}_{h,i}^{(2r)}(X_{i})}{}
  \deqn{\bar{\theta}_{r}(h)= \frac{(-1)^r}{n(n-1) h^{2r+1}} \sum_{i=1}^{n} \sum_{j=1;j \neq i}^{n} K^{(2r)} \left(\frac{X_{j}-X_{i}}{h}\right)}{bar(theta)(h;r) = (-1)^r / n(n-1) h^(2r+1) sum(sum(K((x(j)-x(i)/h);2r)), i=1...n, j=1...n, j != i)}
  Scott and George (1987) proposed using \eqn{\hat{\theta}_{r}(h)}{hat(theta)(h;r)} to estimate \eqn{f^{(r)}(x)}{f(x;r)}. 
  Thus, \eqn{\hat{h}^{(r)}_{BCV1}}{h(r)_(BCV1)}, say, is the \eqn{h} that minimises:
  \deqn{BCV1(h;r)= \frac{R\left(K^{(r)}\right)}{nh^{2r+1}} + \frac{1}{4} \mu_{2}^{2}(K) h^{4} \hat{\theta}_{r+2}(h)}{BCV1(h;r) = R(K(x;r))/ n h^(2r+1) + 0.25 mu(K(x))^2 h^4 hat(theta)(h;r+2)}
  and we define \eqn{\hat{h}^{(r)}_{BCV2}}{h(r)_(BCV2)} as the minimiser of (Jones and Kappenman 1991):
  \deqn{BCV2(h;r)= \frac{R\left(K^{(r)}\right)}{nh^{2r+1}} + \frac{1}{4} \mu_{2}^{2}(K) h^{4} \bar{\theta}_{r+2}(h)}{BCV2(h;r) = R(K(x;r))/ n h^(2r+1) + 0.25 mu(K(x))^2 h^4 bar(theta)(h;r+2)}
  where \eqn{K^{(r)} \ast K^{(r)} (x)}{K(x;r)*K(x;r)} is the convolution of the r'th derivative kernel function \eqn{K^{(r)}(x)}{K(x;r)}
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
  \item{whichbcv}{method selected.}
  \item{h}{value of bandwidth parameter.}
  \item{min.bcv}{the minimal BCV value.}
}
\references{
  Jones, M. C. and Kappenman, R. F. (1991). 
  On a class of kernel density estimate bandwidth selectors. 
  \emph{Scandinavian Journal of Statistics}, \bold{19}, 337--349.
  
  Jones, M. C., Marron, J. S. and Sheather,S. J. (1996).
  A brief survey of bandwidth selection for density estimation. 
  \emph{Journal of the American Statistical Association}, \bold{91}, 401--407.
  
  Peter, H. and Marron, J.S. (1987).
  Estimation of integrated squared density derivatives.
  \emph{Statistics and Probability Letters}, \bold{6}, 109--115.
  
  Scott, D.W. and George, R. T. (1987).
  Biased and unbiased cross-validation in density estimation. 
  \emph{Journal of the American Statistical Association}, \bold{82}, 1131--1146.
  
  Sheather,S. J. (2004).
  Density estimation.
  \emph{Statistical Science}, \bold{19}, 588--597.

  Tarn, D. (2007).
  \CRANpkg{ks}: Kernel density estimation and kernel discriminant
  analysis for multivariate data in \R.
  \emph{Journal of Statistical Software}, \bold{21}(7), 1--16. 
  
  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing}.
  Chapman and Hall, London.
  
  Wolfgang, H. (1991).
  \emph{Smoothing Techniques}, 
  \emph{With Implementation in S}.
  Springer-Verlag, New York. 
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{plot.h.bcv}}, see \code{\link[stats]{bw.bcv}} in package \CRANpkg{stats} and 
\code{\link[MASS]{bcv}} in package \CRANpkg{MASS} for Gaussian kernel only if \code{deriv.order = 0},
\code{\link[ks]{Hbcv}} for bivariate data in package \CRANpkg{ks} for Gaussian kernel 
only if \code{deriv.order = 0}, \code{\link[locfit]{kdeb}} in package \CRANpkg{locfit} 
if \code{deriv.order = 0}.
}
\examples{
## EXAMPLE 1:

x <- rnorm(100)
h.bcv(x,whichbcv = 1, deriv.order = 0)
h.bcv(x,whichbcv = 2, deriv.order = 0)

## EXAMPLE 2:

## Derivative order = 0

h.bcv(kurtotic,deriv.order = 0)

## Derivative order = 1

h.bcv(kurtotic,deriv.order = 1)
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
