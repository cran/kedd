\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}


\name{dkde}
\alias{dkde}
\alias{dkde.default}
\alias{print.dkde}
\title{
Derivatives of Kernel Density Estimator
}
\description{
  The (S3) generic function \code{dkde} computes the r'th 
  derivative of kernel density estimator for one-dimensional 
  data. Its default method does so with the given kernel 
  and bandwidth \eqn{h} for one-dimensional observations.
}
\usage{
dkde(x, \dots)
\method{dkde}{default}(x, y = NULL, deriv.order = 0, h, kernel = c("gaussian", 
         "epanechnikov", "uniform", "triangular", "triweight", 
         "tricube", "biweight", "cosine"), \dots)
}
\arguments{
  \item{x}{the data from which the estimate is to be computed.}
  \item{y}{the points of the grid at which the
    density derivative is to be estimated; the defaults are \eqn{\tau * h} outside
    of range(\eqn{x}), where \eqn{\tau = 4}.}
  \item{deriv.order}{derivative order (scalar).}
  \item{h}{the smoothing bandwidth to be used, can also be a character 
          string giving a rule to choose the bandwidth, see \code{\link{h.bcv}}. The default \code{\link{h.ucv}}.}
  \item{kernel}{a character string giving the smoothing kernel to be used, with default
    \code{"gaussian"}.}
  \item{\dots}{further arguments for (non-default) methods.}
}
\details{
  A simple estimator for the density derivative can be obtained by taking the derivative
  of the kernel density estimate. If the kernel \eqn{K(x)} is differentiable \eqn{r} times 
  then the r'th density derivative estimate can be written as:
  
  \deqn{\hat{f}^{(r)}_{h}(x)=\frac{1}{nh^{r+1}}\sum_{i=1}^{n} K^{(r)}\left(\frac{x-X_{i}}{h}\right)}{hat(f)(x;r) = n^-1 h^-(r+1) sum(K(x-X(i)/h),i=1...n)}
  where, \deqn{K^{(r)}(x) = \frac{d^{r}}{d x^{r}} K(x)}{K(x;r) = d^r /d x^r K(x)}
  for \eqn{r = 0, 1, 2, \dots}
  
  The following assumptions on the density \eqn{f^{(r)}(x)}{f(x;r)}, the bandwidth \eqn{h}, and the kernel \eqn{K(x)}:
  \enumerate{
  \item{}{The \eqn{(r+2)} derivative \eqn{f^{(r+2)}(x)}{f(x;r+2)} is continuous, square integrable and ultimately monotone.}
  \item{}{\eqn{\lim_{n \to \infty} h = 0}{lim_(n -- > Inf) h = 0} and \eqn{\lim_{n \to \infty}n h^{2r+1} = \infty}{lim_(n --> Inf) nh^(2r+1) = Inf} i.e., as the number of samples \eqn{n} is increased \eqn{h} approaches zero at a rate slower than \eqn{1/n^{2r+1}}.}
  \item{}{\eqn{K(x) \geq 0}{K(x) >= 0} and \eqn{\int_{R} K(x) dx = 1}{int K(x) dx = 1}. The kernel function is assumed to be symmetric about the origin i.e., \eqn{\int_{R} xK^{(r)}(x) dx = 0}{int x k(x;r) dx = 0} for even \eqn{r} and has finite second moment i.e., \eqn{\mu_{2}(K)=\int_{R}x^{2} K(x) dx < \infty}{mu(K(x)) = int x^2 K(x) dx < Inf}.}
  }
  
  Some theoretical properties of the estimator \eqn{\hat{f}^{(r)}_{h}}{hat(f)(x;r)} have been investigated, among others, by Bhattacharya (1967), Schuster (1969). Let us now turn to the statistical properties of estimator. We are interested in the mean squared error since it combines squared bias and variance.  
  
  The \bold{bias} can be written as:
  
  \deqn{E\left[\hat{f}^{(r)}_{h}(x)\right]- f^{(r)}(x) = \frac{1}{2}h^{2}\mu_{2}(K) f^{(r+2)}(x)+o(h^{2})}{E[hat(f)(x;r)] - f(x;r) = 0.5 h^2 mu(K(x)) f(x;r+2) + o(h^2)}
  
  The \bold{variance} of the estimator can be written as:
  
  \deqn{VAR\left[\hat{f}^{(r)}_{h}(x)\right]=\frac{f(x) R\left(K^{(r)}\right)}{nh^{2r+1}} + o(1/nh^{2r+1})}{VAR(hat(f)(x;r)) = f(x) R(K(x;r)) / n h^(2r+1) + o(1/nh^(2r+1)) }
  with, \eqn{R\left(K^{(r)}\right) = \int_{R} \left(K^{(r)}(x)\right)^{2}dx.}{R(K(x;r)) = int K(x;r)^2 dx.}

  The \bold{MSE} (Mean Squared Error) for kernel density derivative estimators can be written as:
  \deqn{MSE\left(\hat{f}^{(r)}_{h}(x),f^{(r)}(x)\right)=\frac{f(x)R\left(K^{(r)}\right)}{nh^{2r+1}}+\frac{1}{4}h^{4}\mu_{2}^{2}(K) f^{(r+1)}(x)^{2}+o(h^{4}+1/nh^{2r+1})}{MSE(hat(f)(x;r),f(x;r)) = f(x) R(K(x;r)) / nh^(2r+1) + 1/4 h^4 mu(K(x))^2 f(x;r+1)^2 + o(h^4 + 1/ nh^(2r+1))}
  
  It follows that the MSE-optimal bandwidth for estimating \eqn{\hat{f}^{(r)}_{h}S(x)}{hat(f)(x;r)}, is of order \eqn{n^{-1/(2r+5)}}{n^(-1/2r+5)}. Therefore, 
  the estimation of \eqn{\hat{f}^{(1)}_{h}(x)}{hat(f)(x;1)} requires a bandwidth of order \eqn{n^{-1/7}}{n^-1/7} compared to the optimal \eqn{n^{-1/5}}{n^-1/5} 
  for estimating \eqn{f(x)}{f(x)} itself. It reveals the increasing difficulty in problems of estimating higher derivatives.\cr
  
  The \bold{MISE} (Mean Integrated Squared Error) can be written as:
  
  \deqn{MISE\left(\hat{f}^{(r)}_{h}(x),f^{(r)}(x)\right)=AMISE\left(\hat{f}^{(r)}_{h}(x),f^{(r)}(x)\right)+o(h^{4}+1/nh^{2r+1})}{MISE(hat(f)(x;r),f(x;r))=AMISE(hat(f)(x;r),f(x;r)) + o(h^4 + 1/nh^(2r+1))}
  where,
  \deqn{AMISE\left(\hat{f}^{(r)}_{h}(x),f^{(r)}(x)\right)=\frac{1}{nh^{2r+1}}R\left(K^{(r)}\right)+\frac{1}{4}h^{4}\mu_{2}^{2}(K)R\left(f^{(r+2)}\right)}{AMISE(hat(f)(x;r),f(x;r)) = R(K(x;r))/n h^(2r+1) + 1/4 h^2 mu(K(x))^2 R(f(x;r+2))}
  with: \eqn{R\left(f^{(r)}(x)\right) = \int_{R} \left(f^{(r)}(x)\right)^{2}dx.}{R(f(x;r)) = int f(x;r)^2 dx.}\cr
  The performance of kernel is measured by \bold{MISE} or \bold{AMISE} (Asymptotic MISE).\cr  

  If the bandwidth \code{h} is missing from \code{dkde}, then the default bandwidth is 
  \code{h.ucv(x,deriv.order,kernel)} (Unbiased cross-validation, see \code{\link{h.ucv}}).\cr
  For more details see references.
}
\value{
  \item{x}{data points - same as input.}
  \item{data.name}{the deparsed name of the \code{x} argument.}
  \item{n}{the sample size after elimination of missing values.}
  \item{kernel}{name of kernel to use.}
  \item{deriv.order}{the derivative order to use.}
  \item{h}{the bandwidth value to use.}
  \item{eval.points}{the coordinates of the points where the 
  density derivative is estimated.}
  \item{est.fx}{the estimated density derivative values.}
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\references{
  Alekseev, V. G. (1972).
  Estimation of a probability density function and its derivatives.
  \emph{Mathematical notes of the Academy of Sciences of the USSR}. \bold{12} (5), 808--811.
  
  Alexandre, B. T. (2009).
  \emph{Introduction to Nonparametric Estimation}.
  Springer-Verlag, New York.
  
  Bowman, A. W. and Azzalini, A. (1997). 
  \emph{Applied Smoothing Techniques for
  Data Analysis: the Kernel Approach with 
  S-Plus Illustrations}.
  Oxford University Press, Oxford.
  
  Bhattacharya, P. K. (1967).
  Estimation of a probability density function and Its derivatives.
  \emph{Sankhya: The Indian Journal of Statistics, Series A}, \bold{29}, 373--382.  
  
  Jeffrey, S. S. (1996).
  \emph{Smoothing Methods in Statistics}.
  Springer-Verlag, New York.
  
  Radhey, S. S. (1987).
  MISE of kernel estimates of a density and its derivatives.
  \emph{Statistics and Probability Letters}, \bold{5}, 153--159.
  
  Scott, D. W. (1992).
  \emph{Multivariate Density Estimation. Theory, Practice and Visualization}.
  New York: Wiley.
  
  Schuster, E. F. (1969) 
  Estimation of a probability density function and its derivatives. 
  \emph{The Annals of Mathematical Statistics}, \bold{40} (4), 1187--1195.
  
  Silverman, B. W. (1986).
  \emph{Density Estimation for Statistics and Data Analysis}.
  Chapman & Hall/CRC. London.
  
  Stoker, T. M. (1993).
  Smoothing bias in density derivative estimation. 
  \emph{Journal of the American Statistical Association}, \bold{88}, 855--863.
  
  Venables, W. N. and Ripley, B. D. (2002).
  \emph{Modern Applied Statistics with S}.
  New York: Springer.
  
  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing}.
  Chapman and Hall, London.
  
  Wolfgang, H. (1991).
  \emph{Smoothing Techniques}, 
  \emph{With Implementation in S}.
  Springer-Verlag, New York.
}
\note{This function are available in other packages such as \CRANpkg{KernSmooth}, \CRANpkg{sm}, 
\CRANpkg{np}, \CRANpkg{GenKern} and \CRANpkg{locfit} if \code{deriv.order=0}, and in \CRANpkg{ks} package 
for Gaussian kernel only if \code{0 <= deriv.order <= 10}.
}
\seealso{
\code{\link{plot.dkde}}, see \code{\link[stats]{density}} in package \CRANpkg{stats} if \code{deriv.order = 0}, and \code{\link[ks]{kdde}} in package \CRANpkg{ks}.
}
\examples{
## EXAMPLE 1:  Simple example of a Gaussian density derivative

x <- rnorm(100)
dkde(x,deriv.order=0)  ## KDE of f
dkde(x,deriv.order=1)  ## KDDE of d/dx f
dkde(x,deriv.order=2)  ## KDDE of d^2/x^2 f
dkde(x,deriv.order=3)  ## KDDE of d^3/x^3 f
dev.new()
par(mfrow=c(2,2))
plot(dkde(x,deriv.order=0))
plot(dkde(x,deriv.order=1))
plot(dkde(x,deriv.order=2))
plot(dkde(x,deriv.order=3))

## EXAMPLE 2: Bimodal Gaussian density derivative
## show the kernels in the dkde parametrization

fx  <- function(x) 0.5 * dnorm(x,-1.5,0.5) + 0.5 * dnorm(x,1.5,0.5)
fx1 <- function(x) 0.5 *(-4*x-6)* dnorm(x,-1.5,0.5) + 0.5 *(-4*x+6) * 
                   dnorm(x,1.5,0.5)
				   
## 'h = 0.3' ; 'Derivative order = 0'

kernels <- eval(formals(dkde.default)$kernel)
dev.new()
plot(dkde(bimodal,h=0.3),sub=paste("Derivative order = 0",";",
     "Bandwidth =0.3 "),ylim=c(0,0.5), main = "Bimodal Gaussian Density")
for(i in 2:length(kernels))
   lines(dkde(bimodal, h = 0.3, kernel =  kernels[i]), col = i)
curve(fx,add=TRUE,lty=8)
legend("topright", legend = c(TRUE,kernels), col = c("black",seq(kernels)),
          lty = c(8,rep(1,length(kernels))),cex=0.7, inset = .015)
	   
## 'h = 0.6' ; 'Derivative order = 1'

kernels <- eval(formals(dkde.default)$kernel)[-3]
dev.new()
plot(dkde(bimodal,deriv.order=1,h=0.6),main = "Bimodal Gaussian Density Derivative",sub=paste
         ("Derivative order = 1",";","Bandwidth =0.6"),ylim=c(-0.6,0.6))
for(i in 2:length(kernels))
   lines(dkde(bimodal,deriv.order=1, h = 0.6, kernel =  kernels[i]), col = i)
curve(fx1,add=TRUE,lty=8)
legend("topright", legend = c(TRUE,kernels), col = c("black",seq(kernels)),
          lty = c(8,rep(1,length(kernels))),cex=0.7, inset = .015)
}
\keyword{smooth}
\keyword{nonparametric}
\keyword{density derivative}
