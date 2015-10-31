\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{kernel.fun}
\alias{kernel.fun}
\alias{kernel.fun.default}
\title{
Derivatives of Kernel Function
}
\description{
  The (S3) generic function \code{kernel.fun} computes the 
  r'th derivative for kernel density.}
\usage{
kernel.fun(x, \dots)
\method{kernel.fun}{default}(x = NULL, deriv.order = 0, kernel = c("gaussian","epanechnikov", 
            "uniform", "triangular", "triweight", "tricube", 
            "biweight", "cosine", "silverman"), \dots)
}
\arguments{
  \item{x}{points at which the derivative of kernel function 
           is to be evaluated.}
  \item{deriv.order}{derivative order (scalar).}
  \item{kernel}{a character string giving the smoothing kernel to be used, 
  with default \code{"gaussian"}.}
  \item{\dots}{further arguments for (non-default) methods.}
}
\details{
  We give a short survey of some kernels functions \eqn{K(x;r)}{K(x;r)}; where \eqn{r} is derivative order,
  \itemize{
    \item Gaussian:  \eqn{K(x;\infty) =\frac{1}{\sqrt{2\pi}}\exp\left(-\frac{x^{2}}{2}\right)1_{]-\infty,+\infty[}}{K(x;Inf) = 1/sqrt(2pi) exp(-0.5 x^2)}
    \item Epanechnikov: \eqn{K(x;2)=\frac{3}{4}(1-x^{2})1_{(|x| \leq 1)}}{K(x;2) = 0.75 (1-x^2) (abs(x) <= 1)}
    \item uniform (rectangular): \eqn{K(x;0)=\frac{1}{2}1_{(|x| \leq 1)}}{K(x;0) = 0.5 (abs(x) <= 1)}
    \item triangular: \eqn{K(x;1)=(1-|x|)1_{(|x| \leq 1)}}{K(x;1) = (1-abs(x)) (abs(x) <= 1)}
    \item triweight: \eqn{K(x;6)=\frac{35}{32}(1-x^{2})^{3} 1_{(|x| \leq 1)}}{K(x;6) = 35/36 (1-x^2)^3 (abs(x) <= 1)}
    \item tricube: \eqn{K(x;9)=\frac{70}{81}(1-|x|^{3})^{3} 1_{(|x| \leq 1)}}{K(x;9) = 70/81 (1-abs(x)^3)^3 (abs(x) <= 1)}
    \item biweight: \eqn{K(x;4)=\frac{15}{16}(1-x^{2})^{2} 1_{(|x| \leq 1)}}{K(x;4) = 15/16 (1-x^2)^2 (abs(x) <= 1)}
    \item cosine: \eqn{K(x;\infty)=\frac{\pi}{4}\cos\left(\frac{\pi}{2}x\right) 1_{(|x| \leq 1)}}{0.25 pi cos(0.5 pi x) (abs(x) <= 1)}
    \item Silverman: \eqn{K(x;r \%\% 8)\footnote{\%\% indicates x mod y}=\frac{1}{2}\exp\left(-\frac{|x|}{\sqrt{2}}\right)\sin\left(\frac{|x|}{\sqrt{2}}+\frac{\pi}{4}\right)1_{]-\infty,+\infty[}}{K(x;r mod 8)= 0.5 exp(-abs(x)/sqrt(2)) sin(abs(x)/sqrt(2) + 0.25 pi)}	
  }
  The r'th derivative for kernel function \eqn{K(x)} is written:
  \deqn{K^{(r)}(x) = \frac{d^{r}}{d x^{r}} K(x)}{K(x;r) = d^r / dx^r K(x)}
  for \eqn{r = 0, 1, 2, \dots}\cr
  The r'th derivative of the \bold{Gaussian kernel} \eqn{K(x)} is given by:
  \deqn{K^{(r)}(x) = (-1)^{r} H_{r}(x) K(x)}{K(x;r) = (-1)^r H(x;r) K(x)}
  where \eqn{H_{r}(x)}{H(x;r)} is the r'th \bold{Hermite polynomial}. This polynomials 
  are set of orthogonal polynomials, for more details see, \code{\link[orthopolynom]{hermite.h.polynomials}} 
  in package \CRANpkg{orthopolynom}.
}
\value{
  \item{kernel }{name of kernel to use.}
  \item{deriv.order }{the derivative order to use.}
  \item{x }{the n coordinates of the points where the derivative of kernel 
            function is evaluated.}
  \item{kx }{the kernel derivative values.}
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\references{
  Jones, M. C. (1992).
  Differences and derivatives in kernel estimation. 
  \emph{Metrika}, \bold{39}, 335--340.
  
  Olver, F. W., Lozier, D. W., Boisvert, R. F. and Clark, C. W. (2010).
  \emph{NIST Handbook of Mathematical Functions}.
  Cambridge University Press, New York, USA.
  
  Silverman, B. W. (1986).
  \emph{Density Estimation for Statistics and Data Analysis}.
  Chapman & Hall/CRC. London.
}
\seealso{
\code{\link{plot.kernel.fun}}, \code{\link[stats]{deriv}} and \code{\link[stats]{D}} in 
package "stats" for symbolic and algorithmic derivatives of simple expressions. 
}
\examples{
kernels <- eval(formals(kernel.fun.default)$kernel)
kernels

## gaussian
kernel.fun(x = 0,kernel=kernels[1],deriv.order=0)
kernel.fun(x = 0,kernel=kernels[1],deriv.order=1)

## silverman
kernel.fun(x = 0,kernel=kernels[9],deriv.order=0)
kernel.fun(x = 0,kernel=kernels[9],deriv.order=1)
}
\keyword{nonparametric}
\keyword{kernel}

