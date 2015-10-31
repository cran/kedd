\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{kernel.conv}
\alias{kernel.conv}
\alias{kernel.conv.default}
\title{
Convolutions of r'th Derivative for Kernel Function
}
\description{
  The (S3) generic function \code{kernel.conv} computes the convolution 
  of r'th derivative for kernel function. 
}
\usage{
kernel.conv(x, \dots)
\method{kernel.conv}{default}(x = NULL, deriv.order = 0,kernel = c("gaussian","epanechnikov", 
             "uniform", "triangular", "triweight", "tricube", 
             "biweight", "cosine", "silverman"), \dots)
}
\arguments{
  \item{x}{points at which the convolution of kernel derivative 
           is to be evaluated.}
  \item{deriv.order}{derivative order (scalar).}
  \item{kernel}{a character string giving the smoothing kernel to be used, 
  with default \code{"gaussian"}.}
  \item{\dots}{further arguments for (non-default) methods.}
}
\details{
  The convolution of r'th derivative for kernel function is written \eqn{K^{(r)}\ast K^{(r)}}{K(x;r)*K(x;r)}. It is defined as 
  the integral of the product of the derivative for kernel. As such, it is a particular kind of integral transform:
 
  \deqn{K^{(r)} \ast K^{(r)}(x) = \int_{-\infty}^{+\infty} K^{(r)}(y)K^{(r)}(x-y)dy}{K(x;r)*k(x;r) = int K(y;r) K(x-y;r) dy}
 
  where: \deqn{K^{(r)}(x) = \frac{d^{r}}{d x^{r}} K(x)}{K(x;r) = d^r / dx^r K(x)}
  for \eqn{r = 0, 1, 2, \dots}
}
\value{
  \item{kernel }{name of kernel to use.}
  \item{deriv.order }{the derivative order to use.}
  \item{x }{the n coordinates of the points where the convolution of kernel 
            derivative is evaluated.}
  \item{kx }{the convolution of kernel derivative values.}
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\references{
  Olver, F. W., Lozier, D. W., Boisvert, R. F. and Clark, C. W. (2010).
  \emph{NIST Handbook of Mathematical Functions}.
  Cambridge University Press, New York, USA.
  
  Scott, D. W. (1992).
  \emph{Multivariate Density Estimation. Theory, Practice and Visualization}.
  New York: Wiley.
  
  Silverman, B. W. (1986).
  \emph{Density Estimation for Statistics and Data Analysis}.
  Chapman & Hall/CRC. London.
  
  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing.}
  Chapman and Hall, London.

  Wolfgang, H. (1991).
  \emph{Smoothing Techniques, With Implementation in S.}
  Springer-Verlag, New York.
}
\seealso{
\code{\link{plot.kernel.conv}}, \code{\link[stats]{kernapply}} in package "stats"
for computes the convolution between an input sequence, and \code{\link[stats]{convolve}}
use the Fast Fourier Transform (\code{\link[stats]{fft}}) to compute the several kinds of 
convolutions of two sequences.
}
\examples{
kernels <- eval(formals(kernel.conv.default)$kernel)
kernels

## gaussian
kernel.conv(x = 0,kernel=kernels[1],deriv.order=0)
kernel.conv(x = 0,kernel=kernels[1],deriv.order=1)

## silverman
kernel.conv(x = 0,kernel=kernels[9],deriv.order=0)
kernel.conv(x = 0,kernel=kernels[9],deriv.order=1)
}
\keyword{nonparametric}
\keyword{kernel}
