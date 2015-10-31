\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{plot.kernel.conv}
\alias{plot.kernel.conv}
\title{
Plot for Convolutions of r'th Derivative Kernel Function
}
\description{
The \code{\link{plot.kernel.conv}} function loops through calls to 
the \code{\link{kernel.conv}} function. Plot for convolutions of 
r'th derivative kernel function one-dimensional.
}
\usage{
\method{plot}{kernel.conv}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{kernel.conv} (output from \code{\link{kernel.conv}}).}
  \item{\dots}{other graphics parameters, see \code{\link[graphics]{par}} in package "graphics".}
}
\value{
  Plot of 1-d for convolution of r'th derivative kernel function are sent to graphics window.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{kernel.conv}}.
}
\examples{
## Gaussian kernel

dev.new()
par(mfrow=c(2,2))
plot(kernel.conv(kernel="gaussian",deriv.order=0))
plot(kernel.conv(kernel="gaussian",deriv.order=1))
plot(kernel.conv(kernel="gaussian",deriv.order=2))
plot(kernel.conv(kernel="gaussian",deriv.order=3))

## Silverman kernel

dev.new()
par(mfrow=c(2,2))
plot(kernel.conv(kernel="silverman",deriv.order=0))
plot(kernel.conv(kernel="silverman",deriv.order=1))
plot(kernel.conv(kernel="silverman",deriv.order=2))
plot(kernel.conv(kernel="silverman",deriv.order=3))
}
\keyword{plot}
