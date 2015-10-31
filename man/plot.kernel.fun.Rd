\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{plot.kernel.fun}
\alias{plot.kernel.fun}
\title{
Plot of r'th Derivative Kernel Function
}
\description{
The \code{\link{plot.kernel.fun}} function loops through calls to 
the \code{\link{kernel.fun}} function. Plot for r'th derivative 
kernel function one-dimensional.
}
\usage{
\method{plot}{kernel.fun}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{kernel.fun} (output from \code{\link{kernel.fun}}).}
  \item{\dots}{other graphics parameters, see \code{\link[graphics]{par}} in package "graphics".}
}
\value{
  Plot of 1-d for r'th derivative kernel function are sent to graphics window.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{kernel.fun}}.
}
\examples{
## Gaussian kernel

dev.new()
par(mfrow=c(2,2))
plot(kernel.fun(kernel="gaussian",deriv.order=0))
plot(kernel.fun(kernel="gaussian",deriv.order=1))
plot(kernel.fun(kernel="gaussian",deriv.order=2))
plot(kernel.fun(kernel="gaussian",deriv.order=3))

## Silverman kernel

dev.new()
par(mfrow=c(2,2))
plot(kernel.fun(kernel="silverman",deriv.order=0))
plot(kernel.fun(kernel="silverman",deriv.order=1))
plot(kernel.fun(kernel="silverman",deriv.order=2))
plot(kernel.fun(kernel="silverman",deriv.order=3))
}
\keyword{plot}
