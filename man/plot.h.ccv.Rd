\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{plot.h.ccv}
\alias{plot.h.ccv}
\alias{lines.h.ccv}
\title{
Plot for Complete Cross-Validation
}
\description{
The \code{\link{plot.h.ccv}} function loops through calls to 
the \code{\link{h.ccv}} function. Plot for complete cross-validation 
function for 1-dimensional data.
}
\usage{
\method{plot}{h.ccv}(x, seq.bws=NULL, \dots)
\method{lines}{h.ccv}(x,seq.bws=NULL, \dots)
}
\arguments{
  \item{x}{object of class \code{h.ccv} (output from \code{\link{h.ccv}}).}
  \item{seq.bws}{the sequence of bandwidths in which to compute the complete 
  cross-validation function. By default, the procedure defines a sequence of 
  50 points, from \code{0.15*hos} to \code{2*hos} (Over-smoothing).}
  \item{\dots}{other graphics parameters, see \code{\link[graphics]{par}} in package \CRANpkg{graphics}.}
}
\value{
  Plot of 1-d complete cross-validation function are sent to graphics window.\cr
  \item{kernel}{name of kernel to use.}
  \item{deriv.order}{the derivative order to use.}
  \item{seq.bws}{the sequence of bandwidths.}
  \item{ccv}{the values of the complete cross-validation function in the bandwidths grid.}
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{h.ccv}}.
}
\examples{
par(mfrow=c(2,1))
plot(h.ccv(trimodal,deriv.order=0),main="")
plot(h.ccv(trimodal,deriv.order=1),main="")
}
\keyword{plot}
