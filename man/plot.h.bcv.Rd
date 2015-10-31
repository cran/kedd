\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{plot.h.bcv}
\alias{plot.h.bcv}
\alias{lines.h.bcv}
\title{
Plot for Biased Cross-Validation
}
\description{
The \code{\link{plot.h.bcv}} function loops through calls to 
the \code{\link{h.bcv}} function. Plot for biased cross-validation 
function for 1-dimensional data.
}
\usage{
\method{plot}{h.bcv}(x, seq.bws=NULL, \dots)
\method{lines}{h.bcv}(x,seq.bws=NULL, \dots)
}
\arguments{
  \item{x}{object of class \code{h.bcv} (output from \code{\link{h.bcv}}).}
  \item{seq.bws}{the sequence of bandwidths in which to compute the biased 
  cross-validation function. By default, the procedure defines a sequence of 
  50 points, from \code{0.15*hos} to \code{2*hos} (Over-smoothing).}
  \item{\dots}{other graphics parameters, see \code{\link[graphics]{par}} in package "graphics".}
}
\value{
  Plot of 1-d biased cross-validation function are sent to graphics window.\cr
  \item{kernel}{name of kernel to use.}
  \item{deriv.order}{the derivative order to use.}
  \item{seq.bws}{the sequence of bandwidths.}
  \item{bcv}{the values of the biased cross-validation function in the bandwidths grid.}
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{h.bcv}}.
}
\examples{
## EXAMPLE 1:

plot(h.bcv(trimodal, whichbcv = 1, deriv.order = 0),main="",sub="")
lines(h.bcv(trimodal, whichbcv = 2, deriv.order = 0),col="red")
legend("topright", c("BCV1","BCV2"),lty=1,col=c("black","red"),inset = .015)

## EXAMPLE 2:

plot(h.bcv(trimodal, whichbcv = 1, deriv.order = 1),main="",sub="")
lines(h.bcv(trimodal, whichbcv = 2, deriv.order = 1),col="red")
legend("topright", c("BCV1","BCV2"),lty=1,col=c("black","red"),inset = .015)
}
\keyword{plot}
