\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{plot.h.tcv}
\alias{plot.h.tcv}
\alias{lines.h.tcv}
\title{
Plot for Trimmed Cross-Validation
}
\description{
The \code{\link{plot.h.tcv}} function loops through calls to 
the \code{\link{h.tcv}} function. Plot for trimmed cross-validation 
function for 1-dimensional data.
}
\usage{
\method{plot}{h.tcv}(x, seq.bws=NULL, \dots)
\method{lines}{h.tcv}(x,seq.bws=NULL, \dots)
}
\arguments{
  \item{x}{object of class \code{h.tcv} (output from \code{\link{h.tcv}}).}
  \item{seq.bws}{the sequence of bandwidths in which to compute the trimmed 
  cross-validation function. By default, the procedure defines a sequence of 
  50 points, from \code{0.15*hos} to \code{2*hos} (Over-smoothing).}
  \item{\dots}{other graphics parameters, see \code{\link[graphics]{par}} in package "graphics".}
}
\value{
  Plot of 1-d trimmed cross-validation function are sent to graphics window.\cr
  \item{kernel}{name of kernel to use.}
  \item{deriv.order}{the derivative order to use.}
  \item{seq.bws}{the sequence of bandwidths.}
  \item{tcv}{the values of the trimmed cross-validation function in the bandwidths grid.}
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{h.tcv}}.
}
\examples{
par(mfrow=c(2,1))
plot(h.tcv(trimodal,deriv.order=0),main="")
plot(h.tcv(trimodal,deriv.order=1),seq.bws=seq(0.1,0.5,length.out=50),main="")
}
\keyword{plot}
