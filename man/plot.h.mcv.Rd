\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{plot.h.mcv}
\alias{plot.h.mcv}
\alias{lines.h.mcv}
\title{
Plot for Modified Cross-Validation
}
\description{
The \code{\link{plot.h.mcv}} function loops through calls to 
the \code{\link{h.mcv}} function. Plot for modified cross-validation 
function for 1-dimensional data.
}
\usage{
\method{plot}{h.mcv}(x, seq.bws=NULL, \dots)
\method{lines}{h.mcv}(x,seq.bws=NULL, \dots)
}
\arguments{
  \item{x}{object of class \code{h.mcv} (output from \code{\link{h.mcv}}).}
  \item{seq.bws}{the sequence of bandwidths in which to compute the modified 
  cross-validation function. By default, the procedure defines a sequence of 
  50 points, from \code{0.15*hos} to \code{2*hos} (Over-smoothing).}
  \item{\dots}{other graphics parameters, see \code{\link[graphics]{par}} in package "graphics".}
}
\value{
  Plot of 1-d modified cross-validation function are sent to graphics window.\cr
  \item{kernel}{name of kernel to use.}
  \item{deriv.order}{the derivative order to use.}
  \item{seq.bws}{the sequence of bandwidths.}
  \item{mcv}{the values of the modified cross-validation function in the bandwidths grid.}
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{h.mcv}}.
}
\examples{
par(mfrow=c(2,1))
plot(h.mcv(trimodal,deriv.order=0),main="")
plot(h.mcv(trimodal,deriv.order=1),main="")
}
\keyword{plot}
