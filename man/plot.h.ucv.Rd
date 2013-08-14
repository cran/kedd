\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{plot.h.ucv}
\alias{plot.h.ucv}
\alias{lines.h.ucv}
\title{
Plot for Unbiased Cross-Validation
}
\description{
The \code{\link{plot.h.ucv}} function loops through calls to 
the \code{\link{h.ucv}} function. Plot for unbiased cross-validation 
function for 1-dimensional data.
}
\usage{
\method{plot}{h.ucv}(x, seq.bws=NULL, \dots)
\method{lines}{h.ucv}(x,seq.bws=NULL, \dots)
}
\arguments{
  \item{x}{object of class \code{h.ucv} (output from \code{\link{h.ucv}}).}
  \item{seq.bws}{the sequence of bandwidths in which to compute the unbiased 
  cross-validation function. By default, the procedure defines a sequence of 
  50 points, from \code{0.15*hos} to \code{2*hos} (Over-smoothing).}
  \item{\dots}{other graphics parameters, see \code{\link[graphics]{par}} in package \CRANpkg{graphics}.}
}
\value{
  Plot of 1-d unbiased cross-validation function are sent to graphics window.\cr
  \item{kernel}{name of kernel to use.}
  \item{deriv.order}{the derivative order to use.}
  \item{seq.bws}{the sequence of bandwidths.}
  \item{ucv}{the values of the unbiased cross-validation function in the bandwidths grid.}
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{h.ucv}}.
}
\examples{
par(mfrow=c(2,1))
plot(h.ucv(trimodal,deriv.order=0),seq.bws=seq(0.06,0.2,length=50))
plot(h.ucv(trimodal,deriv.order=1),seq.bws=seq(0.06,0.2,length=50))
}
\keyword{plot}
