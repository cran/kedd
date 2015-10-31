\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{plot.h.mlcv}
\alias{plot.h.mlcv}
\alias{lines.h.mlcv}
\title{
Plot for Maximum-Likelihood Cross-validation
}
\description{
The \code{\link{plot.h.mlcv}} function loops through calls to 
the \code{\link{h.mlcv}} function. Plot for maximum-likelihood 
cross-validation function for 1-dimensional data.
}
\usage{
\method{plot}{h.mlcv}(x, seq.bws=NULL, \dots)
\method{lines}{h.mlcv}(x,seq.bws=NULL, \dots)
}
\arguments{
  \item{x}{object of class \code{h.mlcv} (output from \code{\link{h.mlcv}}).}
  \item{seq.bws}{the sequence of bandwidths in which to compute the maximum-
  likelihood cross-validation function. By default, the procedure defines a 
  sequence of 50 points, from \code{0.15*hos} to \code{2*hos} (Over-smoothing).}
  \item{\dots}{other graphics parameters, see \code{\link[graphics]{par}} in package "graphics".}
}
\value{
  Plot of 1-d maximum-likelihood cross-validation function are sent to graphics window.\cr
  \item{kernel}{name of kernel to use.}
  \item{seq.bws}{the sequence of bandwidths.}
  \item{mlcv}{the values of the maximum-likelihood cross-validation function in the bandwidths grid.}
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{h.mlcv}}.
}
\examples{
plot(h.mlcv(bimodal))
}
\keyword{plot}
