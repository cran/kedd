\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{plot.h.amise}
\alias{plot.h.amise}
\alias{lines.h.amise}
\title{
Plot for Asymptotic Mean Integrated Squared Error
}
\description{
The \code{\link{plot.h.amise}} function loops through calls to 
the \code{\link{h.amise}} function. Plot for asymptotic mean integrated 
squared error function for 1-dimensional data.
}
\usage{
\method{plot}{h.amise}(x, seq.bws=NULL, \dots)
\method{lines}{h.amise}(x,seq.bws=NULL, \dots)
}
\arguments{
  \item{x}{object of class \code{h.amise} (output from \code{\link{h.amise}}).}
  \item{seq.bws}{the sequence of bandwidths in which to compute the AMISE function. 
  By default, the procedure defines a sequence of 50 points, from \code{0.15*hos} 
  to \code{2*hos} (Over-smoothing).}
  \item{\dots}{other graphics parameters, see \code{\link[graphics]{par}} in package "graphics".}
}
\value{
  Plot of 1-d AMISE function are sent to graphics window.\cr
  \item{kernel}{name of kernel to use.}
  \item{deriv.order}{the derivative order to use.}
  \item{seq.bws}{the sequence of bandwidths.}
  \item{amise}{the values of the AMISE function in the bandwidths grid.}
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{h.amise}}.
}
\examples{
plot(h.amise(bimodal,deriv.order=0))
}
\keyword{plot}
