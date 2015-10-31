\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{plot.dkde}
\alias{plot.dkde}
\alias{lines.dkde}
\title{
Plot for Kernel Density Derivative Estimate
}
\description{
The \code{\link{plot.dkde}} function loops through calls to 
the \code{\link{dkde}} function. Plot for kernel density 
derivative estimate for 1-dimensional data.
}
\usage{
\method{plot}{dkde}(x, fx = NULL, \dots)
\method{lines}{dkde}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{dkde} (output from \code{\link{dkde}}).}
  \item{fx}{add to graphics the true density derivative (class :\code{\link{function}}), 
  to compare it by the density derivative to estimate.}
  \item{\dots}{other graphics parameters, see \code{\link[graphics]{par}} in package "graphics".}
}
\details{
  The 1-d plot is a standard plot of a 1-d curve. If
  \code{!is.null(fx)} then a true density derivative is added. 
}
\value{
  Plot of 1-d kernel density derivative estimates are sent to graphics window.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{dkde}}, \code{\link[stats]{plot.density}} in package "stats" if \code{deriv.order = 0}.
}
\examples{
plot(dkde(kurtotic,deriv.order=0,kernel="gaussian"),sub="")
lines(dkde(kurtotic,deriv.order=0,kernel="biweight"),col="red")
}
\keyword{plot}
