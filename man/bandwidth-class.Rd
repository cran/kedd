\name{bandwidth-class}
\docType{class}
\alias{bandwidth-class}
\alias{show,bandwidth-method}
\alias{plot,bandwidth-method}
\alias{lines,bandwidth-method}
\alias{$,bandwidth-method}
\title{Class \code{"bandwidth"} --- Result of Bandwidth Selection}
\description{
  An S4 class returned by all bandwidth selector functions in \pkg{kedd}
  (\code{\link{h.ucv}}, \code{\link{h.bcv}}, \code{\link{h.ccv}},
  \code{\link{h.mcv}}, \code{\link{h.tcv}}, \code{\link{h.scv}},
  \code{\link{h.amise}}, \code{\link{h.mlcv}}).  It stores the selected bandwidth together with
  the data, kernel, derivative order, and optimisation details.
}
\section{Objects from the Class}{
  Objects are created by calls to the bandwidth selector functions, or
  directly with \code{new("bandwidth", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"numeric"}.  The (cleaned)
      data vector.}
    \item{\code{kernel}:}{Object of class \code{"character"}.  Name of the
      kernel function used (one of \code{"gaussian"}, \code{"epanechnikov"},
      \code{"triangular"}, \code{"biweight"}, \code{"uniform"},
      \code{"triweight"}, \code{"tricube"}, \code{"cosine"},
      \code{"silverman"}, \code{"logistic"} or \code{"pow(s,t)"}).}
    \item{\code{deriv.order}:}{Object of class \code{"integer"}.  Derivative
      order \eqn{r \geq 0}{r >= 0}.}
    \item{\code{h}:}{Object of class \code{"numeric"}.  The selected
      bandwidth.}
    \item{\code{min.obj}:}{Object of class \code{"numeric"}.  The minimum
      value of the cross-validation (or other) criterion at \code{h}.
      \code{NA} for \code{h.amise}.}
    \item{\code{hos}:}{Object of class \code{"numeric"}.  The
      over-smoothing bandwidth \eqn{h_{os}}{hos}; the default search
      interval is \eqn{[0.01\,h_{os},\, 2\,h_{os}]}{[0.01 hos, 2 hos]}.}
    \item{\code{method}:}{Object of class \code{"character"}.  The
      selector used: one of \code{"UCV"}, \code{"BCV1"}, \code{"BCV2"},
      \code{"CCV"}, \code{"MCV"}, \code{"TCV"}, \code{"SCV"},
      \code{"AMISE"}, \code{"MLCV"}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{show(object)}}{Prints a compact summary: method, kernel,
      derivative order, selected bandwidth, minimum criterion.}
    \item{\code{plot(x, h = NULL, type = "o", n.points = 100L,
      log_scale_x = FALSE, show.min = TRUE, show.hos = FALSE, ...)}}{Draws
      the cross-validation curve over a grid of bandwidths with \pkg{ggplot2}
      and marks the selected \code{h}.  Returns invisibly a list with
      \code{h}, \code{cv} and the ggplot object \code{plot}.  Not available
      for \code{"AMISE"} (closed-form rule, no criterion).}
    \item{\code{lines(x, n.points = 100L, color, linewidth, linetype, ...)}}{Returns
      a \code{ggplot2::geom_line()} layer with the curve of \code{x}, to be
      added to a plot: \code{plot(bw1)$plot + lines(bw2)}.}
    \item{\code{x$name}}{Compatibility with \pkg{kedd} 1.x, where the
      selectors returned a list: \code{bw$h} is \code{bw@h}, and \code{$}
      also accepts the slot names, \code{x} (the data), \code{n} and the
      1.x names \code{min.ucv}, \code{min.bcv}, ... (the slot
      \code{min.obj}).  Unknown names give \code{NULL}.}
  }
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz}}
\seealso{
  \code{\link{h.ucv}}, \code{\link{h.bcv}}, \code{\link{h.ccv}},
  \code{\link{h.mcv}}, \code{\link{h.tcv}}, \code{\link{h.scv}},
  \code{\link{h.amise}}, \code{\link{h.mlcv}}, \code{\link{dkde}}.
}
\examples{
x <- rnorm(100)
bw <- h.ucv(x, deriv.order = 0)
show(bw)
p <- plot(bw)

## overlay a second criterion (ggplot2 layer)
p$plot + lines(h.bcv(x), color = "darkred")
}
\keyword{classes}
\keyword{smooth}
\keyword{nonparametric}
