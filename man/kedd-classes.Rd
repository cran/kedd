\name{kedd-classes}
\docType{class}
\alias{kedd-classes}
\alias{dkde-class}
\alias{kfun-class}
\alias{kconv-class}
\alias{show,dkde-method}
\alias{plot,dkde-method}
\alias{show,kfun-method}
\alias{plot,kfun-method}
\alias{show,kconv-method}
\alias{plot,kconv-method}
\alias{$,dkde-method}
\alias{$,kfun-method}
\alias{$,kconv-method}
\title{Classes \code{"dkde"}, \code{"kfun"} and \code{"kconv"}}
\description{
  S4 classes returned by \code{\link{dkde}} (kernel density derivative
  estimate), \code{\link{kernel.fun}} (kernel derivative \eqn{K^{(r)}}) and
  \code{\link{kernel.conv}} (convolution \eqn{K^{(r)} * K^{(r)}}).
}
\section{Slots}{
  Common to the three classes:
  \describe{
    \item{\code{x}:}{\code{"numeric"}, evaluation points.}
    \item{\code{y}:}{\code{"numeric"}, values at \code{x}.}
    \item{\code{kernel}:}{\code{"character"}, kernel name.}
    \item{\code{deriv.order}:}{\code{"integer"}, derivative order \eqn{r}.}
    \item{\code{call}:}{\code{"call"}, the matched call.}
  }
  Class \code{"dkde"} has in addition:
  \describe{
    \item{\code{h}:}{\code{"numeric"}, bandwidth used.}
    \item{\code{n}:}{\code{"integer"}, sample size.}
    \item{\code{data}:}{\code{"numeric"}, the (cleaned) observations.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{show(object)}}{prints a short summary.}
    \item{\code{plot(x, type = "l", zero.line, main.title, x.title, y.title,
      colors, title.size, title.face, axis.size, theme, ...)}}{draws the
      curve with \pkg{ggplot2}, prints it and returns the ggplot object
      invisibly.  \code{type} is \code{"l"}, \code{"p"} or \code{"o"};
      \code{zero.line} (default \code{TRUE} when \eqn{r > 0}) adds a dashed
      horizontal line at 0.  For \code{"dkde"} objects, \code{rug} and
      \code{band} (default \code{TRUE}) add the observations and a
      \eqn{\pm}{+/-} one standard-error band.}
    \item{\code{x$name}}{Compatibility with \pkg{kedd} 1.x, where these
      functions returned lists: \code{$kx} is the slot \code{y} of
      \code{"kfun"} and \code{"kconv"} objects, and \code{$eval.points},
      \code{$est.fx} are the slots \code{x}, \code{y} of \code{"dkde"}
      objects; the slot names can also be used.  Unknown names give
      \code{NULL}.}
  }
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz},
  Amel Henider \email{amel.henider@univ-tam.dz} and
  Iago Giné-Vázquez \email{iago.gin-vaz@protonmail.com}}
\seealso{
  \code{\link{dkde}}, \code{\link{kernel.fun}}, \code{\link{kernel.conv}},
  \code{\linkS4class{bandwidth}}.
}
\examples{
kf <- kernel.fun(kernel = "logistic", deriv.order = 1L)
kf
p <- plot(kf, type = "o")

kc <- kernel.conv(kernel = "biweight")
plot(kc)

d <- dkde(data = rnorm(100), h = 0.4)
plot(d, band = FALSE)
}
\keyword{classes}
