\name{kernel.fun}
\alias{kernel.fun}
\title{Evaluate Kernel Function and Its Derivatives}
\description{
  Evaluates the \eqn{r}-th derivative \eqn{K^{(r)}(x)} of a kernel function
  \eqn{K} and returns an S4 object of class \code{\linkS4class{kfun}}.
}
\usage{
kernel.fun(x = NULL, deriv.order = 0L, kernel = "gaussian", n.grid = 512L)
}
\arguments{
  \item{x}{numeric vector of evaluation points. If \code{NULL} (default), a
    uniform grid of \code{n.grid} points over \eqn{[-8, 8]} (Gaussian,
    Silverman, logistic) or \eqn{[-1.1, 1.1]} (compact-support kernels)
    is used.}
  \item{deriv.order}{non-negative integer: derivative order \eqn{r \ge 0}.
    Maximum \eqn{r} with \eqn{K^{(r)} \ne 0}: any for Gaussian, cosine and
    Silverman; logistic 20; Epanechnikov 2; uniform 0; triangular 1;
    biweight 4; triweight 6; tricube 9; \code{pow(s,t)} \eqn{\min(st, 16)}{min(st, 16)}
    (16 if \eqn{s} or \eqn{t} is not an integer). Above it, zero is returned
    (with a warning for logistic and pow). See
    \code{\link{kedd-package}}, section \sQuote{Maximum derivative order}.}
  \item{kernel}{character string giving the kernel; one of
    \code{"gaussian"} (default), \code{"epanechnikov"}, \code{"uniform"},
    \code{"triangular"}, \code{"biweight"}, \code{"triweight"},
    \code{"tricube"}, \code{"cosine"}, \code{"silverman"}, \code{"logistic"},
    or a power kernel \code{"pow(s,t)"} (see \code{\link{kernel.pow}}).}
  \item{n.grid}{number of grid points when \code{x} is \code{NULL}
    (default 512).}
}
\details{
  The supported kernels at order \eqn{r = 0}:
  \itemize{
    \item \bold{Gaussian}: \eqn{K(x)=\frac{1}{\sqrt{2\pi}}e^{-x^2/2}},
          support \eqn{(-\infty,+\infty)}.
    \item \bold{Epanechnikov}: \eqn{K(x)=\frac{3}{4}(1-x^2)\mathbf{1}_{|x|\le 1}}.
    \item \bold{Uniform}: \eqn{K(x)=\frac{1}{2}\mathbf{1}_{|x|\le 1}}.
    \item \bold{Triangular}: \eqn{K(x)=(1-|x|)\mathbf{1}_{|x|\le 1}}.
    \item \bold{Biweight}: \eqn{K(x)=\frac{15}{16}(1-x^2)^2\mathbf{1}_{|x|\le 1}}.
    \item \bold{Triweight}: \eqn{K(x)=\frac{35}{32}(1-x^2)^3\mathbf{1}_{|x|\le 1}}.
    \item \bold{Tricube}: \eqn{K(x)=\frac{70}{81}(1-|x|^3)^3\mathbf{1}_{|x|\le 1}}.
    \item \bold{Cosine}: \eqn{K(x)=\frac{\pi}{4}\cos\!\left(\frac{\pi x}{2}\right)\mathbf{1}_{|x|\le 1}}.
    \item \bold{Silverman}: \eqn{K(x)=\frac{1}{2}e^{-|x|/\sqrt{2}}\sin\!\left(\frac{|x|}{\sqrt{2}}+\frac{\pi}{4}\right)},
          support \eqn{(-\infty,+\infty)}.
    \item \bold{Logistic}: \eqn{K(x)=\frac{e^{-x}}{(1+e^{-x})^2}=\frac{1}{4}\mathrm{sech}^2(x/2)},
          support \eqn{(-\infty,+\infty)}.
    \item \bold{Pow(s,t)}: \eqn{K(x)=c_{s,t}(1-|x|^s)^t\mathbf{1}_{|x|\le 1}},
          \eqn{c_{s,t}=s/(2B(1/s,t+1))}, given as \code{"pow(s,t)"}
          (see \code{\link{kernel.pow}}).
  }
  Derivatives are computed analytically via Hermite polynomials (Gaussian,
  Silverman), Guidoum polynomials (finite-support kernels), the identity
  \eqn{K^{(r)}=\sigma(1-\sigma)Q_{r+1}(\sigma)} with \eqn{\sigma(x)=1/(1+e^{-x})}
  (logistic, \eqn{r \le 20}) and exact power expansions (Pow, \eqn{r \le 16}).
}
\value{
  An S4 object of class \code{\linkS4class{kfun}} with slots:
  \describe{
    \item{\code{x}}{numeric evaluation grid.}
    \item{\code{y}}{\eqn{K^{(r)}(x)} values.}
    \item{\code{kernel}}{kernel name.}
    \item{\code{deriv.order}}{derivative order \eqn{r}.}
    \item{\code{call}}{the matched call.}
  }
  Use \code{show()} for a text summary and \code{plot()} for a ggplot2 graphic.
}
\references{
  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing}. Chapman and Hall, London.

  Silverman, B. W. (1986).
  \emph{Density Estimation for Statistics and Data Analysis}.
  Chapman & Hall/CRC, London.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz}}
\seealso{
  \code{\link{kernel.conv}}, \code{\linkS4class{kfun}},
  \code{\link{dkde}}, \code{\link{h.ucv}}.
}
\examples{
## Gaussian kernel (r = 0)
kf <- kernel.fun(kernel = "gaussian", deriv.order = 0L)
show(kf)
plot(kf)

## First derivative of Epanechnikov kernel
kf2 <- kernel.fun(kernel = "epanechnikov", deriv.order = 1L)
plot(kf2)

## Evaluate at specific points
kf3 <- kernel.fun(x = seq(-2, 2, by = 0.1),
                  kernel = "biweight", deriv.order = 2L)

## Logistic and Pow(s,t) kernels
plot(kernel.fun(kernel = "logistic", deriv.order = 2L))
plot(kernel.fun(kernel = kernel.pow(1.5, 2.5)))
}
\keyword{nonparametric}
\keyword{kernel}
