\name{dkde}
\alias{dkde}
\title{Kernel Density Derivative Estimator}
\description{
  Computes the r'th derivative of a kernel density estimator at a grid
  of evaluation points.  For \code{deriv.order = 0} this reduces to an
  ordinary kernel density estimate.
}
\usage{
dkde(x = NULL, data, h, deriv.order = 0L,
     kernel = "gaussian", nthreads = 0L)
}
\arguments{
  \item{x}{numeric vector of evaluation points.  If \code{NULL}
    (default), a regular grid of 512 points spanning
    \eqn{[\min(data) - 3h,\; \max(data) + 3h]}{[min(data)-3h, max(data)+3h]}
    is used.}
  \item{data}{numeric vector of observations.}
  \item{h}{positive bandwidth scalar.  If missing, the UCV bandwidth
    \code{\link{h.ucv}(data, deriv.order, kernel)} is used.}
  \item{deriv.order}{non-negative integer: derivative order \eqn{r}. Maximum
    \eqn{r} with \eqn{K^{(r)} \ne 0}: any for Gaussian, cosine and Silverman;
    logistic 20; Epanechnikov 2; uniform 0; triangular 1; biweight 4;
    triweight 6; tricube 9; \code{pow(s,t)} \eqn{\min(st, 16)}{min(st, 16)}.
    When \code{h} is missing it is chosen by \code{\link{h.ucv}}, whose limits
    are lower (see \code{\link{kedd-package}}, section
    \sQuote{Maximum derivative order}).}
  \item{kernel}{character string giving the kernel; one of
    \code{"gaussian"} (default), \code{"epanechnikov"}, \code{"uniform"},
    \code{"triangular"}, \code{"biweight"}, \code{"triweight"},
    \code{"tricube"}, \code{"cosine"}, \code{"silverman"}, \code{"logistic"},
    or a power kernel \code{"pow(s,t)"} (see \code{\link{kernel.pow}}).}
  \item{nthreads}{number of OpenMP threads for this call: \code{0} (default)
    = all processors, \code{k > 0} = \code{k} threads, both capped by the
    environment variable \code{OMP_THREAD_LIMIT}. The result does not depend on
    it, and it never changes the threads used by later calls.}
}
\details{
  The r'th derivative kernel density estimator is:
  \deqn{\hat{f}_h^{(r)}(x) =
    \frac{1}{n\,h^{r+1}}\sum_{i=1}^{n}K^{(r)}\!\left(\frac{x-X_i}{h}\right)}{%
    hat_f_h(r)(x) = (1/(n*h^(r+1))) * sum_i K(r)((x-Xi)/h)}
  where \eqn{K^{(r)}(x)=d^r K(x)/dx^r}{K(r)(x) = d^r K(x)/dx^r}
  (see \code{\link{kernel.fun}}).\cr

  Theoretical properties (Bhattacharya 1967; Schuster 1969):
  \itemize{
    \item \bold{Bias:}
      \eqn{E[\hat{f}_h^{(r)}(x)] - f^{(r)}(x) =
        \frac{h^2}{2}\mu_2(K)\,f^{(r+2)}(x) + o(h^2)}{%
        Bias = h^2/2 * mu2 * f(r+2)(x) + o(h^2)}
    \item \bold{Variance:}
      \eqn{Var[\hat{f}_h^{(r)}(x)] =
        \frac{f(x)\,R(K^{(r)})}{n\,h^{2r+1}} + o(1/(n\,h^{2r+1}))}{%
        Var = f(x)*R(K(r)) / (n*h^(2r+1)) + o(1/(n*h^(2r+1)))}
  }
  The AMISE-optimal bandwidth is of order \eqn{n^{-1/(2r+5)}}{n^(-1/(2r+5))},
  so higher derivatives require larger bandwidths.\cr

  The \bold{AMISE} is:
  \deqn{AMISE(\hat{f}_h^{(r)}) =
    \frac{R(K^{(r)})}{n\,h^{2r+1}} +
    \frac{\mu_2^2(K)}{4}\,h^4\,R(f^{(r+2)})}{%
    AMISE = R(K(r))/(n*h^(2r+1)) + mu2^2/4 * h^4 * R(f(r+2))}
  where \eqn{R(K^{(r)})=\int (K^{(r)})^2\,dx}{R(K(r)) = int K(r)^2 dx} and
  \eqn{\mu_2(K)=\int x^2 K(x)\,dx}{mu2(K) = int x^2 K(x) dx}.
}
\value{
  An S4 object of class \code{\linkS4class{dkde}} with slots \code{x}
  (evaluation points), \code{y} (estimates
  \eqn{\hat{f}_h^{(r)}(x_i)}{hat_f_h(r)(x_i)}), \code{h}, \code{kernel},
  \code{deriv.order}, \code{n}, \code{data} and \code{call}.  Use
  \code{show()} for a summary and \code{plot()} for a \pkg{ggplot2} graphic.
}
\references{
  Bhattacharya, P. K. (1967).
  Estimation of a probability density function and its derivatives.
  \emph{Sankhya: The Indian Journal of Statistics, Series A}, \bold{29},
  373--382.

  Radhey, S. S. (1987).
  MISE of kernel estimates of a density and its derivatives.
  \emph{Statistics and Probability Letters}, \bold{5}, 153--159.

  Schuster, E. F. (1969).
  Estimation of a probability density function and its derivatives.
  \emph{The Annals of Mathematical Statistics}, \bold{40}(4), 1187--1195.

  Scott, D. W. (1992).
  \emph{Multivariate Density Estimation. Theory, Practice and
  Visualization}.
  New York: Wiley.

  Silverman, B. W. (1986).
  \emph{Density Estimation for Statistics and Data Analysis}.
  Chapman & Hall/CRC. London.

  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing}.
  Chapman and Hall, London.

  Härdle, W. (1991).
  \emph{Smoothing Techniques, With Implementation in S}.
  Springer-Verlag, New York.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz},
  Amel Henider \email{amel.henider@univ-tam.dz} and
  Iago Giné-Vázquez \email{iago.gin-vaz@protonmail.com}}
\seealso{
  \code{\link{kernel.fun}}, \code{\link{h.ucv}}, \code{\link{h.bcv}},
  \code{\link{h.amise}}, \code{\link[stats]{density}} (for \eqn{r=0}).
}
\examples{
x <- rnorm(100)

## density (r = 0) with automatic UCV bandwidth
est0 <- dkde(data = x, deriv.order = 0)
est0
plot(est0)

## first derivative (r = 1)
est1 <- dkde(data = x, deriv.order = 1)
plot(est1)

## fixed bandwidth, other kernels; slots are accessed with @
est2 <- dkde(data = x, h = 0.5, kernel = "epanechnikov")
head(cbind(x = est2@x, y = est2@y))
plot(dkde(data = x, h = 0.6, kernel = kernel.pow(2, 3)), rug = FALSE)
}
\keyword{smooth}
\keyword{nonparametric}
\keyword{density derivative}
