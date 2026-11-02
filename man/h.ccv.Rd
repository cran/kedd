\name{h.ccv}
\alias{h.ccv}
\title{Complete Cross-Validation for Bandwidth Selection}
\description{
  Computes the complete cross-validation bandwidth selector of the r'th
  derivative of a kernel density estimator (one-dimensional).
  Returns an S4 object of class \code{\linkS4class{bandwidth}}.
}
\usage{
h.ccv(x, deriv.order = 0L, kernel = "gaussian",
      lower, upper, tol, nthreads = 0L)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{deriv.order}{non-negative integer: derivative order \eqn{r}. CCV uses
    \eqn{K^{(r+2)}}, so the maximum \eqn{r} is: any for Gaussian and
    Silverman; logistic 8; biweight 0; triweight 1; tricube 1;
    \code{pow(s,t)} \eqn{\min(r^*-2, 6)}{min(r*-2, 6)} (see \code{\link{h.ucv}}
    for \eqn{r^*}{r*}); not available for uniform, triangular,
    Epanechnikov and cosine. Above it \code{h = NA} is returned with a warning. See
    \code{\link{kedd-package}}, section \sQuote{Maximum derivative order}.}
  \item{kernel}{character string giving the kernel; one of
    \code{"gaussian"} (default), \code{"epanechnikov"}, \code{"uniform"},
    \code{"triangular"}, \code{"biweight"}, \code{"triweight"},
    \code{"tricube"}, \code{"cosine"}, \code{"silverman"}, \code{"logistic"},
    or a power kernel \code{"pow(s,t)"} (see \code{\link{kernel.pow}}).}
  \item{lower, upper}{bounds of the search interval.  Defaults to
    \code{[0.01 * hos, 2 * hos]}.}
  \item{tol}{convergence tolerance for \code{\link{optimize}}.
    Default \code{0.01 * lower} (after validation of \code{lower}).}
  \item{nthreads}{number of OpenMP threads for this call: \code{0} (default)
    = all processors, \code{k > 0} = \code{k} threads, both capped by the
    environment variable \code{OMP_THREAD_LIMIT}. The result does not depend on
    it, and it never changes the threads used by later calls.}
}
\details{
  Jones and Kappenman (1991) proposed \emph{complete cross-validation}
  (CCV) for kernel density estimation.  The method estimates the
  integrated squared density derivative using
  \eqn{\bar{\theta}_r(h)}{bar_theta(h;r)} (Peter and Marron 1987).
  \eqn{\hat{h}_{CCV}^{(r)}}{h_CCV(r)} minimises:
  \deqn{CCV(h;r) = R\!\left(\hat{f}_h^{(r)}\right) - \bar{\theta}_r(h)
    + \tfrac{1}{2}\mu_2(K)\,h^2\,\bar{\theta}_{r+1}(h)
    + \tfrac{1}{24}\bigl(6\mu_2^2(K)-\delta(K)\bigr)\,h^4\,
      \bar{\theta}_{r+2}(h)}{%
    CCV(h;r) = R(hat_f(h;r)) - bar_theta(h;r) +
    1/2 * mu2 * h^2 * bar_theta(h;r+1) +
    1/24 * (6*mu2^2 - delta) * h^4 * bar_theta(h;r+2)}
  with
  \deqn{R\!\left(\hat{f}_h^{(r)}\right) =
    \frac{R(K^{(r)})}{nh^{2r+1}} +
    \frac{(-1)^r}{n(n-1)h^{2r+1}}
    \sum_{i}\sum_{j \neq i}
    K^{(r)}\ast K^{(r)}\!\left(\tfrac{X_j-X_i}{h}\right)}{%
    R(hat_f(h;r)) = R(K(r))/(n*h^(2r+1)) +
    (-1)^r/(n*(n-1)*h^(2r+1)) * sum_i sum_{j!=i}
    K(r)*K(r)((Xj-Xi)/h)}
  and
  \deqn{\bar{\theta}_r(h)=
    \frac{(-1)^r}{n(n-1)h^{2r+1}}
    \sum_i\sum_{j \neq i}
    K^{(2r)}\!\left(\tfrac{X_j-X_i}{h}\right)}{%
    bar_theta(h;r) = (-1)^r/(n*(n-1)*h^(2r+1)) *
    sum_i sum_{j!=i} K(2r)((Xj-Xi)/h)}
  where \eqn{R(K^{(r)})=\int K^{(r)}(x)^2\,dx}{R(K(r))=int K(r)^2 dx},
  \eqn{\mu_2(K)=\int x^2 K(x)\,dx}{mu2(K)=int x^2 K(x) dx},
  \eqn{\delta(K)=\int x^4 K(x)\,dx}{delta(K)=int x^4 K(x) dx}.\cr

  The search is bounded by the over-smoothing bandwidth \code{hos}
  (Scott 1992, p.\,165; Wand and Jones 1995, p.\,61).
}
\value{
  An S4 object of class \code{\linkS4class{bandwidth}} with slots:
  \describe{
    \item{\code{data}}{the (cleaned) data vector.}
    \item{\code{kernel}}{name of the kernel used.}
    \item{\code{deriv.order}}{derivative order \eqn{r}.}
    \item{\code{h}}{selected bandwidth.}
    \item{\code{min.obj}}{minimum CCV value.}
    \item{\code{hos}}{over-smoothing bandwidth (upper bound).}
    \item{\code{method}}{\code{"CCV"}.}
  }
}
\references{
  Jones, M. C. and Kappenman, R. F. (1991).
  On a class of kernel density estimate bandwidth selectors.
  \emph{Scandinavian Journal of Statistics}, \bold{19}, 337--349.

  Peter, H. and Marron, J. S. (1987).
  Estimation of integrated squared density derivatives.
  \emph{Statistics and Probability Letters}, \bold{6}, 109--115.

  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing}.
  Chapman and Hall, London.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz}}
\seealso{
  \code{\linkS4class{bandwidth}}, \code{\link{h.ucv}}, \code{\link{h.bcv}},
  \code{\link{h.mcv}}, \code{\link{h.tcv}}, \code{\link{h.scv}}, \code{\link{h.amise}},
  \code{\link{kernel.conv}}, \code{\link{kernel.fun}}.
}
\examples{
x <- rnorm(100)

## density (r = 0)
bw0 <- h.ccv(x, deriv.order = 0)
show(bw0)
plot(bw0)

## first derivative (r = 1)
bw1 <- h.ccv(x, deriv.order = 1)
show(bw1)
## other kernels on a Marron-Wand sample
h.ccv(trimodal, kernel = "logistic")
h.ccv(trimodal, kernel = "pow(2,3)")
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
