\name{h.mcv}
\alias{h.mcv}
\title{Modified Cross-Validation for Bandwidth Selection}
\description{
  Computes the modified cross-validation bandwidth selector of the r'th
  derivative of a kernel density estimator (one-dimensional).
  Returns an S4 object of class \code{\linkS4class{bandwidth}}.
}
\usage{
h.mcv(x, deriv.order = 0L, kernel = "gaussian",
      lower, upper, tol, nthreads = 0L)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{deriv.order}{non-negative integer: derivative order \eqn{r}. MCV uses
    \eqn{K^{(r+1)}}, so the maximum \eqn{r} is: any for Gaussian and
    Silverman; cosine 0; logistic 8; Epanechnikov 0; biweight 1; triweight 2;
    tricube 2; \code{pow(s,t)} \eqn{\min(r^*-1, 6)}{min(r*-1, 6)} (see
    \code{\link{h.ucv}} for \eqn{r^*}{r*}); not available for uniform and
    triangular. Above it \code{h = NA} is returned with a warning. See
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
  Stute (1992) proposed \emph{modified cross-validation} (MCV), a
  modification of unbiased cross-validation that approximates the
  problematic term via the Hajek projection.
  \eqn{\hat{h}_{MCV}^{(r)}}{h_MCV(r)} minimises:
  \deqn{MCV(h;r) = \frac{R(K^{(r)})}{nh^{2r+1}} +
    \frac{(-1)^r}{n(n-1)h^{2r+1}}
    \sum_i\sum_{j \neq i}
    \varphi^{(r)}\!\left(\frac{X_j-X_i}{h}\right)}{%
    MCV(h;r) = R(K(r))/(n*h^(2r+1)) +
    (-1)^r/(n*(n-1)*h^(2r+1)) *
    sum_i sum_{j!=i} phi(r)((Xj-Xi)/h)}
  where
  \deqn{\varphi^{(r)}(c) =
    \Bigl(K^{(r)}\ast K^{(r)} - K^{(2r)} -
          \tfrac{\mu_2(K)}{2}\,K^{(2r+2)}\Bigr)(c)}{%
    phi(r)(c) = K(r)*K(r)(c) - K(2r)(c) - mu2/2 * K(2r+2)(c)}
  and \eqn{K^{(r)}\ast K^{(r)}}{K(r)*K(r)} is the convolution of the
  r'th kernel derivative (see \code{\link{kernel.conv}},
  \code{\link{kernel.fun}}),
  \eqn{R(K^{(r)})=\int K^{(r)}(x)^2\,dx}{R(K(r))=int K(r)^2 dx},
  \eqn{\mu_2(K)=\int x^2 K(x)\,dx}{mu2(K)=int x^2 K(x) dx}.\cr

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
    \item{\code{min.obj}}{minimum MCV value.}
    \item{\code{hos}}{over-smoothing bandwidth (upper bound).}
    \item{\code{method}}{\code{"MCV"}.}
  }
}
\references{
  Heidenreich, N. B., Schindler, A. and Sperlich, S. (2013).
  Bandwidth selection for kernel density estimation: a review of fully
  automatic selectors.
  \emph{Advances in Statistical Analysis}.

  Stute, W. (1992).
  Modified cross validation in density estimation.
  \emph{Journal of Statistical Planning and Inference}, \bold{30},
  293--305.

  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing}.
  Chapman and Hall, London.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz}}
\seealso{
  \code{\linkS4class{bandwidth}}, \code{\link{h.ucv}}, \code{\link{h.bcv}},
  \code{\link{h.ccv}}, \code{\link{h.tcv}}, \code{\link{h.scv}}, \code{\link{h.amise}},
  \code{\link{kernel.conv}}, \code{\link{kernel.fun}}.
}
\examples{
x <- rnorm(100)

## density (r = 0)
bw0 <- h.mcv(x, deriv.order = 0)
show(bw0)
plot(bw0)

## first derivative (r = 1)
bw1 <- h.mcv(x, deriv.order = 1)
show(bw1)
## other kernels on a Marron-Wand sample
h.mcv(claw, kernel = "logistic")
h.mcv(claw, kernel = kernel.pow(2, 2))
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
