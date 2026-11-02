\name{h.tcv}
\alias{h.tcv}
\title{Trimmed Cross-Validation for Bandwidth Selection}
\description{
  Computes the trimmed cross-validation bandwidth selector of the r'th
  derivative of a kernel density estimator (one-dimensional).
  Returns an S4 object of class \code{\linkS4class{bandwidth}}.
}
\usage{
h.tcv(x, deriv.order = 0L, kernel = "gaussian",
      lower, upper, tol, nthreads = 0L)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{deriv.order}{non-negative integer: derivative order \eqn{r}. Maximum
    \eqn{r} for this selector: any for Gaussian and Silverman; cosine 1;
    logistic 8; Epanechnikov 1; uniform 0; triangular 0; biweight 2;
    triweight 3; tricube 3; \code{pow(s,t)} \eqn{\min(r^*, 6)}{min(r*, 6)}, where
    \eqn{r^*}{r*} is the largest integer \eqn{r < t + 1/2} (and
    \eqn{r < s + 1/2} if \eqn{s} is not an even integer, and \eqn{2r \le st}{2r <= st}
    if \eqn{s} and \eqn{t} are integers). Above it \code{h = NA}
    is returned with a warning. See \code{\link{kedd-package}}, section
    \sQuote{Maximum derivative order}.}
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
  Feluch and Koronacki (1992) proposed \emph{trimmed cross-validation}
  (TCV), a modification of unbiased cross-validation that trims small
  pairwise distances.  \eqn{\hat{h}_{TCV}^{(r)}}{h_TCV(r)} minimises:
  \deqn{TCV(h;r) = \frac{R(K^{(r)})}{nh^{2r+1}} +
    \frac{(-1)^r}{n(n-1)h^{2r+1}}
    \sum_i\sum_{j \neq i}
    \varphi^{(r)}\!\left(\frac{X_j-X_i}{h}\right)}{%
    TCV(h;r) = R(K(r))/(n*h^(2r+1)) +
    (-1)^r/(n*(n-1)*h^(2r+1)) *
    sum_i sum_{j!=i} phi(r)((Xj-Xi)/h)}
  where
  \deqn{\varphi^{(r)}(c) =
    \Bigl(K^{(r)}\ast K^{(r)} -
          2\,K^{(2r)}\chi\!\bigl(|c|>c_n/h^{2r+1}\bigr)\Bigr)(c)}{%
    phi(r)(c) = K(r)*K(r)(c) - 2*K(2r)(c)*chi(|c| > cn/h^(2r+1))}
  with \eqn{c_n=1/n}{cn=1/n} (ensuring convergence),
  \eqn{\chi(.)}{chi(.)} the indicator function,
  and \eqn{K^{(r)}\ast K^{(r)}}{K(r)*K(r)} the convolution of the
  r'th kernel derivative (see \code{\link{kernel.conv}},
  \code{\link{kernel.fun}}).\cr

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
    \item{\code{min.obj}}{minimum TCV value.}
    \item{\code{hos}}{over-smoothing bandwidth (upper bound).}
    \item{\code{method}}{\code{"TCV"}.}
  }
}
\references{
  Feluch, W. and Koronacki, J. (1992).
  A note on modified cross-validation in density estimation.
  \emph{Computational Statistics and Data Analysis}, \bold{13}, 143--151.

  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing}.
  Chapman and Hall, London.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz}}
\seealso{
  \code{\linkS4class{bandwidth}}, \code{\link{h.ucv}}, \code{\link{h.bcv}},
  \code{\link{h.ccv}}, \code{\link{h.mcv}}, \code{\link{h.amise}}, \code{\link{h.scv}},
  \code{\link{kernel.conv}}, \code{\link{kernel.fun}}.
}
\examples{
x <- rnorm(100)

## density (r = 0)
bw0 <- h.tcv(x, deriv.order = 0)
show(bw0)
plot(bw0)

## first derivative (r = 1)
bw1 <- h.tcv(x, deriv.order = 1)
show(bw1)
## other kernels on a Marron-Wand sample
h.tcv(kurtotic, kernel = "logistic")
h.tcv(kurtotic, kernel = "pow(2,3)")
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
