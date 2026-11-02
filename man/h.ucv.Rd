\name{h.ucv}
\alias{h.ucv}
\title{Unbiased (Least-Squares) Cross-Validation for Bandwidth Selection}
\description{
  Computes the unbiased (least-squares) cross-validation bandwidth selector
  of the r'th derivative of a kernel density estimator (one-dimensional).
  Returns an S4 object of class \code{\linkS4class{bandwidth}}.
}
\usage{
h.ucv(x, deriv.order = 0L, kernel = "gaussian",
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
  \item{lower, upper}{bounds of the search interval. Defaults to
    \code{[0.01 * hos, 2 * hos]} where \code{hos} is the over-smoothing
    bandwidth computed internally.}
  \item{tol}{convergence tolerance passed to \code{\link{optimize}}.
    Default \code{0.01 * lower} (after validation of \code{lower}).}
  \item{nthreads}{number of OpenMP threads for this call: \code{0} (default)
    = all processors, \code{k > 0} = \code{k} threads, both capped by the
    environment variable \code{OMP_THREAD_LIMIT}. The result does not depend on
    it, and it never changes the threads used by later calls.}
}
\details{
  Rudemo (1982) and Bowman (1984) proposed unbiased (least-squares)
  cross-validation (UCV) for kernel density estimation.  Härdle,
  Marron and Wand (1990) extended the criterion to the r'th derivative
  estimator.  The bandwidth \eqn{\hat{h}_{UCV}^{(r)}}{h_UCV(r)} minimises:
  \deqn{UCV(h;r) = \frac{R\left(K^{(r)}\right)}{nh^{2r+1}} +
    \frac{(-1)^{r}}{n(n-1)h^{2r+1}} \sum_{i=1}^{n}\sum_{j \neq i}
    \Bigl(K^{(r)}\ast K^{(r)} - 2\,K^{(2r)}\Bigr)
    \!\left(\frac{X_j-X_i}{h}\right)}{%
    UCV(h;r) = R(K(r))/(n*h^(2r+1)) +
    (-1)^r/(n*(n-1)*h^(2r+1)) * sum_i sum_{j!=i}
    (K(r)*K(r) - 2*K(2r))((Xj-Xi)/h)}
  where \eqn{K^{(r)}\ast K^{(r)}(x)}{K(r)*K(r)(x)} is the convolution
  of the r'th derivative of the kernel (see \code{\link{kernel.conv}},
  \code{\link{kernel.fun}}), and
  \eqn{R(K^{(r)})=\int K^{(r)}(x)^2\,dx}{R(K(r)) = int K(r)(x)^2 dx}.\cr

  UCV is unbiased in the sense that
  \eqn{E[UCV(h;r)]=MISE[\hat{f}_h^{(r)}]-R(f^{(r)})}{E[UCV] = MISE - R(f(r))}
  (Scott and Terrell 1987).\cr

  The search is bounded by the over-smoothing bandwidth \code{hos}
  (Terrell and Scott 1985; Scott 1992, p.\,165; Wand and Jones 1995, p.\,61).
}
\value{
  An S4 object of class \code{\linkS4class{bandwidth}} with slots:
  \describe{
    \item{\code{data}}{the (cleaned) data vector.}
    \item{\code{kernel}}{name of the kernel used.}
    \item{\code{deriv.order}}{derivative order \eqn{r}.}
    \item{\code{h}}{selected bandwidth.}
    \item{\code{min.obj}}{minimum UCV value.}
    \item{\code{hos}}{over-smoothing bandwidth (upper bound).}
    \item{\code{method}}{\code{"UCV"}.}
  }
}
\references{
  Bowman, A. (1984).
  An alternative method of cross-validation for the smoothing of kernel
  density estimates.
  \emph{Biometrika}, \bold{71}, 353--360.

  Rudemo, M. (1982).
  Empirical choice of histograms and kernel density estimators.
  \emph{Scandinavian Journal of Statistics}, \bold{9}, 65--78.

  Scott, D. W. and Terrell, G. R. (1987).
  Biased and unbiased cross-validation in density estimation.
  \emph{Journal of the American Statistical Association}, \bold{82},
  1131--1146.

  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing}.
  Chapman and Hall, London.

  Härdle, W., Marron, J. S. and Wand, M. P. (1990).
  Bandwidth choice for density derivatives.
  \emph{Journal of the Royal Statistical Society, Series B}, 223--232.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz}}
\seealso{
  \code{\linkS4class{bandwidth}}, \code{\link{h.bcv}}, \code{\link{h.ccv}},
  \code{\link{h.mcv}}, \code{\link{h.tcv}}, \code{\link{h.scv}}, \code{\link{h.amise}},
  \code{\link{kernel.conv}}, \code{\link{kernel.fun}}, \code{\link{dkde}}.
}
\examples{
x <- rnorm(100)

## density (r = 0)
bw0 <- h.ucv(x, deriv.order = 0)
show(bw0)
plot(bw0)

## first derivative (r = 1)
bw1 <- h.ucv(x, deriv.order = 1)
show(bw1)
## other kernels on a Marron-Wand sample
h.ucv(claw, kernel = "logistic")
h.ucv(claw, kernel = "pow(2,3)")
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
