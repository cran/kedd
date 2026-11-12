\name{h.bcv}
\alias{h.bcv}
\title{Biased Cross-Validation for Bandwidth Selection}
\description{
  Computes the biased cross-validation bandwidth selector (BCV1 or BCV2)
  of the r'th derivative of a kernel density estimator (one-dimensional).
  Returns an S4 object of class \code{\linkS4class{bandwidth}}.
}
\usage{
h.bcv(x, whichbcv = 1L, deriv.order = 0L, kernel = "gaussian",
      lower, upper, tol, nthreads = 0L)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{whichbcv}{integer: \code{1} for BCV1 (Scott and Terrell 1987,
    default) or \code{2} for BCV2 (Jones and Kappenman 1991).}
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
  Let
  \deqn{\hat{\theta}_{r}(h)= \frac{(-1)^{r}}{n(n-1)h^{2r+1}}
    \sum_{i=1}^{n}\sum_{j \neq i}
    K^{(r)}\ast K^{(r)}\!\left(\frac{X_j-X_i}{h}\right)}{%
    hat_theta(h;r) = (-1)^r/(n*(n-1)*h^(2r+1)) *
    sum_i sum_{j!=i} K(r)*K(r)((Xj-Xi)/h)}
  and
  \deqn{\bar{\theta}_{r}(h)= \frac{(-1)^r}{n(n-1)h^{2r+1}}
    \sum_{i=1}^{n}\sum_{j \neq i}
    K^{(2r)}\!\left(\frac{X_j-X_i}{h}\right)}{%
    bar_theta(h;r) = (-1)^r/(n*(n-1)*h^(2r+1)) *
    sum_i sum_{j!=i} K(2r)((Xj-Xi)/h)}

  \bold{BCV1} (Scott and Terrell 1987) minimises:
  \deqn{BCV1(h;r)= \frac{R\left(K^{(r)}\right)}{nh^{2r+1}} +
    \frac{\mu_2^2(K)}{4}\,h^{4}\,\hat{\theta}_{r+2}(h)}{%
    BCV1(h;r) = R(K(r))/(n*h^(2r+1)) + mu2^2/4 * h^4 * hat_theta(h;r+2)}

  \bold{BCV2} (Jones and Kappenman 1991) minimises:
  \deqn{BCV2(h;r)= \frac{R\left(K^{(r)}\right)}{nh^{2r+1}} +
    \frac{\mu_2^2(K)}{4}\,h^{4}\,\bar{\theta}_{r+2}(h)}{%
    BCV2(h;r) = R(K(r))/(n*h^(2r+1)) + mu2^2/4 * h^4 * bar_theta(h;r+2)}

  where \eqn{K^{(r)}\ast K^{(r)}}{K(r)*K(r)} is the convolution of the
  r'th kernel derivative (see \code{\link{kernel.conv}},
  \code{\link{kernel.fun}}),
  \eqn{R(K^{(r)})=\int K^{(r)}(x)^2\,dx}{R(K(r))=int K(r)^2 dx},
  \eqn{\mu_2(K)=\int x^2 K(x)\,dx}{mu2(K)=int x^2 K(x) dx}.\cr

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
    \item{\code{min.obj}}{minimum BCV value.}
    \item{\code{hos}}{over-smoothing bandwidth (upper bound).}
    \item{\code{method}}{\code{"BCV1"} or \code{"BCV2"}.}
  }
}
\references{
  Jones, M. C. and Kappenman, R. F. (1991).
  On a class of kernel density estimate bandwidth selectors.
  \emph{Scandinavian Journal of Statistics}, \bold{19}, 337--349.

  Peter, H. and Marron, J. S. (1987).
  Estimation of integrated squared density derivatives.
  \emph{Statistics and Probability Letters}, \bold{6}, 109--115.

  Scott, D. W. and Terrell, G. R. (1987).
  Biased and unbiased cross-validation in density estimation.
  \emph{Journal of the American Statistical Association}, \bold{82},
  1131--1146.

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
  \code{\linkS4class{bandwidth}}, \code{\link{h.ucv}}, \code{\link{h.ccv}},
  \code{\link{h.mcv}}, \code{\link{h.tcv}}, \code{\link{h.scv}}, \code{\link{h.amise}},
  \code{\link{kernel.conv}}, \code{\link{kernel.fun}}, \code{\link{dkde}}.
}
\examples{
x <- rnorm(100)

## BCV1, density (r = 0)
bw1 <- h.bcv(x, whichbcv = 1, deriv.order = 0)
show(bw1)

## BCV2, first derivative (r = 1)
bw2 <- h.bcv(x, whichbcv = 2, deriv.order = 1)
show(bw2)
plot(bw2)
## other kernels on a Marron-Wand sample
h.bcv(bimodal, kernel = "logistic")
h.bcv(bimodal, whichbcv = 2, kernel = kernel.pow(2, 4))
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
