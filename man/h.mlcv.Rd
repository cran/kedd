\name{h.mlcv}
\alias{h.mlcv}
\title{Maximum-Likelihood Cross-Validation for Bandwidth Selection}
\description{
  Computes the maximum-likelihood (Kullback-Leibler) cross-validation
  bandwidth selector for a kernel density estimator (density estimation
  only, \code{deriv.order = 0}).
  Returns an S4 object of class \code{\linkS4class{bandwidth}}.
}
\usage{
h.mlcv(x, kernel = "gaussian", lower, upper, tol, nthreads = 0L)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{kernel}{character string giving the kernel; one of
    \code{"gaussian"} (default), \code{"epanechnikov"}, \code{"uniform"},
    \code{"triangular"}, \code{"biweight"}, \code{"triweight"},
    \code{"tricube"}, \code{"cosine"}, \code{"silverman"}, \code{"logistic"},
    or a power kernel \code{"pow(s,t)"} (see \code{\link{kernel.pow}}).}
  \item{lower, upper}{bounds of the search interval.  Defaults to
    \code{[0.01 * hos, 2 * hos]}.}
  \item{tol}{convergence tolerance for \code{\link{optimize}}.
    Default \code{0.01 * lower}.}
  \item{nthreads}{number of OpenMP threads for this call: \code{0} (default)
    = all processors, \code{k > 0} = \code{k} threads, both capped by the
    environment variable \code{OMP_THREAD_LIMIT}. The result does not depend on
    it, and it never changes the threads used by later calls.}
}
\details{
  Habbema, Hermans, and Van den Broek (1974) and Duin (1976) proposed
  maximising the leave-one-out log-likelihood:
  \deqn{MLCV(h) = n^{-1}\sum_{i=1}^{n}
    \log\!\left[\hat{f}_{h,i}(X_i)\right]}{%
    MLCV(h) = (1/n) * sum_i log(hat_f_{-i}(Xi))}
  where the leave-one-out estimator is
  \deqn{\hat{f}_{h,i}(X_i) =
    \frac{1}{(n-1)h}\sum_{j \neq i}K\!\left(\frac{X_j-X_i}{h}\right)}{%
    hat_f_{-i}(Xi) = 1/((n-1)*h) * sum_{j!=i} K((Xj-Xi)/h)}
  Equivalently,
  \deqn{h_{mlcv} = \arg\max_{h}\left(n^{-1}\sum_{i}\log\!\left[
    \sum_{j \neq i}K\!\left(\frac{X_j-X_i}{h}\right)\right]
    -\log[(n-1)h]\right)}{%
    h_mlcv = argmax_h { (1/n)*sum_i log(sum_{j!=i} K((Xj-Xi)/h))
    - log((n-1)*h) }}

  Only applicable to density estimation (\eqn{r=0}).
}
\value{
  An S4 object of class \code{\linkS4class{bandwidth}} with slots:
  \describe{
    \item{\code{data}}{the (cleaned) data vector.}
    \item{\code{kernel}}{name of the kernel used.}
    \item{\code{deriv.order}}{\code{0L} (density only).}
    \item{\code{h}}{selected bandwidth.}
    \item{\code{min.obj}}{negative of the maximum MLCV value.}
    \item{\code{hos}}{over-smoothing bandwidth (upper bound).}
    \item{\code{method}}{\code{"MLCV"}.}
  }
}
\references{
  Duin, R. P. W. (1976).
  On the choice of smoothing parameters of Parzen estimators of
  probability density functions.
  \emph{IEEE Transactions on Computers}, \bold{C-25}, 1175--1179.

  Habbema, J. D. F., Hermans, J. and Van den Broek, K. (1974).
  A stepwise discrimination analysis program using density estimation.
  \emph{Compstat 1974: Proceedings in Computational Statistics}.
  Physica Verlag, Vienna.

  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing}.
  Chapman and Hall, London.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz}}
\seealso{
  \code{\linkS4class{bandwidth}}, \code{\link{h.ucv}}, \code{\link{h.amise}}, \code{\link{h.scv}},
  \code{\link{dkde}}.
}
\examples{
x <- rnorm(100)

bw <- h.mlcv(x)
show(bw)
plot(bw)

bw2 <- h.mlcv(x, kernel = "epanechnikov")
show(bw2)
## other kernels on a Marron-Wand sample
h.mlcv(outlier, kernel = "logistic")
h.mlcv(outlier, kernel = "pow(2,2)")
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
