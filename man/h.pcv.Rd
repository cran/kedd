\name{h.pcv}
\alias{h.pcv}
\title{Partitioned Cross-Validation for Bandwidth Selection}
\description{
  Computes the partitioned cross-validation (PCV) bandwidth selector of
  Marron (1987) for the r'th derivative of a kernel density estimator
  (one-dimensional): the sample is partitioned at random into \code{m}
  subsamples, a cross-validation score is computed in each subsample, the
  bandwidth minimises their average and is rescaled from the subsample size to
  the sample size. Returns an S4 object of class \code{\linkS4class{bandwidth}}.
}
\usage{
h.pcv(x, m = 2L, criterion = "UCV", nrep = 1L, deriv.order = 0L,
      kernel = "gaussian", lower, upper, tol, nthreads = 0L)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{m}{number of subsamples, a positive integer; each subsample must
    contain at least 2 observations. \code{m = 1} gives the score itself.}
  \item{criterion}{cross-validation score computed in each subsample: one of
    \code{"UCV"} (default), \code{"TCV"}, \code{"MCV"}, \code{"CCV"},
    \code{"BCV1"} and \code{"BCV2"} (see \code{\link{h.ucv}},
    \code{\link{h.tcv}}, \code{\link{h.mcv}}, \code{\link{h.ccv}} and
    \code{\link{h.bcv}}).}
  \item{nrep}{number of random partitions, a positive integer. \code{nrep = 1}
    (default) is the method of Marron (1987); with \code{nrep > 1} the average
    score is also averaged over \code{nrep} independent random partitions.}
  \item{deriv.order}{non-negative integer: derivative order \eqn{r}. The
    maximum order is that of \code{criterion} for the kernel; above it
    \code{h = NA} is returned with a warning. See \code{\link{kedd-package}},
    section \sQuote{Maximum derivative order}.}
  \item{kernel}{character string giving the kernel; one of
    \code{"gaussian"} (default), \code{"epanechnikov"}, \code{"uniform"},
    \code{"triangular"}, \code{"biweight"}, \code{"triweight"},
    \code{"tricube"}, \code{"cosine"}, \code{"silverman"}, \code{"logistic"},
    or a power kernel \code{"pow(s,t)"} (see \code{\link{kernel.pow}}).}
  \item{lower, upper}{bounds of the search interval for the bandwidth \eqn{h} of
    the whole sample. Defaults to \code{[0.01 * hos, 2 * hos]}, where
    \code{hos} is the over-smoothing bandwidth of the whole sample.}
  \item{tol}{convergence tolerance passed to \code{\link{optimize}}.
    Default \code{0.01 * lower} (after validation of \code{lower}).}
  \item{nthreads}{number of OpenMP threads for this call: \code{0} (default)
    = all processors, \code{k > 0} = \code{k} threads, both capped by the
    environment variable \code{OMP_THREAD_LIMIT}.}
}
\details{
  Marron (1987) proposed partitioned cross-validation to reduce the
  across-sample variability of the cross-validation bandwidth. The indices
  \eqn{1, \dots, n} are partitioned at random, independently of the data, into
  \eqn{m} subsamples \eqn{S_1, \dots, S_m} of sizes about \eqn{n/m} (they differ
  by at most one). If \eqn{CV_j(b;r)} denotes the score \code{criterion}
  computed on the observations of \eqn{S_j} with the bandwidth \eqn{b}, the
  average score \eqn{m^{-1} \sum_j CV_j(b;r)} is minimised by a bandwidth
  \eqn{\hat b} suited to a sample of size \eqn{n/m}, and since the optimal
  bandwidth is of order \eqn{n^{-1/(2r+5)}}{n^(-1/(2r+5))},
  \deqn{\hat h_{PCV} = m^{-1/(2r+5)}\, \hat b}{h_PCV = m^(-1/(2r+5)) b}
  is the bandwidth of the whole sample. Equivalently, \code{h.pcv()} minimises
  in \eqn{h}
  \deqn{PCV(h;r) = \frac{1}{m} \sum_{j=1}^{m} CV_j\big(h\, m^{1/(2r+5)};r\big).}{%
    PCV(h;r) = (1/m) sum_j CV_j(h m^(1/(2r+5)); r).}
  For \eqn{r = 0} and UCV, this is exactly the method of Marron (1987), with
  the rescaling \eqn{m^{-1/5}}{m^(-1/5)}. The other scores, which Marron
  suggests (for BCV) but does not study, the derivatives (\eqn{r \ge 1}) and
  \code{nrep > 1}, an average over several random partitions in the spirit of
  the suggestion of N. I. Fisher reported by Marron (1987, Section 4), are
  extensions. For the fourth-order Silverman kernel the exponent
  \eqn{1/(2r+5)} is replaced by \eqn{1/(2r+9)}.\cr

  A larger \eqn{m} reduces the variance of the bandwidth across samples but
  increases a bias, since the rescaling is only asymptotically correct (Marron
  1987, Section 3); for derivatives the subsamples should be larger. The
  bandwidth also depends on the random partition; \code{nrep > 1} makes it
  more stable, at a cost multiplied by \code{nrep}. Each subsample contains
  about \eqn{n^2/(2m^2)} pairs, so that one partition costs about \eqn{1/m} of
  the score on the whole sample.\cr

  As for the other selectors, the average score is minimised with
  \code{\link{optimize}} over the search interval; it may have several local
  minima, and for CCV and BCV2 a spurious one at small bandwidths, so that its
  curve (\code{plot()}) should be checked and \code{lower} or \code{upper}
  changed if needed. The partition is random: use \code{\link{set.seed}} for reproducible results. The partitions
  are kept in the slot \code{groups}, so that \code{plot()} and \code{lines()}
  draw \eqn{PCV(h;r)}.
}
\value{
  An S4 object of class \code{\linkS4class{bandwidth}} with slots:
  \describe{
    \item{\code{data}}{the (cleaned) data vector.}
    \item{\code{kernel}}{name of the kernel used.}
    \item{\code{deriv.order}}{derivative order \eqn{r}.}
    \item{\code{h}}{selected bandwidth for the whole sample.}
    \item{\code{min.obj}}{minimum value of \eqn{PCV(h;r)}.}
    \item{\code{hos}}{over-smoothing bandwidth of the whole sample.}
    \item{\code{method}}{\code{"PCV"}.}
    \item{\code{criterion}}{the score computed in each subsample.}
    \item{\code{groups}}{the subsample of each observation, for each partition
      one after the other (length \code{nrep * length(data)}).}
  }
}
\references{
  Hall, P. and Robinson, A. P. (2009).
  Reducing variability of crossvalidation for smoothing-parameter choice.
  \emph{Biometrika}, \bold{96}, 175--186.

  Marron, J. S. (1987).
  Partitioned cross-validation.
  \emph{Econometric Reviews}, \bold{6}(2), 271--283.
  \doi{10.1080/07474938708800136}
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz},
  Amel Henider \email{amel.henider@univ-tam.dz} and
  Iago Giné-Vázquez \email{iago.gin-vaz@protonmail.com}}
\seealso{
  \code{\linkS4class{bandwidth}}, \code{\link{h.ucv}}, \code{\link{h.tcv}},
  \code{\link{h.mcv}}, \code{\link{h.ccv}}, \code{\link{h.bcv}},
  \code{\link{h.scv}}, \code{\link{h.amise}}, \code{\link{h.mlcv}}, \code{\link{dkde}}.
}
\examples{
set.seed(1)
x <- rnorm(400)

## density (r = 0), UCV in 4 subsamples: the method of Marron (1987)
bw0 <- h.pcv(x, m = 4)
bw0
plot(bw0)

## first derivative (r = 1), TCV in 2 subsamples
h.pcv(x, m = 2, criterion = "TCV", deriv.order = 1)

## second derivative (r = 2), UCV in 4 subsamples averaged over 10 partitions
h.pcv(x, m = 4, nrep = 10, deriv.order = 2)

## another kernel
h.pcv(bimodal, m = 2, kernel = "pow(2,4)")
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
