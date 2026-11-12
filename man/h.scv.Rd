\name{h.scv}
\alias{h.scv}
\title{Smoothed Cross-Validation for Bandwidth Selection}
\description{
  Selects the bandwidth of the r'th derivative kernel density estimator by
  minimising the smoothed cross-validation (SCV) criterion of Hall, Marron
  and Park (1992).  Returns an S4 object of class
  \code{\linkS4class{bandwidth}}.
}
\usage{
h.scv(x, g = NULL, deriv.order = 0L, kernel = "gaussian",
      lower, upper, tol, nthreads = 0L)
}
\arguments{
  \item{x}{numeric vector of data values.}
  \item{g}{pilot bandwidth (positive number).  If \code{NULL} (default),
    \eqn{g = \hat{\sigma}\,0.9266\,n^{-2/13}} (Hall et al. 1992, eq. 4.2).}
  \item{deriv.order}{non-negative integer: derivative order \eqn{r}; no upper
    limit (Gaussian kernel). See \code{\link{kedd-package}}, section
    \sQuote{Maximum derivative order}.}
  \item{kernel}{must be \code{"gaussian"}, the only kernel for which the
    SCV criterion is available in closed form.}
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
  With a Gaussian kernel and a Gaussian pilot estimate \eqn{\tilde f_g}, the
  integrated squared bias of \eqn{\hat f_h^{(r)}} is estimated in closed form
  by derivatives of normal densities:
  \deqn{SCV(h; r) = \frac{R(K^{(r)})}{n h^{2r+1}} + \frac{(-1)^r\,2}{n(n-1)}
    \sum_{i<j} \left[\phi^{(2r)}_{s_1} - 2\phi^{(2r)}_{s_2}
    + \phi^{(2r)}_{s_3}\right](X_j - X_i),}{%
    SCV(h; r) = R(K(r))/(n h^(2r+1)) + (-1)^r 2/(n(n-1)) sum_{i<j}
    [phi(2r)_s1 - 2 phi(2r)_s2 + phi(2r)_s3](Xj - Xi),}
  where \eqn{s_1 = \sqrt{2(h^2+g^2)}}{s1 = sqrt(2(h^2+g^2))},
  \eqn{s_2 = \sqrt{h^2+2g^2}}{s2 = sqrt(h^2+2g^2)},
  \eqn{s_3 = \sqrt{2}\,g}{s3 = sqrt(2) g} and
  \eqn{\phi^{(k)}_s(u) = \phi^{(k)}(u/s)/s^{k+1}}{phi(k)_s(u) = phi(k)(u/s)/s^(k+1)}.
  It follows from
  \eqn{\int [((K_h-\delta) * \tilde f_g)^{(r)}]^2 = (-1)^r \int
  ((K_h-\delta) * \tilde f_g)\,((K_h-\delta) * \tilde f_g)^{(2r)}}{int
  [((K_h - delta) * f_g)^(r)]^2 = (-1)^r int u u^(2r)}.
  For \eqn{r = 0} it is the criterion of Hall, Marron and Park (1992).

  The default pilot bandwidth is the density pilot of Hall et al.; for
  derivatives a pilot can be supplied through \code{g}.
}
\value{
  An S4 object of class \code{\linkS4class{bandwidth}} with
  \code{method = "SCV"}.
}
\references{
  Hall, P., Marron, J. S. and Park, B. U. (1992).
  Smoothed cross-validation.
  \emph{Probability Theory and Related Fields}, \bold{92}, 1--20.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz},
  Amel Henider \email{amel.henider@univ-tam.dz} and
  Iago Giné-Vázquez \email{iago.gin-vaz@protonmail.com}}
\seealso{
  \code{\link{h.ucv}}, \code{\link{h.bcv}}, \code{\linkS4class{bandwidth}}.
}
\examples{
x <- rnorm(200)
bw <- h.scv(x)
show(bw)
plot(bw)

## first derivative (r = 1)
bw1 <- h.scv(x, deriv.order = 1)
plot(dkde(data = x, h = bw1@h, deriv.order = 1))
## on a Marron-Wand sample (SCV is defined for the Gaussian kernel)
h.scv(bimodal)
h.scv(bimodal, deriv.order = 2)
}
\keyword{smooth}
\keyword{nonparametric}
