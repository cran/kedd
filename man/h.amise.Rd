\name{h.amise}
\alias{h.amise}
\title{AMISE Reference (Maximal Smoothing) Bandwidth}
\description{
  Computes the reference bandwidth \eqn{h_{os}}{h_os} for the r'th
  derivative of a kernel density estimator: the AMISE-optimal bandwidth in
  which \eqn{R(f^{(r+2)})}{R(f(r+2))} is replaced by the value given by the
  maximal smoothing principle of Terrell (1990).  For \eqn{r = 0} it is the
  oversmoothed bandwidth, an upper bound for the optimal one.  Returns an S4
  object of class \code{\linkS4class{bandwidth}}.
}
\usage{
h.amise(x, deriv.order = 0L, kernel = "gaussian")
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
}
\details{
  The AMISE of the r'th derivative estimator is (Scott 1992, p.\,131):
  \deqn{AMISE(h;r)=\frac{R(K^{(r)})}{nh^{2r+1}} +
    \frac{\mu_2^2(K)}{4}\,h^{4}\,R\!\left(f^{(r+2)}\right)}{%
    AMISE(h;r) = R(K(r))/(n*h^(2r+1)) + mu2^2/4 * h^4 * R(f(r+2))}

  The minimising bandwidth is:
  \deqn{h_r^{\ast} =
    \left[\frac{(2r+1)\,R(K^{(r)})}{\mu_2^2(K)\,R(f^{(r+2)})}\right]^{1/(2r+5)}
    n^{-1/(2r+5)}}{%
    h*(r) = ((2r+1)*R(K(r)) / (mu2^2 * R(f(r+2))))^(1/(2r+5)) * n^(-1/(2r+5))}

  \eqn{R(f^{(r+2)})}{R(f(r+2))} is replaced by
  \eqn{35/(243\,\hat{\sigma}^{2r+5})}{35/(243 sigma_hat^(2r+5))}
  (maximal smoothing principle, Terrell 1990), where
  \eqn{\hat{\sigma}}{sigma_hat} is the sample standard deviation, which gives
  \deqn{h_{os} = \hat{\sigma}\left[\frac{243\,(2r+1)\,R(K^{(r)})}{35\,\mu_2^2(K)\,n}\right]^{1/(2r+5)}.}{%
    h_os = sigma_hat * (243 (2r+1) R(K(r)) / (35 mu2^2 n))^(1/(2r+5)).}
  The same value bounds the search interval of the cross-validation
  selectors (\eqn{[0.01\,h_{os}, 2\,h_{os}]}{[0.01 h_os, 2 h_os]}).\cr

  No optimisation is performed; the result is computed directly.
  \eqn{R(K^{(r)})=\int K^{(r)}(x)^2\,dx}{R(K(r))=int K(r)^2 dx},
  \eqn{\mu_2(K)=\int x^2 K(x)\,dx}{mu2(K)=int x^2 K(x) dx}.
}
\value{
  An S4 object of class \code{\linkS4class{bandwidth}} with slots:
  \describe{
    \item{\code{data}}{the (cleaned) data vector.}
    \item{\code{kernel}}{name of the kernel used.}
    \item{\code{deriv.order}}{derivative order \eqn{r}.}
    \item{\code{h}}{reference bandwidth \eqn{h_{os}}{h_os}.}
    \item{\code{min.obj}}{\code{NA} (no minimisation is performed).}
    \item{\code{hos}}{same as \code{h} (the reference bandwidth).}
    \item{\code{method}}{\code{"AMISE"}.}
  }
}
\references{
  Radhey, S. S. (1987).
  MISE of kernel estimates of a density and its derivatives.
  \emph{Statistics and Probability Letters}, \bold{5}, 153--159.

  Scott, D. W. (1992).
  \emph{Multivariate Density Estimation. Theory, Practice and
  Visualization}.
  New York: Wiley.

  Silverman, B. W. (1986).
  \emph{Density Estimation for Statistics and Data Analysis}.
  Chapman & Hall/CRC. London.

  Terrell, G. R. (1990).
  The maximal smoothing principle in density estimation.
  \emph{Journal of the American Statistical Association}, \bold{85},
  470--477.

  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing}.
  Chapman and Hall, London.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz},
  Amel Henider \email{amel.henider@univ-tam.dz} and
  Iago Giné-Vázquez \email{iago.gin-vaz@protonmail.com}}
\seealso{
  \code{\linkS4class{bandwidth}}, \code{\link{h.ucv}}, \code{\link{h.bcv}},
  \code{\link{h.ccv}}, \code{\link{h.mcv}}, \code{\link{h.tcv}}, \code{\link{h.scv}},
  \code{\link{h.mlcv}}, \code{\link{dkde}}.
}
\examples{
x <- rnorm(100)

## density (r = 0)
bw0 <- h.amise(x, deriv.order = 0)
show(bw0)

## first derivative (r = 1)
bw1 <- h.amise(x, deriv.order = 1)
show(bw1)
## other kernels on a Marron-Wand sample
h.amise(kurtotic, kernel = "logistic")
h.amise(kurtotic, kernel = kernel.pow(2, 4))
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
