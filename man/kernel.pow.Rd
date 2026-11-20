\name{kernel.pow}
\alias{kernel.pow}
\title{Power Kernel Family Pow(s,t)}
\description{
  Builds the name of a kernel of the power family
  \deqn{K_{s,t}(x) = c_{s,t}\,(1-|x|^s)^t\,\mathbf{1}_{|x|\le 1},
        \qquad c_{s,t} = \frac{s}{2\,B(1/s,\,t+1)},}{%
        K(x) = c(s,t) (1-|x|^s)^t 1{|x| <= 1},  c(s,t) = s / (2 B(1/s, t+1)),}
  which can be passed as \code{kernel} to every function of the package
  (bandwidth selectors, \code{\link{dkde}}, \code{\link{kernel.fun}},
  \code{\link{kernel.conv}}).
}
\usage{
kernel.pow(s = 2, t = 2)
}
\arguments{
  \item{s}{single positive number: shape exponent \eqn{s > 0}.}
  \item{t}{single non-negative number: power \eqn{t \ge 0}.}
}
\details{
  The kernel can equally be given directly as the string \code{"pow(s,t)"},
  e.g. \code{kernel = "pow(2,3)"}; \code{"pow"} alone means \code{"pow(2,2)"}.
  The family contains several classical kernels:
  \tabular{ll}{
    \code{"pow(s,0)"} \tab uniform \cr
    \code{"pow(1,1)"} \tab triangular \cr
    \code{"pow(2,1)"} \tab Epanechnikov \cr
    \code{"pow(2,2)"} \tab biweight \cr
    \code{"pow(2,3)"} \tab triweight \cr
    \code{"pow(3,3)"} \tab tricube \cr
  }
  The constants used by the bandwidth selectors are exact:
  \eqn{\mu_k(K) = B((k+1)/s,\,t+1)/B(1/s,\,t+1)} for even \eqn{k} and
  \eqn{R(K) = s\,B(1/s,\,2t+1) / (2\,B(1/s,\,t+1)^2)}.  For \eqn{x > 0},
  \eqn{K^{(r)}(x)} is a finite sum of terms
  \eqn{a\,x^{p}(1-x^s)^{q}}: an exact binomial expansion when \eqn{t} is an
  integer (\eqn{t \le 10}), a recurrence otherwise; derivatives are available
  up to order 16.

  \eqn{R(K^{(r)})} is finite only when \eqn{2r < 2t+1} and, unless \eqn{s} is
  an even integer, \eqn{2r < 2s+1} (otherwise \eqn{K^{(r)}} is not square integrable
  at \eqn{|x| = 1} or at \eqn{0}).  Bandwidth selectors return \code{NA} with a
  warning for such derivative orders, as for the other finite-support kernels.

  \bold{Maximum derivative order.} The regularity of \eqn{K_{s,t}} is
  \eqn{q = \min(\lceil t + 1 \rceil - 1, \lceil s + 1 \rceil - 1)}{q = min(ceiling(t + 1) - 1, ceiling(s + 1) - 1)},
  the term in \eqn{s} being dropped when \eqn{s} is an even integer
  (\eqn{q = t} for an integer \eqn{t}): for \eqn{m \le q} the pointwise
  derivative \eqn{K^{(m)}} is the derivative of \eqn{K}. A criterion that
  evaluates \eqn{K^{(m)}} needs \eqn{m \le q}: \eqn{r \le q} for
  \code{\link{dkde}} and \code{\link{h.amise}}, \eqn{2r \le q} for
  \code{\link{h.ucv}} and \code{\link{h.tcv}}, \eqn{2r + 2 \le q} for
  \code{\link{h.mcv}}, \eqn{r + 2 \le q} for BCV1 and \eqn{2r + 4 \le q} for
  BCV2 and \code{\link{h.ccv}}, with \eqn{K^{(r)}} square integrable; the
  selectors are capped at \eqn{r = 6} and \code{dkde} at \eqn{r = 16}. For
  example \code{pow(2,4)}: \eqn{q = 4} and 4, 4, 2, 1, 2, 0;
  \code{pow(2,3)}: \eqn{q = 3} and 3, 3, 1, 0, 1, not available. See
  \code{\link{kedd-package}}, section \sQuote{Maximum derivative order}.
}
\value{
  A character string \code{"pow(s,t)"}.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz},
  Amel Henider \email{amel.henider@univ-tam.dz} and
  Iago Giné-Vázquez \email{iago.gin-vaz@protonmail.com}}
\seealso{
  \code{\link{kernel.fun}}, \code{\link{kernel.conv}}, \code{\link{h.ucv}},
  \code{\link{dkde}}.
}
\examples{
kernel.pow(2, 3)                 # "pow(2,3)"

x <- rnorm(200)
h.ucv(x, kernel = kernel.pow(2, 2))   # same bandwidth as kernel = "biweight"
h.ucv(x, kernel = "pow(1.5,2.5)")

plot(kernel.fun(kernel = kernel.pow(1.5, 2.5), deriv.order = 1L))
}
\keyword{kernel}
\keyword{nonparametric}
