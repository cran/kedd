\name{kedd-package}
\alias{kedd-package}
\alias{kedd}
\docType{package}
\title{Kernel Estimator and Bandwidth Selection for Density and Its Derivatives}
\description{
  \if{html}{\figure{logo.png}{options: style='float: right' alt='kedd logo' width='120'}}

  Kernel smoothing of a univariate density \eqn{f} and of its \eqn{r}-th
  derivative \eqn{f^{(r)}}:
  \deqn{\hat{f}^{(r)}_h(x) = \frac{1}{n h^{r+1}} \sum_{i=1}^{n}
        K^{(r)}\!\left(\frac{x - X_i}{h}\right),}{%
        f^(r)(x) = 1/(n h^(r+1)) sum_i K^(r)((x - X_i)/h),}
  with S4 classes, \pkg{ggplot2} graphics and Rcpp/OpenMP data-driven
  bandwidth selectors.
}
\details{
  \bold{Kernels.} Every function accepts \code{kernel} as one of
  \code{"gaussian"}, \code{"epanechnikov"}, \code{"uniform"},
  \code{"triangular"}, \code{"biweight"}, \code{"triweight"},
  \code{"tricube"}, \code{"cosine"}, \code{"silverman"}, \code{"logistic"},
  or a member \code{"pow(s,t)"} of the power family
  \eqn{K(x)=c_{s,t}(1-|x|^s)^t\mathbf{1}_{|x|\le 1}} (see
  \code{\link{kernel.pow}}).

  \bold{Maximum derivative order} \eqn{r} for each kernel (see the section
  below).

  \bold{Bandwidth selectors} (S4 class \code{\linkS4class{bandwidth}}):
  \tabular{ll}{
    \code{\link{h.amise}} \tab asymptotic MISE (normal reference) \cr
    \code{\link{h.ucv}}   \tab unbiased (least-squares) cross-validation \cr
    \code{\link{h.bcv}}   \tab biased cross-validation (BCV1, BCV2) \cr
    \code{\link{h.ccv}}   \tab complete cross-validation \cr
    \code{\link{h.mcv}}   \tab modified cross-validation \cr
    \code{\link{h.tcv}}   \tab trimmed cross-validation \cr
    \code{\link{h.scv}}   \tab smoothed cross-validation (Gaussian) \cr
    \code{\link{h.pcv}}   \tab partitioned cross-validation \cr
    \code{\link{h.mlcv}}  \tab maximum-likelihood cross-validation \cr
  }

  \bold{Estimation and kernels}: \code{\link{dkde}} (S4 class
  \code{dkde}), \code{\link{kernel.fun}} (\eqn{K^{(r)}}, class \code{kfun}),
  \code{\link{kernel.conv}} (\eqn{K^{(r)} * K^{(r)}}, class \code{kconv}).
  All classes have \code{show()} and \pkg{ggplot2}-based \code{plot()} methods;
  \code{lines()} on a \code{bandwidth} object returns a layer to add with
  \code{+}.

  \bold{Parallelism}: the \eqn{O(n^2)} criteria run in C++ with OpenMP. The
  \code{nthreads} argument sets the threads of each call: \code{0} (default) =
  all processors, \code{k > 0} = \code{k} threads, capped by
  \code{OMP_THREAD_LIMIT}; it does not change results nor later calls.
  \code{\link{Cpp_omp_threads}(nthreads)} reports the threads actually used.

  \bold{Data}: five samples of size 200 from Marron and Wand (1992) normal
  mixtures, \code{kurtotic}, \code{outlier}, \code{bimodal}, \code{trimodal}
  and \code{claw} (see \code{\link{kedd-data}}).

  \bold{Source code and bug reports}:
  \url{https://github.com/ac-guidoum/kedd} and
  \url{https://github.com/ac-guidoum/kedd/issues}.

  \bold{Documentation}: a worked introduction is given in
  \code{vignette("kedd", package = "kedd")}, and demonstrations in
  \code{demo("kedd")}, \code{demo("kernels")} and \code{demo("bandwidths")}.
}
\section{Maximum derivative order}{
  The regularity \eqn{q} of a kernel is the largest \eqn{m} such that the
  pointwise derivative \eqn{K^{(m)}} is the derivative of \eqn{K} in the sense
  of distributions (\eqn{K^{(m-1)}} absolutely continuous). For \eqn{m > q},
  \eqn{K^{(m)}} misses the Dirac masses at the jumps of \eqn{K^{(q)}}, and a
  criterion that evaluates it at the pairwise differences \eqn{X_i - X_j} no
  longer estimates its target. Each criterion therefore restricts \eqn{r}:
  \tabular{ll}{
    \code{\link{dkde}}, \code{\link{h.amise}} \tab \eqn{r \le q} \cr
    \code{\link{h.ucv}}, \code{\link{h.tcv}} (\eqn{K^{(2r)}}) \tab \eqn{2r \le q} \cr
    \code{\link{h.mcv}} (\eqn{K^{(2r+2)}}) \tab \eqn{2r + 2 \le q} \cr
    \code{\link{h.bcv}}, BCV1 (convolution of \eqn{K^{(r+2)}}) \tab \eqn{r + 2 \le q} \cr
    \code{\link{h.bcv}}, BCV2, and \code{\link{h.ccv}} (\eqn{K^{(2r+4)}}) \tab \eqn{2r + 4 \le q} \cr
  }
  In addition \eqn{K^{(r)}} (\eqn{K^{(r+2)}} for BCV1) must be square
  integrable, and the selectors are capped at \eqn{r = 8} for the logistic
  kernel and \eqn{r = 6} for \code{pow(s,t)}, the derivative orders
  implemented being 20 and 16. Above the limit the selectors return
  \code{h = NA} with a warning, and \code{\link{dkde}} gives a warning.
  "any" means no limit and "--" means not available, even at \eqn{r = 0}.
  \tabular{lccccccc}{
    \bold{kernel} \tab \eqn{q} \tab \bold{dkde} \tab \bold{AMISE} \tab \bold{UCV, TCV} \tab \bold{MCV} \tab \bold{BCV1} \tab \bold{BCV2, CCV} \cr
    \code{gaussian}     \tab \eqn{\infty}{Inf} \tab any \tab any \tab any \tab any \tab any \tab any \cr
    \code{logistic}     \tab \eqn{\infty}{Inf} \tab 20  \tab 8   \tab 8   \tab 8   \tab 8   \tab 8   \cr
    \code{silverman}    \tab 3 \tab 3 \tab (*) \tab 1 \tab (*) \tab (*) \tab (*) \cr
    \code{triweight}    \tab 3 \tab 3 \tab 3   \tab 1 \tab 0  \tab 1  \tab -- \cr
    \code{tricube}      \tab 3 \tab 3 \tab 3   \tab 1 \tab 0  \tab 1  \tab -- \cr
    \code{biweight}     \tab 2 \tab 2 \tab 2   \tab 1 \tab 0  \tab 0  \tab -- \cr
    \code{epanechnikov} \tab 1 \tab 1 \tab 1   \tab 0 \tab -- \tab -- \tab -- \cr
    \code{triangular}   \tab 1 \tab 1 \tab 1   \tab 0 \tab -- \tab -- \tab -- \cr
    \code{cosine}       \tab 1 \tab 1 \tab 1   \tab 0 \tab -- \tab -- \tab -- \cr
    \code{uniform}      \tab 0 \tab 0 \tab 0   \tab 0 \tab -- \tab -- \tab -- \cr
  }
  (*) The Silverman kernel is of order 4 (\eqn{\mu_2(K) = 0}{mu2(K) = 0}):
  \code{\link{h.amise}} returns the value of the Gaussian kernel with a
  warning, and the search interval of the selectors uses it too. BCV1, BCV2,
  MCV and CCV, built on the bias expansion of a second-order kernel
  (\eqn{\mu_2(K)}{mu2(K)}, \eqn{\mu_4(K)}{mu4(K)}), are not defined for it:
  with \eqn{\mu_2(K) = 0}{mu2(K) = 0} the bias term of BCV vanishes and the
  criterion decreases up to the end of the search interval.

  For \code{pow(s,t)}, the factor \eqn{(1 - |x|)^t} at \eqn{|x| = 1} gives
  \eqn{q = \lceil t + 1 \rceil - 1}{q = ceiling(t + 1) - 1} (\eqn{q = t} for an
  integer \eqn{t}), and \eqn{|x|^s} at 0 gives
  \eqn{q = \lceil s + 1 \rceil - 1}{q = ceiling(s + 1) - 1} unless \eqn{s} is an
  even integer; \eqn{q} is the smaller of the two. \eqn{K^{(m)}} is square
  integrable when \eqn{m < t + 1/2} and, unless \eqn{s} is an even integer,
  \eqn{m < s + 1/2}. For example \code{pow(2,4)}: \eqn{q = 4} and the
  limits 4, 4, 2, 1, 2, 0; \code{pow(2,8)}: 8, 6, 4, 3, 6, 2;
  \code{pow(1.5,2.5)}: \eqn{q = 2} and 1, 1, 1, 0, --, --.
  \code{\link{h.scv}} (Gaussian kernel) has no limit, \code{\link{h.mlcv}}
  is for \eqn{r = 0} only, and \code{\link{h.pcv}} has the limits of the
  score computed in each subsample.
}
\author{
  Arsalane Chouaib Guidoum \email{acguidoum@univ-tam.dz} (author, maintainer),
  Amel Henider \email{amel.henider@univ-tam.dz} (author),
  Iago Giné-Vázquez \email{iago.gin-vaz@protonmail.com} (contributor).
}
\references{
  Guidoum, A. C. (2020). Kernel estimator and bandwidth selection for density
  and its derivatives: the kedd package. \emph{arXiv:2012.06102}.

  Marron, J. S. and Wand, M. P. (1992). Exact mean integrated squared error.
  \emph{The Annals of Statistics}, \bold{20}, 712--736.

  Wand, M. P. and Jones, M. C. (1995). \emph{Kernel Smoothing}.
  Chapman and Hall, London.
}
\seealso{
  \code{\link{dkde}}, \code{\link{kernel.fun}}, \code{\link{kernel.conv}},
  \code{\link{kernel.pow}}, \code{\link{h.ucv}}, \code{\link{h.scv}}, \code{\link{h.pcv}},
  \code{\linkS4class{bandwidth}}, \code{\link{kedd-data}},
  \code{vignette("kedd")}, \code{citation("kedd")}.
}
\examples{
x <- rnorm(150)
bw <- h.ucv(x, kernel = "biweight")
bw
plot(dkde(data = x, h = bw@h, kernel = "biweight"))

## logistic and power kernels
h.bcv(x, whichbcv = 2, kernel = "logistic")
h.bcv(x, whichbcv = 2, kernel = kernel.pow(2, 4))
}
\keyword{package}
