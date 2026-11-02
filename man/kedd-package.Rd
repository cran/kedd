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
  Highest derivative order \eqn{r} (\code{deriv.order}) for each kernel.
  Column 2: \eqn{K^{(r)}} is not identically zero (\code{\link{kernel.fun}},
  \code{\link{kernel.conv}}, \code{\link{dkde}}); above it these functions
  return zero. Columns 3 to 5: the selector criterion is finite; above it the
  selectors return \code{h = NA} with a warning. "any" means no limit and "--"
  means not available, even at \eqn{r = 0}.
  \tabular{lcccc}{
    \bold{kernel} \tab \bold{kernel.fun, kernel.conv, dkde} \tab \bold{UCV, BCV, TCV, AMISE} \tab \bold{MCV} \tab \bold{CCV} \cr
    \code{gaussian}     \tab any \tab any \tab any \tab any \cr
    \code{cosine}       \tab any \tab 1   \tab 0   \tab --  \cr
    \code{silverman}    \tab any \tab any \tab any \tab any \cr
    \code{logistic}     \tab 20  \tab 8   \tab 8   \tab 8   \cr
    \code{epanechnikov} \tab 2   \tab 1   \tab 0   \tab --  \cr
    \code{uniform}      \tab 0   \tab 0   \tab --  \tab --  \cr
    \code{triangular}   \tab 1   \tab 0   \tab --  \tab --  \cr
    \code{biweight}     \tab 4   \tab 2   \tab 1   \tab 0   \cr
    \code{triweight}    \tab 6   \tab 3   \tab 2   \tab 1   \cr
    \code{tricube}      \tab 9   \tab 3   \tab 2   \tab 1   \cr
    \code{pow(s,t)}     \tab \eqn{\min(st, 16)}{min(st, 16)} \tab \eqn{\min(r^*, 6)}{min(r*, 6)} \tab \eqn{\min(r^*-1, 6)}{min(r*-1, 6)} \tab \eqn{\min(r^*-2, 6)}{min(r*-2, 6)} \cr
  }
  For \code{pow(s,t)}, \eqn{st} applies when \eqn{s} and \eqn{t} are integers
  (16 otherwise), and \eqn{r^*}{r*} is the largest integer
  \eqn{r < t + 1/2}, with \eqn{2r \le st}{2r <= st} when \eqn{s} and \eqn{t}
  are integers and \eqn{r < s + 1/2} when \eqn{s} is not an even integer.
  For example \code{pow(2,3)}: 6, 3, 2, 1; \code{pow(2,8)}: 16, 6, 6, 6;
  \code{pow(1.5,2.5)}: 16, 1, 0, --.
  \code{\link{h.scv}} (Gaussian kernel) has no limit and \code{\link{h.mlcv}}
  is for \eqn{r = 0} only.
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
  \code{\link{kernel.pow}}, \code{\link{h.ucv}}, \code{\link{h.scv}},
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
