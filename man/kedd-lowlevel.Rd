\name{kedd-lowlevel}
\alias{kedd-lowlevel}
\alias{Cpp_kernel_constants}
\alias{Cpp_hos}
\alias{Cpp_kernel_deriv}
\alias{Cpp_kernel_conv}
\alias{Cpp_h_ucv}
\alias{Cpp_h_bcv_1}
\alias{Cpp_h_bcv_2}
\alias{Cpp_h_ccv}
\alias{Cpp_h_mcv}
\alias{Cpp_h_tcv}
\alias{Cpp_h_scv}
\alias{Cpp_plot_cv}
\alias{Cpp_conv2conv}
\alias{Cpp_omp_threads}
\title{Low-Level C++ Routines}
\description{
  Compiled (Rcpp/OpenMP) routines used by the user-level functions.  They do
  not validate their arguments: \code{kernel} must be a canonical kernel name
  (as returned in the \code{kernel} slot of \pkg{kedd} objects, e.g.
  \code{"biweight"} or \code{"pow(2,3)"}) and \code{x} must be free of
  \code{NA}.  Most users should call \code{\link{h.ucv}},
  \code{\link{kernel.fun}}, \code{\link{dkde}}, ... instead.
}
\usage{
Cpp_kernel_constants(kernel, deriv_order)
Cpp_hos(kernel, deriv_order, n)
Cpp_kernel_deriv(kernel, u, deriv_order, nthreads = 0L)
Cpp_kernel_conv(kernel, u, deriv_order, nthreads = 0L)
Cpp_h_ucv(R_h, R_kernel, R_r, R_x, nthreads)
Cpp_h_bcv_1(R_h, R_kernel, R_r, R_x, nthreads)
Cpp_h_bcv_2(R_h, R_kernel, R_r, R_x, nthreads)
Cpp_h_ccv(R_h, R_kernel, R_r, R_x, nthreads)
Cpp_h_mcv(R_h, R_kernel, R_r, R_x, nthreads)
Cpp_h_tcv(R_h, R_kernel, R_r, R_x, nthreads)
Cpp_h_scv(R_h, R_g, R_kernel, R_r, R_x, nthreads)
Cpp_plot_cv(method, h_sexp, kernel, r, x, nthreads = 0L)
Cpp_conv2conv(t, deriv_order1, deriv_order2, kernel1, kernel2, nthreads)
Cpp_omp_threads(nthreads = 0L)
}
\arguments{
  \item{kernel, R_kernel, kernel1, kernel2}{canonical kernel name.}
  \item{deriv_order, R_r, r, deriv_order1, deriv_order2}{derivative order(s).}
  \item{n}{sample size.}
  \item{u}{numeric vector or matrix of evaluation points.}
  \item{R_h}{bandwidth at which the criterion is evaluated.}
  \item{R_g}{pilot bandwidth (SCV).}
  \item{R_x, x}{numeric data vector.}
  \item{nthreads}{number of OpenMP threads for this call: \code{0} = all
    processors, \code{k > 0} = \code{k} threads; both capped by the
    environment variable \code{OMP_THREAD_LIMIT}. The setting applies to the
    call only and never changes later calls.}
  \item{method}{one of \code{"UCV"}, \code{"BCV1"}, \code{"BCV2"},
    \code{"CCV"}, \code{"MCV"}, \code{"TCV"}, \code{"SCV"}.}
  \item{h_sexp}{numeric vector or matrix of bandwidths.}
  \item{t}{numeric vector or matrix of evaluation points.}
}
\value{
  \describe{
    \item{\code{Cpp_kernel_constants}}{named vector: \code{c} (normalising
      constant of a \code{"pow(s,t)"} kernel, \code{NA} otherwise),
      \code{mu2} \eqn{=\int x^2K}, \code{mu4} \eqn{=\int x^4K} and
      \code{R} \eqn{=\int (K^{(r)})^2}.}
    \item{\code{Cpp_hos}}{over-smoothing bandwidth for unit variance.}
    \item{\code{Cpp_kernel_deriv}, \code{Cpp_kernel_conv}}{\eqn{K^{(r)}(u)}
      and \eqn{(K^{(r)} * K^{(r)})(u)}, with the shape of \code{u}.}
    \item{\code{Cpp_h_*}}{value of the criterion at \code{R_h}.}
    \item{\code{Cpp_plot_cv}}{criterion \code{method} at each bandwidth,
      with the shape of \code{h_sexp}.}
    \item{\code{Cpp_omp_threads}}{number of threads a parallel region
      actually uses for this \code{nthreads} (1 when the package was built
      without OpenMP).}
    \item{\code{Cpp_conv2conv}}{\eqn{(K_1^{(r_1)} * K_1^{(r_1)}) *
      (K_2^{(r_2)} * K_2^{(r_2)})} at \code{t} by numerical integration.}
  }
}
\seealso{\code{\link{kedd-package}}}
\examples{
Cpp_kernel_constants("biweight", 0L)
Cpp_kernel_deriv("pow(2,3)", c(-0.5, 0, 0.5), 1L)
x <- rnorm(100)
Cpp_h_ucv(0.4, "gaussian", 0L, x, 1L)
Cpp_omp_threads(0L)   # threads used by nthreads = 0 (all cores)
Cpp_omp_threads(2L)
}
\keyword{internal}
