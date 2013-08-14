\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{h.mlcv}
\alias{h.mlcv}
\alias{h.mlcv.default}
\alias{print.h.mlcv}
\title{
Maximum-Likelihood Cross-validation for Bandwidth Selection
}
\description{
  The (S3) generic function \code{h.mlcv} computes the maximum 
  likelihood cross-validation (Kullback-Leibler information) 
  bandwidth selector of a one-dimensional kernel density estimate.
}
\usage{
h.mlcv(x, \dots)
\method{h.mlcv}{default}(x, lower = 0.1, upper = 5, tol = 0.1 * lower, 
         kernel = c("gaussian", "epanechnikov", "uniform", "triangular", 
         "triweight", "tricube", "biweight", "cosine"), \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector of data values.}
  \item{lower, upper}{range over which to maximize. The default is
    almost always satisfactory.}
  \item{tol}{the convergence tolerance for \code{\link{optimize}}.}
  \item{kernel}{a character string giving the smoothing kernel to be used, with default
    \code{"gaussian"}.}
  \item{\dots}{further arguments for (non-default) methods.}
}
\details{
  \code{h.mlcv} maximum-likelihood cross-validation implements for choosing 
  the optimal bandwidth \eqn{h} of kernel density estimator.\cr
  
  This method was proposed by Habbema, Hermans, and Van den Broeck (1971) and by Duin (1976). The maximum-likelihood 
  cross-validation (MLCV) function is defined by:
  \deqn{MLCV(h) = n^{-1} \sum_{i=1}^{n} \log\left[\hat{f}_{h,i}(x)\right]}{MLCV(h) = n^-1 sum( log(hat(f(h))),i=1...n)}
  the estimate \eqn{\hat{f}_{h,i}(x)}{hat(f)(x)} on the subset \eqn{\{X_{j}\}_{j \neq i}}{(X_j)_(j != i)}
  denoting the leave-one-out estimator, can be written:
  \deqn{\hat{f}_{h,i}(X_{i}) = \frac{1}{(n-1) h} \sum_{j \neq i} K \left(\frac{X_{j}-X_{i}}{h}\right)}{hat(f)(X_i) = 1/(n-1) h sum(K(x(j)-x(i)/h), j != i)}
  Define that \eqn{h_{mlcv}}{h(mlcv)} as good which approaches the finite maximum of \eqn{MLCV(h)}{MLCV(h)}:
  \deqn{h_{mlcv} = \arg \max_{h} MLCV(h) = \arg \max_{h} \left(n^{-1} \sum_{i=1}^{n} \log\left[\sum_{j \neq i} K \left(\frac{X_{j}-X_{i}}{h}\right)\right]-\log[(n-1)h]\right)}{h(mlcv)= argmax MLCV(h) = argmax n^-1 sum(log(sum(K(x(j)-x(i)/h),j != i)),i=1...n) - log((n-1)h)}
}
\value{
  \item{x}{data points - same as input.}
  \item{data.name}{the deparsed name of the \code{x} argument.}
  \item{n}{the sample size after elimination of missing values.}
  \item{kernel}{name of kernel to use}
  \item{h}{value of bandwidth parameter.}
  \item{mlcv}{the maximal likelihood CV value.}
}
\references{
  Habbema, J. D. F., Hermans, J., and Van den Broek, K. (1974)
  A stepwise discrimination analysis program using density estimation. 
  \emph{Compstat 1974: Proceedings in Computational Statistics}. Physica Verlag, Vienna.

  Duin, R. P. W. (1976).
  On the choice of smoothing parameters of Parzen estimators of probability density functions. 
  \emph{IEEE Transactions on Computers}, \bold{C-25}, 1175--1179.
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{plot.h.mlcv}}, see \code{\link[locfit]{lcv}} in package \CRANpkg{locfit}.
}
\examples{
h.mlcv(bimodal)
h.mlcv(bimodal, kernel ="epanechnikov")
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
