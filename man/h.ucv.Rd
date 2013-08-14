\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}


\name{h.ucv}
\alias{h.ucv}
\alias{h.ucv.default}
\alias{print.h.ucv}
\title{
Unbiased (Least-Squares) Cross-Validation for Bandwidth Selection
}
\description{
  The (S3) generic function \code{h.ucv} computes the unbiased 
  (least-squares) cross-validation bandwidth selector
  of r'th derivative of kernel density estimator one-dimensional.
  
}
\usage{
h.ucv(x, \dots)
\method{h.ucv}{default}(x, deriv.order = 0, lower = 0.1 * hos, upper = 2 * hos, 
         tol = 0.1 * lower, kernel = c("gaussian", "epanechnikov", "uniform", 
         "triangular", "triweight", "tricube", "biweight", "cosine"), \dots)
}
\arguments{
  \item{x}{vector of data values.}
  \item{deriv.order}{derivative order (scalar).}
  \item{lower, upper}{range over which to minimize. The default is
    almost always satisfactory. \code{hos} (Over-smoothing) is calculated internally
    from an \code{kernel}, see details.}
  \item{tol}{the convergence tolerance for \code{\link{optimize}}.}
  \item{kernel}{a character string giving the smoothing kernel to be used, with default
    \code{"gaussian"}.}
  \item{\dots}{further arguments for (non-default) methods.}
}
\details{
  \code{h.ucv} unbiased (least-squares) cross-validation implements for choosing the bandwidth \eqn{h} 
  of a r'th derivative kernel density estimator.\cr
  
  Rudemo (1982) and Bowman (1984) proposed a so-called unbiased (least-squares)  cross-validation 
  (UCV) in kernel density estimator. An adaptation of unbiased cross-validation is proposed by 
  Wolfgang et al. (1990) for bandwidth choice in the r'th derivative of kernel density estimator. 
  The essential idea of this methods, for the estimation of \eqn{f^{(r)}(x)}{f(x;r)} (\eqn{r} is derivative order), 
  is to use the bandwidth \eqn{h} which minimizes the function:
  \deqn{UCV(h;r) = \int \left(\hat{f}_{h}^{(r)}(x)\right)^{2} - 2n^{-1}(-1)^{r}\sum_{i=1}^{n} \hat{f}_{h,i}^{(2r)}(X_{i})}{UCV(h;r)= int (hat(f)(x;r))^2 - 2 n^(-1) (-1)^r sum(hat(f)(X_i;2r),i=1,...n)}
  The bandwidth minimizing this function is:
  \deqn{\hat{h}^{(r)}_{ucv} = \arg \min_{h^{(r)}} UCV(h;r)}{h(r) = argmin UCV(h;r)}
  for \eqn{r = 0, 1, 2, \dots}\cr
  where  
  \deqn{\int \left(\hat{f}_{h}^{(r)}(x)\right)^{2} = \frac{R\left(K^{(r)}\right)}{nh^{2r+1}} + \frac{(-1)^{r}}{n (n-1) h^{2r+1}} \sum_{i=1}^{n}\sum_{j=1;j \neq i}^{n} K^{(r)} \ast K^{(r)} \left(\frac{X_{j}-X_{i}}{h}\right)}{int (hat(f)(x;r))^2 dx = R(k(x;r))/n h^(2r+1) +  (-1)^r / n (n-1) h^(2r+1) sum(sum(K(.;r)*K(.;r)(x(j)-x(i)/h)), i=1...n, j=1...n, j != i)}
  and \eqn{K^{(r)} \ast K^{(r)} (x)}{K(x;r)*K(x;r)} is the convolution of the r'th derivative kernel function \eqn{K^{(r)}(x)}{K(x;r)}
  (see \code{\link{kernel.conv}} and \code{\link{kernel.fun}}).\cr
  The estimate \eqn{\hat{f}_{h,i}^{(2r)}(x)}{hat(f)(x;2r)} on the subset \eqn{\{X_{j}\}_{j \neq i}}{(X_j)_(j != i)}
  denoting the leave-one-out estimator, can be written:
  \deqn{\hat{f}_{h,i}^{(2r)}(X_{i}) = \frac{1}{(n-1) h^{2r+1}} \sum_{j \neq i} K^{(2r)} \left(\frac{X_{j}-X_{i}}{h}\right)}{hat(f)(X_i;2r) = 1 /(n-1) h^(2r+1) sum(K(x(j)-x(i)/h;2r), j != i)}
  The function \eqn{UCV(h;r)} is unbiased cross-validation in the sense that \eqn{E[UCV]=MISE[\hat{f}_{h}^{(r)}(x)]-R(f^{(r)}(x))}{E[UCV(h;r)] = MISE[hat(f)(x;r)]-R(f(x;r))}
  (see, Scott and George 1987). Can be simplified to give the computationally:
  \deqn{UCV(h;r) = \frac{R\left(K^{(r)}\right)}{nh^{2r+1}} + \frac{(-1)^{r}}{n (n-1) h^{2r+1}} \sum_{i=1}^{n}\sum_{j=1 ;j \neq i}^{n} \left(K^{(r)} \ast K^{(r)} -2K^{(2r)}\right) \left(\frac{X_{j}-X_{i}}{h}\right)}{UCV(h;r) = R(k(x;r))/n h^(2r+1) +  (-1)^r / n (n-1) h^(2r+1) sum(sum(K(.;r)*K(.;r) - 2 K(.;r))(x(j)-x(i)/h), i=1...n, j=1...n, j != i)}
  where \eqn{R\left(K^{(r)}\right) = \int_{R} K^{(r)}(x)^{2} dx}{R(K(x;r)) = int K(x;r)^2 dx}.\cr 
  
  The range over which to minimize is \code{hos} Oversmoothing bandwidth, the default is almost always 
  satisfactory. See George and Scott (1985), George (1990), Scott (1992, pp 165), Wand and Jones (1995, pp 61).    
}
\value{
  \item{x}{data points - same as input.}
  \item{data.name}{the deparsed name of the \code{x} argument.}
  \item{n}{the sample size after elimination of missing values.}
  \item{kernel}{name of kernel to use}
  \item{deriv.order}{the derivative order to use.}
  \item{h}{value of bandwidth parameter.}
  \item{min.ucv}{the minimal UCV value.}
}
\references{  
  Bowman, A. (1984). 
  An alternative method of cross-validation for the smoothing 
  of kernel density estimates. 
  \emph{Biometrika}, \bold{71}, 353--360.
  
  Jones, M. C. and Kappenman, R. F. (1991). 
  On a class of kernel density estimate bandwidth selectors. 
  \emph{Scandinavian Journal of Statistics}, \bold{19}, 337--349.
  
  Jones, M. C., Marron, J. S. and Sheather,S. J. (1996).
  A brief survey of bandwidth selection for density estimation. 
  \emph{Journal of the American Statistical Association}, \bold{91}, 401--407.
  
  Peter, H. and Marron, J.S. (1987).
  Estimation of integrated squared density derivatives.
  \emph{Statistics and Probability Letters}, \bold{6}, 109--115.
  
  Rudemo, M. (1982). 
  Empirical choice of histograms and kernel density estimators. 
  \emph{Scandinavian Journal of Statistics}, \bold{9}, 65--78. 

  Scott, D.W. and George, R. T. (1987).
  Biased and unbiased cross-validation in density estimation. 
  \emph{Journal of the American Statistical Association}, \bold{82}, 1131--1146.  
  
  Sheather, S. J. (2004).
  Density estimation.
  \emph{Statistical Science}, \bold{19}, 588--597.

  Tarn, D. (2007).
  \CRANpkg{ks}: Kernel density estimation and kernel discriminant
  analysis for multivariate data in \R.
  \emph{Journal of Statistical Software}, \bold{21}(7), 1--16. 
  
  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing}.
  Chapman and Hall, London.
  
  Wolfgang, H. (1991).
  \emph{Smoothing Techniques}, 
  \emph{With Implementation in S}.
  Springer-Verlag, New York. 

  Wolfgang, H., Marron, J. S. and Wand, M. P. (1990).
  Bandwidth choice for density derivatives.
  \emph{Journal of the Royal Statistical Society, Series B}, 223--232. 
}
\author{Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz}
}
\seealso{
\code{\link{plot.h.ucv}}, see \code{\link[stats]{bw.ucv}} in package \CRANpkg{stats} and 
\code{\link[MASS]{ucv}} in package \CRANpkg{MASS} for Gaussian kernel only if \code{deriv.order = 0},
\code{\link[ks]{hlscv}} in package \CRANpkg{ks} for Gaussian kernel only if \code{0 <= deriv.order <= 5}, 
\code{\link[locfit]{kdeb}} in package \CRANpkg{locfit} if \code{deriv.order = 0}. 
}
\examples{
## Derivative order = 0

h.ucv(kurtotic,deriv.order = 0)

## Derivative order = 1

h.ucv(kurtotic,deriv.order = 1)
}
\keyword{smooth}
\keyword{bandwidth selection}
\keyword{nonparametric}
