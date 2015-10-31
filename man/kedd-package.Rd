\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\newcommand{\HR}{\href{http://www.usthb.dz/en/}}

\name{kedd-package}
\alias{kedd-package}
\alias{kedd}
\docType{package}
\title{
Kernel Estimator and Bandwidth Selection for Density and Its Derivatives
}
\description{
Smoothing techniques and computing bandwidth selectors of the 
r'th derivative of a probability density for one-dimensional data.
}
\details{
\tabular{ll}{
Package: \tab kedd\cr
Type: \tab Package\cr
Version: \tab 1.0.3\cr
Date: \tab 2015-10-30\cr
License: \tab GPL (>= 2) \cr
}
  There are four main types of functions in this package:
  \enumerate{
    \item Compute the derivatives and convolutions of a kernel function (1-d). 
    \item Compute the kernel estimators for density and its derivatives (1-d). 
    \item Computing the bandwidth selectors (1-d).
    \item Displaying kernel estimators.
  }
}
\section{Main Features}{
\bold{Convolutions and derivatives in kernel function:}\cr\cr
  In non-parametric statistics, a kernel is a weighting function used in non-parametric estimation techniques. 
  The kernels functions \eqn{K(x)}{K(x)} are used in derivatives of kernel density estimator to estimate 
  \eqn{\hat{f}^{(r)}_{h}(x)}{hat(f)(x;r)}, satisfying the following three requirements:
  \enumerate{
  \item \eqn{\int_{R} K(x) dx = 1}{ int K(x) dx = 1}
  \item \eqn{\int_{R} xK(x) dx = 0}{ int x K(x) dx = 0}
  \item \eqn{\mu_{2}(K) = \int_{R}x^{2} K(x) dx < \infty}{mu(K(x)) = int x^2 K(x) dx < inf}
  }
  Several types of kernel functions \eqn{K(x)} are commonly used in this package: Gaussian, Epanechnikov, Uniform (rectangular), Triangular, 
  Triweight, Tricube, Biweight (quartic), Cosine.\cr
  
  The function \code{\link{kernel.fun}} for kernel derivative \eqn{K^{(r)}(x)}{K(x;r)} and \code{\link{kernel.conv}} for 
  kernel convolution \eqn{K^{(r)}\ast K^{(r)} (x)}{K(x;r) * K(x;r)}, where the write formally:
  \deqn{K^{(r)}(x) = \frac{d^{r}}{d x^{r}} K(x)}{K(x;r) = d^r/d x^r (K(x))}  
  \deqn{K^{(r)} \ast K^{(r)} (x) = \int_{-\infty}^{+\infty} K^{(r)}(y)K^{(r)}(x-y)dy}{K(x;r) * K(x;r) = int K(y;r) K(x-y;r) dy}
  for \eqn{r = 0, 1, 2, \dots}\cr
  
\bold{Estimators of r'th derivative of a density function:}\cr\cr
  \dfn{A natural estimator of the r'th derivative of a density function \eqn{f(x)} is:   
  \deqn{\hat{f}^{(r)}_{h}(x)= \frac{d^{r}}{d x^{r}} \frac{1}{nh} \sum_{i=1}^{n} K\left(\frac{x-X_{i}}{h}\right) =
                          \frac{1}{nh^{r+1}}\sum_{i=1}^{n} K^{(r)}\left(\frac{x-X_{i}}{h}\right)}{hat(f)(x;r) = n^-1 h^-(r+1) Sum ( K(x-X(i)/h ;r) ,i = 1...n)}
  Here, \eqn{X_{1}, X_{2}, \dots,X_{n}}{X(1), X(2),...,X(n)} is an i.i.d, sample of size \eqn{n} from the distribution with density 
  \eqn{f(x)}, \eqn{K(x)} is the kernel function which we take to be a symmetric probability density with 
  at least \eqn{r} non zero derivatives when estimating \eqn{f^{(r)}(x)}{f(x;r)}, and \eqn{h} is the bandwidth,
  this parameter is very important that controls the degree of smoothing applied to the data.}\cr
  
  The case \eqn{(r=0)} is the standard kernel density estimator (e.g. Silverman 1986, Wolfgang 1991, Scott 1992, 
  Wand and Jones 1995, Jeffrey 1996, Bowman and Azzalini 1997, Alexandre 2009), properties of such derivative 
  estimators are well known e.g. Sheather and Jones (1991), Jones and Kappenman (1991), Wolfgang (1991). For 
  the case \eqn{(r > 0)}, is derivative of kernel density estimator (e.g. Bhattacharya 1967, Schuster 1969, Alekseev 1972, 
  Wolfgang et all 1990, Jones 1992, Stoker 1993) and for applications which require the estimation of density derivatives can 
  be found in Singh (1977).\cr
  
  For r'th derivatives of kernel density estimator one-dimensional, the main function is \code{\link{dkde}}. For display, 
  its plot method calls \code{\link{plot.dkde}}, and if to add a plot using \code{\link{lines.dkde}}.
  \preformatted{
  R> data(trimodal)
  R> dkde(x = trimodal, deriv.order = 0, kernel = "gaussian")
   
    Data: trimodal (200 obs.);      Kernel: gaussian
    Derivative order: 0;    Bandwidth 'h' = 0.1007
          eval.points           est.fx         
    Min.   :-2.91274   Min.   :0.0000066  
    1st Qu.:-1.46519   1st Qu.:0.0669750  
    Median :-0.01765   Median :0.1682045  
    Mean   :-0.01765   Mean   :0.1723692  
    3rd Qu.: 1.42989   3rd Qu.:0.2484626  
    Max.   : 2.87743   Max.   :0.4157340 
   
  R> dkde(x = trimodal, deriv.order = 1, kernel = "gaussian")
  
    Data: trimodal (200 obs.);      Kernel: gaussian
    Derivative order: 1;    Bandwidth 'h' = 0.09094
          eval.points           est.fx         
    Min.   :-2.87358   Min.   :-1.740447  
    1st Qu.:-1.44562   1st Qu.:-0.343952  
    Median :-0.01765   Median : 0.009057  
    Mean   :-0.01765   Mean   : 0.000000  
    3rd Qu.: 1.41031   3rd Qu.: 0.415343  
    Max.   : 2.83828   Max.   : 1.256891  
  }

\bold{Bandwidth selectors:}\cr\cr
  The most important factor in the r'th derivative kernel density estimate is a choice of the bandwidth 
  \eqn{h}{h} for one-dimensional observations. Because of its role in controlling both the amount and 
  the direction of smoothing, this choice is particularly important. We present the popular bandwidth 
  selection (for more details see references) methods in this package:
  \itemize{
    \item Optimal Bandwidth (AMISE); with \code{deriv.order >= 0}, name of this function is \code{\link{h.amise}}.\cr 
	For display, its plot method calls \code{\link{plot.h.amise}}, and to add a plot used \code{\link{lines.h.amise}}.
	\item Maximum-likelihood cross-validation (MLCV); with \code{deriv.order = 0}, name of this function is \code{\link{h.mlcv}}.\cr
	For display, its plot method calls \code{\link{plot.h.mlcv}}, and to add a plot used \code{\link{lines.h.mlcv}}. 
    \item Unbiased cross validation (UCV); with \code{deriv.order >= 0}, name of this function is \code{\link{h.ucv}}.\cr
	For display, its plot method calls \code{\link{plot.h.ucv}}, and to add a plot used \code{\link{lines.h.ucv}}.
    \item Biased cross validation (BCV); with \code{deriv.order >= 0}, name of this function is \code{\link{h.bcv}}.\cr
	For display, its plot method calls \code{\link{plot.h.bcv}}, and to add a plot used \code{\link{lines.h.bcv}}.
	\item Complete cross-validation (CCV); with \code{deriv.order >= 0}, name of this function is \code{\link{h.ccv}}.\cr
	For display, its plot method calls \code{\link{plot.h.ccv}}, and to add a plot used \code{\link{lines.h.ccv}}.
	\item Modified cross-validation (MCV); with \code{deriv.order >= 0}, name of this function is \code{\link{h.mcv}}.\cr
	For display, its plot method calls \code{\link{plot.h.mcv}}, and to add a plot used \code{\link{lines.h.mcv}}.
	\item Trimmed cross-validation (TCV); with \code{deriv.order >= 0}, name of this function is \code{\link{h.tcv}}.\cr
	For display, its plot method calls \code{\link{plot.h.tcv}}, and to add a plot used \code{\link{lines.h.tcv}}.
  }
  \preformatted{
  R> data(trimodal)
  R> h.bcv(x = trimodal, whichbcv = 1, deriv.order = 0, kernel = "gaussian")
  
    Call:           Biased Cross-Validation 1
    Derivative order = 0
    Data: trimodal (200 obs.);      Kernel: gaussian
    Min BCV = 0.004511636;  Bandwidth 'h' = 0.4357812 
	
  R> h.ccv(x = trimodal, deriv.order = 1, kernel = "gaussian")	
  
    Call:           Complete Cross-Validation
    Derivative order = 1 
    Data: trimodal (200 obs.);      Kernel: gaussian
    Min CCV = 0.01985078;   Bandwidth 'h' = 0.5828336
	
  R> h.tcv(x = trimodal, deriv.order = 2, kernel = "gaussian")
  
    Call:           Trimmed Cross-Validation
    Derivative order = 2
    Data: trimodal (200 obs.);      Kernel: gaussian
    Min TCV = -295.563;     Bandwidth 'h' = 0.08908582
	
  R> h.ucv(x = trimodal, deriv.order = 3, kernel = "gaussian")

    Call:           Unbiased Cross-Validation
    Derivative order = 3
    Data: trimodal (200 obs.);      Kernel: gaussian
    Min UCV = -63165.18;    Bandwidth 'h' = 0.1067236  
  }

For an overview of this package, see \code{vignette("kedd")}.  
}
\section{Requirements}{
\R version >= 2.15.0
}
\author{
Arsalane Chouaib Guidoum \email{acguidoum@usthb.dz} (Dept. Probability and Statistics, \HR{USTHB}, Algeria).\cr 
Please send comments, error reports, etc. to the author via the addresses email mentioned above.
}
\section{Licence}{
This package and its documentation are usable under the terms of the "GNU
General Public License", a copy of which is distributed with the package.
}
\references{
  Alekseev, V. G. (1972).
  Estimation of a probability density function and its derivatives.
  \emph{Mathematical notes of the Academy of Sciences of the USSR}. \bold{12}(5), 808--811.
  
  Alexandre, B. T. (2009).
  \emph{Introduction to Nonparametric Estimation}.
  Springer-Verlag, New York.
  
  Bowman, A. W. (1984). 
  An alternative method of cross-validation for the smoothing 
  of kernel density estimates. 
  \emph{Biometrika}, \bold{71}, 353--360.
  
  Bowman, A. W. and Azzalini, A. (1997). 
  \emph{Applied Smoothing Techniques for
  Data Analysis: the Kernel Approach with 
  S-Plus Illustrations}.
  Oxford University Press, Oxford.
  
  Bowman, A.W. and Azzalini, A. (2003).
  Computational aspects of nonparametric smoothing
  with illustrations from the \CRANpkg{sm} library.
  \emph{Computational Statistics and Data Analysis}, \bold{42}, 545--560.
  
  Bowman, A.W. and Azzalini, A. (2013).
  \CRANpkg{sm}: Smoothing methods for nonparametric 
  regression and density estimation. 
  \emph{\R package version 2.2-5.3}. Ported to \R by B. D. Ripley.
  
  Bhattacharya, P. K. (1967).
  Estimation of a probability density function and Its derivatives.
  \emph{Sankhya: The Indian Journal of Statistics, Series A}, \bold{29}, 373--382. 
  
  Duin, R. P. W. (1976).
  On the choice of smoothing parameters of Parzen estimators of probability density functions. 
  \emph{IEEE Transactions on Computers}, \bold{C-25}, 1175--1179.
  
  Feluch, W. and Koronacki, J. (1992).
  A note on modified cross-validation in density estimation.
  \emph{Computational Statistics and Data Analysis}, \bold{13}, 143--151.
  
  George, R. T. (1990).
  The maximal smoothing principle in density estimation. 
  \emph{Journal of the American Statistical Association}, \bold{85}, 470--477.
  
  George, R. T. and Scott, D. W. (1985).
  Oversmoothed nonparametric density estimates. 
  \emph{Journal of the American Statistical Association}, \bold{80}, 209--214.
  
  Habbema, J. D. F., Hermans, J., and Van den Broek, K. (1974)
  A stepwise discrimination analysis program using density estimation. 
  \emph{Compstat 1974: Proceedings in Computational Statistics}. Physica Verlag, Vienna.
  
  Heidenreich, N. B., Schindler, A. and Sperlich, S. (2013).
  Bandwidth selection for kernel density estimation: 
  a review of fully automatic selectors.
  \emph{Advances in Statistical Analysis}.
  
  Jeffrey, S. S. (1996).
  \emph{Smoothing Methods in Statistics}.
  Springer-Verlag, New York.
  
  Jones, M. C. (1992).
  Differences and derivatives in kernel estimation. 
  \emph{Metrika}, \bold{39}, 335--340.
  
  Jones, M. C., Marron, J. S. and Sheather,S. J. (1996).
  A brief survey of bandwidth selection for density estimation. 
  \emph{Journal of the American Statistical Association}, \bold{91}, 401--407.
  
  Jones, M. C. and Kappenman, R. F. (1991). 
  On a class of kernel density estimate bandwidth selectors. 
  \emph{Scandinavian Journal of Statistics}, \bold{19}, 337--349.

  Loader, C. (1999). 
  \emph{Local Regression and Likelihood}. 
  Springer, New York.

  Olver, F. W., Lozier, D. W., Boisvert, R. F. and Clark, C. W. (2010).
  \emph{NIST Handbook of Mathematical Functions}.
  Cambridge University Press, New York, USA.
  
  Peter, H. and Marron, J.S. (1987).
  Estimation of integrated squared density derivatives.
  \emph{Statistics and Probability Letters}, \bold{6}, 109--115.
  
  Peter, H. and Marron, J.S. (1991).
  Local minima in cross-validation functions.
  \emph{Journal of the Royal Statistical Society, Series B}, \bold{53}, 245--252.

  Radhey, S. S. (1987).
  MISE of kernel estimates of a density and its derivatives.
  \emph{Statistics and Probability Letters}, \bold{5}, 153--159.

  Rudemo, M. (1982). 
  Empirical choice of histograms and kernel density estimators. 
  \emph{Scandinavian Journal of Statistics}, \bold{9}, 65--78.
  
  Scott, D. W. (1992).
  \emph{Multivariate Density Estimation. Theory, Practice and Visualization}.
  New York: Wiley.
  
  Scott, D.W. and George, R. T. (1987).
  Biased and unbiased cross-validation in density estimation. 
  \emph{Journal of the American Statistical Association}, \bold{82}, 1131--1146.
  
  Schuster, E. F. (1969) 
  Estimation of a probability density function and its derivatives. 
  \emph{The Annals of Mathematical Statistics}, \bold{40} (4), 1187--1195.
  
  Sheather, S. J. (2004).
  Density estimation.
  \emph{Statistical Science}, \bold{19}, 588--597.
  
  Sheather, S. J. and Jones, M. C. (1991).
  A reliable data-based bandwidth selection method for 
  kernel density estimation.
  \emph{Journal of the Royal Statistical Society, Series B}, \bold{53}, 683--690.
  
  Silverman, B. W. (1986).
  \emph{Density Estimation for Statistics and Data Analysis}.
  Chapman & Hall/CRC. London.
  
  Singh, R. S. (1977).                            
  Applications of estimators of a density and 
  its derivatives to certain statistical problems.
  \emph{Journal of the Royal Statistical Society, Series B}, \bold{39}(3), 357--363.
  
  Stoker, T. M. (1993).
  Smoothing bias in density derivative estimation. 
  \emph{Journal of the American Statistical Association}, \bold{88}, 855--863.
  
  Stute, W. (1992).
  Modified cross validation in density estimation.
  \emph{Journal of Statistical Planning and Inference}, \bold{30}, 293--305.
  
  Tarn, D. (2007).
  \CRANpkg{ks}: Kernel density estimation and kernel discriminant
  analysis for multivariate data in \R.
  \emph{Journal of Statistical Software}, \bold{21}(7), 1--16. 
  
  Tristen, H. and Jeffrey, S. R. (2008).
  Nonparametric Econometrics: The \CRANpkg{np} Package.
  \emph{Journal of Statistical Software},\bold{27}(5).
  
  Venables, W. N. and Ripley, B. D. (2002).
  \emph{Modern Applied Statistics with S}.
  New York: Springer.
  
  Wand, M. P. and Jones, M. C. (1995).
  \emph{Kernel Smoothing}.
  Chapman and Hall, London.
  
  Wand, M.P. and Ripley, B. D. (2013). 
  \CRANpkg{KernSmooth}: Functions for Kernel Smoothing 
  for Wand and Jones (1995). 
  \emph{\R package version 2.23-10}. 
  
  Wolfgang, H. (1991).
  \emph{Smoothing Techniques}, 
  \emph{With Implementation in S}.
  Springer-Verlag, New York.
  
  Wolfgang, H., Marlene, M., Stefan, S. and Axel, W. (2004).
  \emph{Nonparametric and Semiparametric Models}.
  Springer-Verlag, Berlin Heidelberg.
  
  Wolfgang, H., Marron, J. S. and Wand, M. P. (1990).
  Bandwidth choice for density derivatives.
  \emph{Journal of the Royal Statistical Society, Series B}, 223--232.
}
\keyword{package}
\seealso{
\CRANpkg{ks}, \CRANpkg{KernSmooth}, \CRANpkg{sm}, \CRANpkg{np}, \CRANpkg{locfit}, \CRANpkg{feature}, \CRANpkg{GenKern}.
}

