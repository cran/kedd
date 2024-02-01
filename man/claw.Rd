\name{Claw, Bimodal, Kurtotic, Outlier, Trimodal}
\alias{claw}
\alias{bimodal}
\alias{kurtotic}
\alias{outlier}
\alias{trimodal}
\docType{data}
\title{
Datasets
}
\description{
  A random sample of size 200 from the claw, bimodal, kurtotic, outlier and trimodal Gaussian density.
}
\usage{
data(claw)
data(bimodal)
data(kurtotic)
data(outlier)
data(trimodal)
}
\format{
  Numeric vector with length 200.
}
\details{
   Generate 200 random numbers, distributed according to a normal mixture, using 
   \code{\link[nor1mix]{rnorMix}} in package \CRANpkg{nor1mix}.
  \preformatted{
  ## Claw density
  claw <- rnorMix(n=200, MW.nm10)
  plot(MW.nm10)
  
  ## Bimodal density
  bimodal <- rnorMix(n=200, MW.nm7)
  plot( MW.nm7)
  
  ## Kurtotic density
  kurtotic <- rnorMix(n=200, MW.nm4)
  plot(MW.nm4)
  
  ## Outlier density
  outlier <- rnorMix(n=200, MW.nm5)
  plot( MW.nm5)
  
  ## Trimodal density
  trimodal <- rnorMix(n=200, MW.nm9)
  plot(MW.nm9)
  }   
}
\source{
  Randomly generated a normal mixture with the function \code{\link[nor1mix]{rnorMix}} in package \CRANpkg{nor1mix}.
}
\references{
  Martin, M. (2013). 
  \CRANpkg{nor1mix}: Normal (1-d) mixture models (S3 classes and methods). 
  \emph{\R package version 1.1-4}.
}
\keyword{datasets}
