\name{R1magic-package}
\alias{R1magic-package}
\alias{R1magic}
\docType{package}
\title{
  Compressive Sampling: Sparse signal recovery utilities
}
\description{
  Provides miminization routines and related utilities for compressive sampling. l-1, l-2 and TV (1-D) minimization, DFT basis matrix, sparse signal generator, mutual cumulative coherence and examples.
}
\details{
\tabular{ll}{
Package: \tab R1magic\cr
Type: \tab Package\cr
Version: \tab 0.2 \cr
Date: \tab 2013-04-09\cr
License: \tab GPL (>= 3) \cr
LazyLoad: \tab yes\cr
}
}
\author{
Mehmet Suzen
Maintainer: Mehmet Suzen  <mehmet.suzen@physics.org>
}
\references{
 Emmanuel Candes, Justin Romberg, and Terence Tao, Robust uncertainty principles: Exact signal reconstruction from highly incomplete frequency information. (IEEE Trans. on Information Theory, 52(2) pp. 489 - 509, February 2006)

Emmanuel Candes and Justin Romberg, Quantitative robust uncertainty principles and optimally sparse decompositions. (Foundations of Comput. Math., 6(2), pp. 227 - 254, April 2006)

David Donoho, Compressed sensing. (IEEE Trans. on Information Theory, 52(4), pp. 1289 - 1306, April 2006) 

}
\keyword{ package }
\examples{
 CompareL1_L2_TV1(100,10,0.1);
}
