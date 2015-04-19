\name{R1magic-package}
\alias{R1magic-package}
\alias{R1magic}
\docType{package}
\title{
  Compressive Sampling: Sparse signal recovery utilities
}
\description{
Utilities for sparse signal recovery suitable for compressed sensing. L1, L2 and TV penalties, DFT basis matrix, simple sparse signal generator, mutual cumulative coherence between two matrices and examples, Lp complex norm, scaling back regression coefficients.
}
\details{
\tabular{ll}{
Package: \tab R1magic\cr
Type: \tab Package\cr
Version: \tab 0.3.2 \cr
Date: \tab 2014-04-19\cr
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
