\name{mutualCoherence}
\alias{mutualCoherence}
\title{
 Cumulative mutual coherence
}
\description{
 Generate vector of cumulative mutual coherence of a given matrix up to a given order. \\
 Mutual Cumulative Coherence of a Matrix A at order k is defined as 
 \eqn{ M(A, k) = max_{p} max_{p \ne q, q \in \Omega } \sum_{q} | <a_{p}, a_{q}> | / ( |a_{p}| |a_{q}|)}
}
\usage{
mutualCoherence(A, k)
}
\arguments{
  \item{A}{
   A matrix.
}
  \item{k}{
   Integer value determines number of columns or the order of mutual coherence function to .
}
}
\value{
Returns k-vector
}
\references{
 Compressed sensing in diffuse optical tomography \\ M. Suzen, A.Giannoula and T. Durduran, \\ Opt. Express 18, 23676-23690 (2010)  \\
 J. A. Tropp \\ Greed is good: algorithmic results for sparse approximation, \\IEEE Trans. Inf. Theory 50, 2231-2242 (2004)
}
\author{
Mehmet Suzen
}

\examples{
set.seed(42)
B <- matrix(rnorm(100), 10, 10) # Gaussian Random Matrix
mutualCoherence(B, 3) # mutual coherence up to order k
}
