\name{objectiveL2}
\alias{objectiveL2}
\title{
  L-2 Penalized Objective Function
}
\description{
  L-2 Penalized Objective Function
}
\usage{
objectiveL2(x, T, phi, y, lambda)
}
\arguments{
  \item{x}{
    Initial value of the vector to be recovered. Sparse representation
      of the vector ( N x 1 matrix ) X=Tx, where X is the original vector
}
  \item{T}{
   sparsity bases ( N x N matrix )
}
  \item{phi}{
    Measurement matrix (M x N).
}
  \item{y}{
   Measurement vector (Mx1).
}
  \item{lambda}{
    Penalty coefficient.
}
}
\value{
   Returns a vector.
}
\references{
 Emmanuel Candes, Justin Romberg, and Terence Tao, Robust uncertainty principles: Exact signal reconstruction from highly incomplete frequency information. (IEEE Trans. on Information Theory, 52(2) pp. 489 - 509, February 2006) 
}
\author{
Mehmet Suzen
}
