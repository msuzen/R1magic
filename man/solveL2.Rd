\name{solveL2}
\alias{solveL2}
\title{
 l2  Penalized Nonlinear Minimization
}
\description{
  l2 Penalized Nonlinear Minimization
}
\usage{
 solveL2(phi,y,T,x0,lambda=0.1)
}
\arguments{
  \item{x0}{
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
    Penalty coefficient. Defaults 0.1
}
}
\value{
  Returns nlm object.
}
\author{
 Mehmet Suzen
}
