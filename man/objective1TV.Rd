\name{objective1TV}
\alias{objective1TV}
\title{
  1-D Total Variation Penalized Objective Function
}
\description{
  1-D Total Variation Penalized Objective Function
}
\usage{
objective1TV(x, T, phi, y, lambda)
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
\author{
 Mehmet Suzen
}
