\name{sparseSignal}
\alias{sparseSignal}
\title{
   Sparse digital signal Generator.
}
\description{
   Sparse digital signal Generator with given thresholds.
}
\usage{
sparseSignal(N, s, b = 1, delta = 1e-07, nlev = 0.05, slev = 0.9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
     Number of signal components, vector size.
}
  \item{s}{
    Number of spikes, significatn components
}
  \item{b}{
    Signal bandwidth, defaults 1.
}
  \item{delta}{
    Length of discrete distances among components, defaults 1e-7.
}
  \item{nlev}{
     Maximum value of insignificant component, relative to b, defaults to 0.05
}
  \item{slev}{
     Maximum value of significant component, relative to b, defaults to 0.9
}
}
\author{
Mehmet Suzen
}

