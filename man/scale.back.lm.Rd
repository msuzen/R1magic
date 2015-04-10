\name{scale.back.lm}
\alias{scale.back.lm}
\title{
Transform back multiple regression coefficients to unscaled regression coefficients 
}
\description{
Transform back multiple regression coefficients to unscaled regression coefficients
Original question posed by Mark Seeto on the R mailing list.
}
\usage{
scale.back.lm(X, Y, betas.scaled)
}
\arguments{
  \item{X}{
    X, unscaled design matrix without the intercept, m by n matrix.
  }
  \item{Y}{
    Y, unscaled response, m by 1 matrix
  }
  \item{betas.scaled}{
   betas.scaled, coefficients of multiple regression, first term is the intercept, n by 1 matrix
  }
}
\value{
   Returns, coefficients of multiple regression, first term is the intercept, n by 1 matrix
}
\author{
Mehmet Suzen
}
