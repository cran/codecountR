% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BoxMullerGen.R
\name{BoxMullerGen}
\alias{BoxMullerGen}
\title{BoxMullerGen}
\usage{
BoxMullerGen(r, s)
}
\arguments{
\item{r}{a number}

\item{s}{a number}
}
\value{
a vector
}
\description{
BoxMullerGen
}
\examples{
#with runif
v=BoxMullerGen(runif(1), runif(1))
print(v)

#with congruGen
seed = 123456789
X=c()
for(i in 1: 2) {
Z=congruGen(seed)
seed=Z$seedUpdate
X=append(X, Z$aleaNum)
}
#print(X)

N=BoxMullerGen(X[1], X[2])
print(N[1])
print(N[2])

}
