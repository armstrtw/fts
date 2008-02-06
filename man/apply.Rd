\name{apply}
\alias{row.apply}
\alias{column.apply}

\title{Apply Function}

\description {
  Apply a function to the rows or columns of an rseries object
}

\usage{
column.apply(x, FUN, ...)
row.apply(x, FUN, ...)
}
\arguments{
  \item{x}{ An Rseries object }
  \item{FUN}{ function to be applied }
  \item{\dots}{ further arguments to function }
}
\value{
  an Rseries object or vector depending on the fuction type
}
\author{ Whit Armstrong }
\examples{
x <- cbind(rseries(),rseries())

## returns vector
x.col.sum <- column.apply(x,sum)

## returns rseries
x.row.sum <- row.apply(x,sum)

}
\keyword{ts}
