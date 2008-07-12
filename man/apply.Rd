\name{apply}
\alias{apply}
\alias{row.apply}
\alias{column.apply}

\title{Apply Function}

\description{
  Apply a function to the rows or columns of an fts object
}

\usage{
column.apply(x, FUN, ...)
row.apply(x, FUN, ...)
}

\arguments{
  \item{x}{ An Fts object }
  \item{FUN}{ function to be applied }
  \item{\dots}{ further arguments to function }
}
\value{
  an Fts object or vector depending on the fuction type
}
\author{ Whit Armstrong }
\examples{
x <- cbind(fts(),fts())

## returns vector
x.col.sum <- column.apply(x,sum)

## returns fts
x.row.sum <- row.apply(x,sum)

}
\keyword{ts}
