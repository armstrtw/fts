\name{rseries.logical}
\alias{col.any}
\alias{col.all}
\alias{row.any}
\alias{row.all}

\title{Logical subsets of objects}
\description{
Find subsets of logical objects
}
\usage{
col.any(x)
col.all(x)
row.any(x)
row.all(x)
}
\arguments{
  \item{x}{ a rectangular object }
}
\value{
  a logical vector
}
\author{ Whit Armstrong }
\examples{

x <- rseries(matrix(rnorm(100),nrow=50))
jj <- x > 0
row.all(jj)
}
\keyword{ts}
