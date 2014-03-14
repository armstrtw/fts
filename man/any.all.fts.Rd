\name{fts.logical}
\alias{fts.logical}
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
x <- fts(seq(from=Sys.Date(),by="months",length.out=50),matrix(rnorm(100),nrow=50))
jj <- x > 0
row.all(jj)
row.any(jj)

col.any(x > 0)
col.all(x > -3)
}
\keyword{ts}
