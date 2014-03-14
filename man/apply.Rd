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
x <- fts(index=seq(from=Sys.Date(),by="months",length.out=24),data=1:24)
y <- fts(index=seq(from=Sys.Date(),by="months",length.out=24),data=1:24)

z <- cbind(x,y)

## returns vector
z.col.sum <- column.apply(z,sum)

## returns fts
z.row.sum <- row.apply(z,sum)

}
\keyword{ts}
