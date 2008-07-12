\name{fill}
\alias{fill}
\alias{fill.fwd}
\alias{fill.bwd}
\alias{fill.value}

\title{Fill Missing Values}
\description{
  Fill a missing value (NA) with any of previous value, next value, or a
  user supplied value.
}
\usage{
fill.fwd(x)
fill.bwd(x)
fill.value(x,value)
}
\arguments{
  \item{x}{ An Fts object }
  \item{value}{ a value to replace the missing values}
}
\value{
  an Fts object
}
\author{ Whit Armstrong }
\examples{
x <- fts(rnorm(10))

x[5,] <- NA
fill.fwd(x)

x[5,] <- NA
fill.bwd(x)

x[5,] <- NA
fill.value(x,100.0)
}
\keyword{ts}

