\name{expanding}
\alias{expanding}
\alias{expanding.mean}
\alias{expanding.sum}
\alias{expanding.product}
\alias{expanding.max}
\alias{expanding.min}
\alias{expanding.sd}
\alias{expanding.rank}

\alias{expanding.cor}
\alias{expanding.cov}

\title{Moving Functions}
\description{
  apply summary functions on an expanding basis
}
\usage{
expanding.max(x)
expanding.min(x)
}

\arguments{
  \item{x}{ An Fts object }
}


\details{
  apply a function that takes a vector and returns a scalar on an expanding
  basis to an fts object
}
\value{
  an fts object
}
\author{ Whit Armstrong }
\examples{
x <- fts()

x.emax <- expanding.max(x)
x.emin <- expanding.min(x)
}
\keyword{ts}
