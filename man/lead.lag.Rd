\name{lead.lag}
\alias{lead}
\alias{lag}
\alias{lead.fts}
\alias{lag.fts}

\title{Shift an Fts ojbect in time}
\description{
  Shift an Fts ojbect forward or backwards in time be the supplied
  number of periods
}
\usage{
lead(x, k, ...)
lag(x, k, ...)
}
\arguments{
  \item{x}{ An Fts object }
  \item{k}{ number of periods to shift }
  \item{\dots}{ further arguments to function }
}
\details{
  removed elements are replaced with NA
}
\value{
  an Fts object
}
\author{ Whit Armstrong }
\examples{
x <- fts(rnorm(5))
print(x)
lag(x,1)
lead(x,1)
}
\keyword{ts}
