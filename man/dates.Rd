\name{dates}
\alias{dates}
\alias{dates.rseries}
\title{ Extract Dates }
\description{
  shortcut wrapper to extract dates from an Rseries object
}
\usage{
dates(x)
}
\arguments{
  \item{x}{ An Rseries object }
}
\details{
  extracts the dates attribute from an Rseries object
}
\value{
  a vector of dates
}
\author{ Whit Armstrong }
\examples{
x <- rseries()
dates(x)
}
\keyword{ts}

