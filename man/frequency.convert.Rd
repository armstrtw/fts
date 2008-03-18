\name{frequency.convert}
\alias{to.weekly}
\alias{to.monthly}
\alias{to.quarterly}

\title{Change Frequencies}
\description{
  convert a time series from a higher frequency to a lower frequency
  or from an irregular frequency to a regular frequency
}
\usage{
to.weekly(x)
to.monthly(x)
to.quarterly(x)
}
\arguments{
  \item{x}{ An Fts object }
}
\value{
  an Fts object
}
\author{ Whit Armstrong }
\examples{
x <- fts()

to.weekly(x)
to.monthly(x)
to.quarterly(x)
}
\keyword{ts}
