\name{frequency.convert}
\alias{daily2weekly}
\alias{daily2monthly}
\alias{daily2quarterly}

\title{Change Frequencies}
\description{
  convert a time series from a higher frequency to a lower frequency
  or from an irregular frequency to a regular frequency
}
\usage{
daily2weekly(x)
daily2monthly(x)
daily2quarterly(x)
}
\arguments{
  \item{x}{ An Rseries object }
}
\value{
  an Rseries object
}
\author{ Whit Armstrong }
\examples{
x <- rseries()

daily2weekly(x)
daily2monthly(x)
daily2quarterly(x)
}
\keyword{ts}
