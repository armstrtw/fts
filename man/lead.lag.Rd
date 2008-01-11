\name{lead.lag}
%\alias{lag}
\alias{lead}
\alias{lead.rseries}
\alias{lag.rseries}

\title{Shift an Rseries ojbect in time}
\description{
  Shift an Rseries ojbect forward or backwards in time be the supplied
  number of periods
}
\usage{
lead(x,k, ...)
}
\arguments{
  \item{x}{ An Rseries object }
  \item{k}{ number of periods to shift }
  \item{\dots}{ further arguments to function }
}
\details{
  removed elements are replaced with NA
}
\value{
  an Rseries object
}
\author{ Whit Armstrong }
\examples{
x <- rseries(rnorm(20))

lag(x,10)
lead(x,10)

}
\keyword{ts}
