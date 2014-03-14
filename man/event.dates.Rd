\name{event.dates}
\alias{event.dates}
\title{ Extract Dates }
\description{
  Extract the dates from a one column LOGICAL Fts object where value is TRUE
}
\usage{
event.dates(x)
}

\arguments{
  \item{x}{ An Fts object }
}
\details{
  removes NA values before extracting dates
}
\value{
  a vector of dates
}
\author{ Whit Armstrong }
\examples{
x <- fts(index=seq(from=Sys.Date(),by="days",length.out=100),data=rnorm(100))
x.bool <- x > 10
event.dates(x.bool)

## ignores NA's
x.bool[10:20] <- NA
event.dates(x.bool)

}
\keyword{ts}

