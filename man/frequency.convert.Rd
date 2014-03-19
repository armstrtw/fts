\name{frequency.convert}
\alias{frequency.convert}
\alias{to.weekly}
\alias{to.day.of.week}
\alias{to.monthly}
\alias{to.quarterly}
\alias{to.daily}
\alias{to.hourly}
\alias{to.minute}
\alias{to.second}
\alias{to.yearly}


\title{Change Frequencies}
\description{
  convert a time series from a higher frequency to a lower frequency
  or from an irregular frequency to a regular frequency
}
\usage{
to.weekly(x)
to.monthly(x)
to.quarterly(x)
to.day.of.week(x,day.of.week,beginning.of.period=TRUE)
}
\arguments{
  \item{x}{An Fts object.}
  \item{day.of.week}{a numerical value indicating the day of week
    following POSIXlt conventions.}
  \item{beginning.of.period}{whether to shift the sampling dates to the
    beginning of period dates.}
}
\value{
  an Fts object
}
\author{ Whit Armstrong }
\examples{
x <- fts(index=seq(from=Sys.Date(),by="days",length.out=500),data=1:500)

to.weekly(x)
to.monthly(x)
to.quarterly(x)
}
\keyword{ts}
