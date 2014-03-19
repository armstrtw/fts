\name{pad}
\alias{pad.trim}
\alias{pad}
\alias{trim}
\alias{filter.min.obs}
\title{pad and trim dates}
\description{
  add dates to an Fts object by padding w/ additional dates or
  remove dates from an Fts object by trimming dates
}
\usage{
pad(x, pad.dates)
trim(x, trim.dates)
filter.min.obs(x, obs.required)
}

\arguments{
  \item{x}{ An Fts object }
  \item{pad.dates}{a vector of dates.}
  \item{trim.dates}{a vector of dates.}
  \item{obs.required}{number of required observations per row.}
}
\value{
  an fts object
}
\author{ Whit Armstrong }
\examples{

x <- fts(index=seq(from=Sys.Date(),by="days",length.out=5),data=1:5)
pad.dates <- index(x)[1] + c(10L,20L)
pad(x,pad.dates)

trim.dts <- index(x)[c(1,3)]

trim(x,trim.dts)
}
\keyword{ts}

