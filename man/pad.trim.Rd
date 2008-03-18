\name{pad}
\alias{pad}
\alias{trim}
\title{pad and trim dates}
\description{
  add dates to an Fts object by padding w/ additional dates or
  remove dates from an Fts object by trimming dates
}
\usage{
pad(x, pad.dates)
trim(x, trim.dates)
}

\arguments{
  \item{x}{ An Fts object }
  \item{pad.dates}{ a vector of dates }
  \item{trim.dates}{ a vector of dates }
}
\value{
  an Fts object
}
\author{ Whit Armstrong }
\examples{

x <- fts(rnorm(5))
pad(x,Sys.time()+60*60*24*200)

trim.dts <- dates(x)[c(1,3)]

trim(x,trim.dts)
}
\keyword{ts}

