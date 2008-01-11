\name{pad}
\alias{pad}
\alias{trim}
\title{pad and trim dates}
\description{
  add dates to an Rseries object by padding w/ additional dates or
  remove dates from an Rseries object by trimming dates
}
\usage{
pad(x, pad.dates)
trim(x, trim.dates)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ An Rseries object }
  \item{pad.dates}{ a vector of dates }
  \item{trim.dates}{ a vector of dates }
}
\value{
  an Rseries object
}
\author{ Whit Armstrong }
\examples{

x <- rseries(rnorm(5))
pad(x,Sys.time()+60*60*24*200)

trim.dts <- seq(from = as.POSIXct(format(Sys.time(), "%Y-%m-%d")),by = "DSTday",length.out = 2)
trim(x,trim.dts)
}
\keyword{ts}

