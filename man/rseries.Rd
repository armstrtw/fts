\name{rseries}
\alias{rseries}
\alias{Ops.rseries}
\alias{[.rseries}
\alias{[<-.rseries}
\alias{dates<-}
\alias{cbind.rseries}
\alias{rbind.rseries}
\alias{head.rseries}
\alias{tail.rseries}
\alias{print.rseries}
\alias{plot.rseries}

\title{Rseries: a fast timeseries library }
\description{
  create an rseries object by specifying dates and data
}
\usage{
rseries(data, dates)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a matrix, dataframe, or vector}
  \item{dates}{a vector of dates (default format is POSIXct) }
}
\details{
rseries is an S3 class in which the rseries object is represented as a
native R matrix and the dates are attached as an attribute to the matrix
}
\value{
  a rseries object
}
\author{ Whit Armstrong }
\seealso{\code{\link{as.rseries}}}
\examples{
x <- rseries()
y <- rseries(data=1:100)
y <- rseries(dates=seq.POSIXt(from=as.POSIXct(Sys.time()),by="months",length.out=12))
}
\keyword{ts}
