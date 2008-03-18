\name{fts}
\alias{fts}
\alias{Ops.fts}
\alias{[.fts}
\alias{[<-.fts}
\alias{dates<-}
\alias{cbind.fts}
\alias{rbind.fts}
\alias{head.fts}
\alias{tail.fts}
\alias{print.fts}
\alias{plot.fts}

\title{Fts: a fast timeseries library }
\description{
  create an fts object by specifying dates and data
}
\usage{
fts(data, dates)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a matrix, dataframe, or vector}
  \item{dates}{a vector of dates (default format is POSIXct) }
}
\details{
fts is an S3 class in which the fts object is represented as a
native R matrix and the dates are attached as an attribute to the matrix
}
\value{
  a fts object
}
\author{ Whit Armstrong }
\seealso{\code{\link{as.fts}}}
\examples{
x <- fts()
y <- fts(data=1:100)
y <- fts(dates=seq.POSIXt(from=as.POSIXct(Sys.time()),by="months",length.out=12))
}
\keyword{ts}
