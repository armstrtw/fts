\name{fts}
\alias{fts}
\alias{Ops.fts}
\alias{[.fts}
\alias{[<-.fts}
\alias{index}
\alias{index<-}
\alias{index.fts}
\alias{index<-.fts}
\alias{cbind.fts}
\alias{rbind.fts}
\alias{print.fts}
\alias{plot.fts}

\title{Fts: a fast timeseries library }
\description{
  create an fts object by specifying dates and data
}
\usage{
fts(data, dates)
\method{index}{fts}(x)
\method{index}{fts}(x) <- value
}

\arguments{
  \item{data}{a matrix, dataframe, or vector}
  \item{dates}{a vector of dates (default format is POSIXct) }
  \item{x}{an fts object}
  \item{value}{a vector of POSIXct dates}
  \item{\dots}{further arguments to function}
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
z <- fts(dates=seq.POSIXt(from=as.POSIXct(Sys.time()),by="months",length.out=12))

xx <- x[1:10,]

## intersection of dates is taken for Arith methods
xyp <- x + y
xys <- x - y
xym <- x * y
xyd <- x / y
xyg <- x > y
xyl <- x < y

cxy <- cbind(x,y)
rxy <- rbind(x,y)
print(x)
plot(x)
}
\keyword{ts}
