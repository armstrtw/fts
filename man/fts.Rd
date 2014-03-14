\name{fts}
\alias{fts}
\alias{Ops.fts}
\alias{[.fts}
\alias{[<-.fts}
\alias{cbind.fts}
\alias{rbind.fts}
\alias{print.fts}
\alias{plot.fts}

\title{Fts: a fast timeseries library }
\description{
  create an fts object by specifying index and data
}
\usage{
fts(index,data)
}

\arguments{
  \item{index}{a vector of dates}
  \item{data}{a matrix, dataframe, or vector}
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
x <- fts(index=seq(from=Sys.Date(),by="months",length.out=24),data=1:24)
y <- fts(index=seq(from=Sys.Date(),by="months",length.out=12),data=13:24)
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
