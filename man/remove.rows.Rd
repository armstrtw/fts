\name{remove.rows}
\alias{remove.rows}
\alias{remove.na.rows}
\alias{remove.all.na.rows}
\title{Remove Rows}
\description{
  remove.na.rows removes rows which contain at least 1 NA
  remove.all.na rows removes rows which are all NA's
}
\usage{
remove.na.rows(x)
remove.all.na.rows(x)
}
\arguments{
  \item{x}{ An Fts object }
}
\value{
  an Fts object
}
\author{ Whit Armstrong }
\examples{
x <- fts(index=seq(from=Sys.Date(),by="days",length.out=10),matrix(rnorm(20),ncol=2))

x[5,1] <- NA
x[10,] <- NA

print(x)

## will drop rows where NA's appear
## in any of the columns
remove.na.rows(x)

## will drop rows where NA's appear
## in all of the columns
remove.all.na.rows(x)
}
\keyword{ts}
