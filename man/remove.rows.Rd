\name{remove.rows}
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
x <- fts(matrix(rnorm(20),ncol=2))

x[5,1] <- NA
x[10,] <- NA

print(x)

remove.na.rows(x)
remove.all.na.rows(x)
}
\keyword{ts}
