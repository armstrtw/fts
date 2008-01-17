\name{since.na}
\alias{since.na}
\title{Count distance since an NA has occurred}
\description{
Count number of rows since an NA has occurred
}
\usage{
since.na(x)
}
\arguments{
  \item{x}{ An Rseries object }
}
\value{
  an Rseries object
}
\author{ Whit Armstrong }
\examples{
x <- rseries(rnorm(100))

x[10,] <- NA

since.na(x)
}
\keyword{ts}
