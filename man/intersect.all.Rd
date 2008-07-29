\name{intersect.all}
\alias{intersect.all}
\title{find date intersection among multiple fts objects}
\description{
find date intersection
}
\usage{
intersect.all(\dots)
}
\arguments{
  \item{\dots}{ Fts objects }
}
\value{
a vector of dates
}
\author{ Whit Armstrong }
\examples{
x <- fts()
y <- fts()
y <- y[1:10,]

intersect.all(x,y)
}
\keyword{ts}
