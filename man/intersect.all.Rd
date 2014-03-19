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
x <- fts(index=seq(from=Sys.Date(),by="days",length.out=100),data=1:100)
y <- fts(index=seq(from=Sys.Date(),by="days",length.out=100),data=1:100)
y <- y[1:nrow(y) \%\% 2==0,]
intersect.all(x,y)
}
\keyword{ts}
