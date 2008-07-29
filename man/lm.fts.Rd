\name{lm.fts}
\alias{lm.fts}
\title{linear regression for fts objects}
\description{
Do a linear regression using date matching between left hand side and
explanatory variables
}
\usage{
lm.fts(y, \dots, origin=F)
}
\arguments{
  \item{y}{ An Fts object }
  \item{\dots}{ Fts objects }
  \item{origin}{ boolean indicating whether regression should go though origin}
}
\value{
  an lm obmect
}
\author{ Whit Armstrong }
\examples{
x <- fts()
y <- fts()
z <- fts()

## show that date matching is done
z <- z[1:50,]

lm.result <- lm.fts(z,y,x)
summary(lm.result)
}
\keyword{ts}
