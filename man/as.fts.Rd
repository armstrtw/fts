\name{as.fts}
\alias{as.fts}
\alias{as.fts.default}
\alias{as.fts.data.frame}
\alias{as.fts.matrix}
\alias{as.data.frame.fts}
\alias{as.matrix.fts}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convert from/to fts }
\description{
convert an object into an fts and vice versa
}
\usage{
as.fts(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an R matrix or data.frame }
}
\details{
  converts a rectanular object into an Fts object
  must be able to convert rownames into some form of dates
}
\value{
  an Fts object
}
\author{ Whit Armstrong }
\examples{
N <- 100
xm <- matrix(rnorm(N))
dts <- format(seq(from=Sys.time(),length.out=N,by="DSTday"),"\%Y-\%m-\%d")
rownames(xm) <- dts
x.from.m <- as.fts(xm)
x.from.df <- as.fts(as.data.frame(xm))
}
\keyword{ts}
