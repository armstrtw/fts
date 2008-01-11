\name{as.rseries}
\alias{as.rseries}
\alias{as.rseries.default}
\alias{as.rseries.data.frame}
\alias{as.rseries.matrix}
\alias{as.data.frame.rseries}
\alias{as.matrix.rseries}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convert from/to rseries }
\description{
convert an object into an rseries and vice versa
}
\usage{
as.rseries(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an R matrix or data.frame }
}
\details{
  converts a rectanular object into an Rseries object
  must be able to convert rownames into some form of dates
}
\value{
  an Rseries object
}
\author{ Whit Armstrong }
\examples{
N <- 100
xm <- matrix(rnorm(N))
dts <- format(seq(from=Sys.time(),length.out=N,by="DSTday"),"\%Y-\%m-\%d")
rownames(xm) <- dts
x.from.m <- as.rseries(xm)
x.from.df <- as.rseries(as.data.frame(xm))
}
\keyword{ts}
