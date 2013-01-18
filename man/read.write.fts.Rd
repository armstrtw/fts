\name{read.write.fts}
\alias{read.write.fts}
\alias{read.csv.fts}
\alias{write.csv.fts}

\title{Read / Write Files }
\description{
  Read / Write files to csv or .RDS format
}
\usage{
read.csv.fts(file, date.column=1, date.format="\%Y-\%m-\%d", ...)
write.csv.fts(x, file, ...)
}
\arguments{
  \item{x}{ An Fts object }
  \item{file}{ filename of file to read/write}
  \item{date.column}{ column that = the dates are in }
  \item{date.format}{ the format of the date strings }
  \item{\dots}{ further arguments to underlying read/write functions }
}
\value{
  a Fts object for functions that read data
}
\author{ Whit Armstrong }
\examples{
x <- fts()
colnames(x) <- "big.ass.black.dog"

csv.fname <- paste(tempfile(),".csv",sep="")
write.csv.fts(x,csv.fname)
y.csv <- read.csv.fts(csv.fname)

all.equal(x,y.csv)
}
\keyword{ts}
