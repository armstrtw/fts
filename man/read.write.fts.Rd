\name{read.write.fts}
\alias{read.write.fts}
\alias{read.rds.fts}
\alias{write.rds.fts}
\alias{read.csv.fts}
\alias{write.csv.fts}

\title{Read / Write Files }
\description{
  Read / Write files to csv or .RDS format
}
\usage{
read.rds.fts(file)
write.rds.fts(x,file)

read.csv.fts(file, date.format, ...)
write.csv.fts(x, file, date.format, ...)
}
\arguments{
  \item{x}{ An Fts object }
  \item{file}{ filename of file to read/write}
  \item{date.format}{ a format string specifying the date format to use
    when reading / writing dates}
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

rds.fname <- paste(tempfile(),".fts",sep="")
write.rds.fts(x,rds.fname)
y.rds <- read.rds.fts(rds.fname)
all.equal(x,y.rds)
}
\keyword{ts}
