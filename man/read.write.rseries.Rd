\name{read.write.rseries}

\alias{read.rds.rseries}
\alias{write.rds.rseries}
\alias{read.csv.rseries}
\alias{write.csv.rseries}

\title{Read / Write Files }
\description{
  Read / Write files to csv or .RDS format
}
\usage{
read.rds.rseries(file)
write.rds.rseries(x,file)

read.csv.rseries(file, date.format, ...)
write.csv.rseries(x, file, date.format, ...)
}
\arguments{
  \item{x}{ An Rseries object }
  \item{file}{ filename of file to read/write}
  \item{date.format}{ a format string specifying the date format to use
    when reading / writing dates}
  \item{\dots}{ further arguments to underlying read/write functions }
}
\value{
  a Rseries object for functions that read data
}
\author{ Whit Armstrong }
\examples{
x <- rseries()
colnames(x) <- "big.ass.black.dog"

csv.fname <- paste(tempfile(),".csv",sep="")
write.csv.rseries(x,csv.fname)
y.csv <- read.csv.rseries(csv.fname)

all.equal(x,y.csv)

rds.fname <- paste(tempfile(),".rseries",sep="")
write.rds.rseries(x,rds.fname)
y.rds <- read.rds.rseries(rds.fname)
all.equal(x,y.rds)
}
\keyword{ts}
