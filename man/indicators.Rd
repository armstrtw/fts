\name{indicators}
\alias{indicators}
\alias{above.ma}
\alias{analog}
\alias{below.ma}
\alias{wday}
\alias{mday}
\alias{diff.fts}
\alias{up}
\alias{down}
\alias{ema}
\alias{gap.continue}
\alias{gap.direction}
\alias{gap.down}
\alias{gap.down.continue}
\alias{gap.down.reverse}
\alias{gap.reverse}
\alias{gap.up}
\alias{gap.up.continue}
\alias{gap.up.reverse}
\alias{higher.high}
\alias{higher.low}
\alias{hl.oc.ratio}
\alias{inside.day}
\alias{inside.day.direction}
\alias{inside.day.down}
\alias{inside.day.up}
\alias{lower.high}
\alias{lower.low}
\alias{ma.crossover}
\alias{ma.crossover.down}
\alias{ma.crossover.up}
\alias{ma.d}
\alias{ma.distance}
\alias{month}
\alias{year}
\alias{monthly.sum}
\alias{new.high}
\alias{new.low}
\alias{outside.day}
\alias{outside.day.direction}
\alias{outside.day.down}
\alias{outside.day.up}
\alias{pct.chg}
\alias{repeated}
\alias{rsi}
\alias{rsi.crossover}
\alias{rsi.crossover.down}
\alias{rsi.crossover.up}
\alias{template.fts}
\alias{trend.day}
\alias{trend.day.down}
\alias{trend.day.up}

\title{Trading indicators}
\description{
  Various binary indicators for fts objects
}
\usage{
above.ma(x,n)
below.ma(x,n)
wday(x)
mday(x)
\method{diff}{fts}(x,k,\dots)
up(x)
down(x)
ema(x,periods)
gap.continue(x)
gap.direction(x)
gap.down(x)
gap.down.continue(x)
gap.down.reverse(x)
gap.reverse(x)
gap.up(x)
gap.up.continue(x)
gap.up.reverse(x)
higher.high(x)
higher.low(x)
hl.oc.ratio(x)
inside.day(x)
inside.day.direction(x)
inside.day.down(x)
inside.day.up(x)
lower.high(x)
lower.low(x)
ma.crossover(x,n)
ma.crossover.down(x,n)
ma.crossover.up(x,n)
ma.d(x,n)
ma.distance(x,periods)
month(x)
year(x)
monthly.sum(x)
new.high(x,n)
new.low(x,n)
outside.day(x)
outside.day.direction(x)
outside.day.down(x)
outside.day.up(x)
pct.chg(x)
repeated(x,times)
rsi(x,periods)
rsi.crossover(x,periods,thresh)
rsi.crossover.down(x,periods,thresh)
rsi.crossover.up(x,periods,thresh)
template.fts(index, cnames)
trend.day(x,thresh)
trend.day.down(x,thresh)
trend.day.up(x,thresh)
}
\arguments{
  \item{x}{ An Fts object }
  \item{periods}{ number of periods}
  \item{n}{ number of periods}
  \item{k}{ number of lags}
  \item{times}{ how many times }
  \item{thresh}{ threshold level}
  \item{index}{ index to use to construct fts object}
  \item{cnames}{ colnames to use to construct fts object}
  \item{\dots}{ further arguments to function }
}
\details{
  removed elements are replaced with NA
}
\value{
  an fts object
}
\author{ Whit Armstrong }
\examples{
x <- fts(index=seq(from=Sys.Date(),by="months",length.out=5),rnorm(5))
print(x)
lag(x,1)
lead(x,1)
}
\keyword{ts}
