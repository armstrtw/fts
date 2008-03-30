require(fts)
require(zoo)

NC <- 20
NR <- 1000
WIN <- 20

raw.data <- matrix(rnorm(NR*NC),nrow=NR,ncol=NC)

x <- fts(raw.data)
x.zoo <- zoo(raw.data, dates(x))


## moving mean test
fts.mean.time <- system.time(x.mean <- moving.mean(x,WIN))
zoo.mean.time <- system.time(x.zoo.mean <- rollmean(x.zoo,WIN))

stopifnot(all.equal(as.vector(x.mean[-(1:(WIN-1)),]),as.vector(x.zoo.mean)))

## moving mean test
fts.max.time <- system.time(x.max <- moving.max(x,WIN))
zoo.max.time <- system.time(x.zoo.max <- rollmax(x.zoo,WIN))

stopifnot(all.equal(as.vector(x.max[-(1:(WIN-1)),]),as.vector(x.zoo.max)))

print(fts.mean.time)
print(zoo.mean.time)

print(fts.max.time)
print(zoo.max.time)
