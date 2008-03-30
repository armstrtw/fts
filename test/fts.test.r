require(fts)
require(zoo)

NC <- 10
NR <- 10000
WIN <- 50

raw.data <- matrix(rnorm(NR*NC),nrow=NR,ncol=NC)

x <- fts(raw.data)
x.zoo <- zoo(raw.data, dates(x))

fts.mean.time <- system.time(x.mean <- moving.mean(x,WIN))
zoo.mean.time <- system.time(x.zoo.mean <- rollmean(x.zoo,WIN))

print(all.equal(as.vector(x),as.vector(x.zoo)))
