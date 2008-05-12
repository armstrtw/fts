require(fts)
require(zoo)

NC <- 20
NR <- 10000
WIN <- 50

gctorture(TRUE)

raw.data <- matrix(rnorm(NR*NC),nrow=NR,ncol=NC)

x <- fts(raw.data)
x.zoo <- zoo(raw.data, dates(x))

do.test <- function(test.name, fts.data, zoo.data, fts.func, zoo.func, win) {
    cat(test.name,"\n")

    fts.time <- system.time(fts.result <- fts.func(fts.data, win))
    zoo.time <- system.time(zoo.result <- zoo.func(zoo.data, win))

    stopifnot(all.equal(as.vector(fts.result[-(1:(win-1)),]),as.vector(zoo.result)))

    ans <- list(test=test.name,
                fts.time=fts.time,
                zoo.time=zoo.time)

    ans
}

mean.test <- do.test("moving.mean", x, x.zoo, moving.mean, rollmean, WIN)
max.test <- do.test("moving.max", x, x.zoo, moving.max, rollmax, WIN)
##min.test <- do.test("moving.min", x, x.zoo, moving.min, rollmin, WIN)
