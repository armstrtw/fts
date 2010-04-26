require(fts)
require(zoo)

NC <- 20
NR <- 10000
WIN <- 50

##gctorture(TRUE)

raw.data <- matrix(rnorm(NR*NC),nrow=NR,ncol=NC)

x <- fts(raw.data)
x.zoo <- zoo(raw.data, dates(x))

do.test <- function(test.name, fts.data, zoo.data, fts.func, zoo.func, win) {
    cat(test.name,"\n")

    fts.time <- system.time(fts.result <- fts.func(fts.data, win))
    zoo.time <- system.time(zoo.result <- rollapply(zoo.data, win, zoo.func))

    stopifnot(all.equal(as.vector(fts.result),as.vector(zoo.result)))

    ## ratio of speeds is result
    ans <- zoo.time/fts.time

    ans[c("user.self","elapsed")]
}

test.list <- list()
test.list[["sum"]] <- do.test("moving.sum", x, x.zoo, moving.sum, sum, WIN)
test.list[["mean"]] <- do.test("moving.mean", x, x.zoo, moving.mean, mean, WIN)
test.list[["max"]] <- do.test("moving.max", x, x.zoo, moving.max, max, WIN)
test.list[["min"]] <- do.test("moving.min", x, x.zoo, moving.min, min, WIN)

res <- do.call(cbind,test.list)

print(res)
