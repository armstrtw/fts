###############################################################
################ Fts class definitions ####################
###############################################################
###############################################################

## layout of an fts object is
## a matrix with a dates attribute attached
fts <- function(data,dates) {

    if(missing(data) && !missing(dates)) {
        data <- rep(NA,length(dates))
    }

    ## generate random data if no data provided
    if(missing(data)) data <- 100 + cumsum(rnorm(1000))

    if(is.null(dim(data))) dim(data) <- c(NROW(data),1)

    if(missing(dates)) {
        dates <- seq(from=as.POSIXct(format(Sys.time(),"%Y-%m-%d")),by="DSTday",length.out=NROW(data))
    }

    if(length(dates) != NROW(data) && length(data)==1) {
        data <- rep(data,length(dates))
        dim(data) <- c(NROW(data),1)
    } else if (length(dates) != NROW(data)) {
        stop("Dates and data must be same length.")
    }
    data <- unclass(data)

    if(is.null(dim(data))) {
        dim(data) <- c(length(data),1)
    }
    if(is.numeric(dates)&&is.null(class(dates))) {
        class(dates) <- c("POSIXt","POSIXct")
    } else {
        dates <- as.POSIXct(dates)
    }
    ans <- data

    ## kill old rownames
    rownames(data) <- NULL

    ## set dates attribute of answer
    attr(ans,"dates") <- dates
    class(ans) <- c("fts","matrix")
    ans
}

as.fts <- function(x) {
    UseMethod("as.fts")
}

as.fts.default <- function(x) {
    if(!is.null(dim(x))) {
        dts <- rownames(x)
    } else {
        dts <- names(x)
    }
    fts(data=as.matrix(x),dates=dts)
}

as.fts.zoo <- function(x) {
    stopifnot(inherits(index(x), "POSIXct"))

    fts(data=coredata(x),
        dates=index(x))
}

as.matrix.fts <- function(x, ...) {
    ans <- matrix(as.numeric(x),nrow=nrow(x),ncol=ncol(x))
    colnames(ans) <- colnames(x)
    rownames(ans) <- format(dates(x),"%Y%m%d")
    ans
}

Ops.fts <- function (e1, e2) {

    if(missing(e2)) {
        ans.data <- NextMethod()
        attr(ans.data,"dates") <- dates(e1)
        class(ans.data) <- c("fts","matrix")
        ans.data
    } else {

        c.e1 <- class(e1)
        c.e2 <- class(e2)

        if("fts"%in%c.e1 && "fts"%in%c.e2) {
            i.dates <- intersect(dates(e1),dates(e2))
            class(i.dates) <- c("POSIXt","POSIXct")

            ## if there is an intersection, the do the Op
            if(length(i.dates)) {

                nce1 <- ncol(e1)
                nce2 <- ncol(e2)

                e1 <- e1[i.dates,]
                e2 <- e2[i.dates,]

                if(nce1==nce2) {
                    ans.data <- NextMethod()
                } else {
                    if(nce1==1) {
                        e1 <- rep(e1,nce2)
                        ans.data <- NextMethod()
                    } else if(nce2==1) {
                        e2 <- rep(e2,nce1)
                        ans.data <- NextMethod()
                    } else {
                        stop("Ops.fts: non conformable data.")
                    }
                }
                attr(ans.data,"dates") <- i.dates
                class(ans.data) <- c("fts","matrix")
                ans.data
            } else {
                ## no matching dates, return NULL
                NULL
            }

        } else {
            ans.data <- NextMethod()
            if("fts"%in%c.e1) {
                ans.dates <- attr(e1,"dates")
            } else {
                ans.dates <- attr(e2,"dates")
            }
            attr(ans.data,"dates") <- ans.dates
            class(ans.data) <- c("fts","matrix")
            ans.data
        }
    }
}

print.fts <- function(x, ...) {
    cnms <- colnames(x)
    dnms <- list(format(dates(x)),cnms)
    dims <- dim(x)
    attributes(x) <- NULL
    dim(x) <- dims
    dimnames(x) <- dnms
    print(x,...)
    invisible(x)
}

"[.fts" <- function(x,i,j,...,drop=FALSE) {

    if(missing(i)) i <- 1:nrow(x)
    if(missing(j)) j <- 1:ncol(x)

    ##
    ##if(typeof(i)=="character") {
    ##    i <- guess.dates(i,dates(x))
    ##}

    ## if we have dates, then use them
    if("POSIXct"%in%class(i)) {
        i <- match(i,dates(x))
    }

    ## FIXME: not sure about this hack...
    ##i <- i[!is.na(i)]
    ##j <- j[!is.na(j)]

    ans.dates <- dates(x)[i]
    ans <- unclass(x)[i,j,...,drop=drop]
    attr(ans,"dates") <- ans.dates
    class(ans) <- c("fts","matrix")

    ticker <- attr(x,"ticker")
    if(!is.null(ticker)) attr(ans,"ticker") <- ticker

    currency <- attr(x,"currency")
    if(!is.null(currency)) attr(ans,"currency") <- currency

    cfactor <- attr(x,"cfactor")
    if(!is.null(cfactor)) attr(ans,"cfactor") <- cfactor

    ans
}

"[<-.fts" <- function(x, i = TRUE, j = TRUE, ..., value) {
    ## if we have dates, then use them
    if(!missing(i)) {
        if("POSIXct"%in%class(i)) {
            i <- match(i,dates(x))
        }
    }
    x <- unclass(x)
    x <- NextMethod()
    class(x) <- "fts"
    x
}

## apply a function to an fts row
row.apply <- function(x,FUN,...) {
    ans <- apply(x,1,FUN,...)

    if(!is.null(dim(ans))) {
        ans <- t(ans)
        rownames(ans) <- NULL
    }

    fts(data=ans,
        dates=dates(x))
}


row.any <- function(x) {
    apply(x,1,any)
}

row.all <- function(x) {
    apply(x,1,all)
}

col.any <- function(x) {
    apply(x,2,any)
}

col.all <- function(x) {
    apply(x,2,all)
}

## apply a function to an fts column
column.apply <- function(x,FUN,...) {
    apply(x,2,FUN,...)
}

remove.na.rows <- function(x) {
    x[!row.any(is.na(x)),]
}

remove.all.na.rows <- function(x) {
    x[!row.all(is.na(x)),]
}

as.data.frame.fts <- function(x,row.names = NULL, optional = FALSE, ...) {
    ans <- as.data.frame.matrix(unclass(x))
    rownames(ans) <- dates(x)
    ans
}

as.matrix.fts <- function(x, ...) {
    rownames(x) <- format(dates(x))
    attr(x,"dates") <- NULL
    class(x) <- "matrix"
    x
}

rbind.fts <- function(...) {
    x <- list(...)

    ## check for same number of cols
    if(length(unique(unlist(lapply(x,ncol))))!=1) {
        stop("unequal number of cols in arguments.")
    }

    ## unclass data
    ans.unclass <- lapply(x,unclass)

    ## do rbind
    ans.data <- do.call(rbind,ans.unclass)

    ## get dates
    ans.dates <- do.call(c,lapply(x,dates))

    ## sort it
    new.order <- order(ans.dates)
    ## must do data before we sort dates
    ans.data <- ans.data[new.order,,drop=F]
    ans.dates <- ans.dates[new.order]
    fts(dates=ans.dates,data=ans.data)
}

cbind.fts <- function(...) {
    x <- list(...)

    ans.dates <- sort(unique(unlist(lapply(x,dates))))
    class(ans.dates) <- c("POSIXt","POSIXct")
    nrow.ans <- length(ans.dates)
    ncol.list <- lapply(x,ncol)
    ncol.ans <- sum(unlist(ncol.list))

    ans <- matrix(NA,nrow=nrow.ans,ncol=ncol.ans)

    ## starting col
    start.col <- 1

    ## fill the answer
    for(i in 1:length(x)) {
        ## guard against null contracts from LIM
        if(!is.null(x[[i]])) {
            end.col <- start.col + ncol.list[[i]] - 1
            ans[match(dates(x[[i]]),ans.dates),start.col:end.col] <- x[[i]]
            start.col <- end.col + 1
        }
    }

    ## set attributes
    ## fix blank cnames
    cnames.list <- lapply(x,colnames)
    colnames(ans) <- fix.cnames(cnames.list)
    attr(ans,"dates") <- ans.dates
    class(ans) <- "fts"
    ans
}

fix.cnames <- function(cnames.list) {
    null.cnames <- unlist(lapply(cnames.list,is.null))
    for(i in 1:length(cnames.list)) {
        if(null.cnames[i]) {
            cnames.list[[i]] <- ""
        }
    }
    unlist(cnames.list)
}

pad <- function(x,pad.dates) {

    new.dates <- sort(unique(c(dates(x),pad.dates)))
    class(new.dates) <- c("POSIXt","POSIXct")
    ans <- matrix(nrow=length(new.dates),ncol=ncol(x))
    attr(ans,"dates") <- new.dates
    class(ans) <- c("fts","matrix")
    ans[dates(x),] <- x
    colnames(ans) <- colnames(x)
    ans
}

trim <- function(x,trim.dates) {
    new.dates <- sort(intersect(dates(x),trim.dates))
    class(new.dates) <- c("POSIXt","POSIXct")
    x[new.dates,]
}

write.csv.fts <- function(x,file,date.format="%Y-%m-%d %T",...) {
    rownames(x) <- format(dates(x),date.format)
    write.csv(x,file,na="=NA()",...)
}

read.csv.fts <- function(file,date.format="%Y-%m-%d %T",...) {
    ans <- as.matrix(read.csv(file,row.names=1,...))

    dts <- as.POSIXct(strptime(rownames(ans),date.format))

    rownames(ans) <- NULL

    fts(dates=dts,
            data=ans)
}

read.rds.fts <- function(file) {
    x <- .readRDS(file)
    x
}

write.rds.fts <- function(x,file) {
    .saveRDS(x,file)
}


head.fts <- function(x,n=10,...) {
    x[1:n,]
}

tail.fts <- function(x,n=10,...) {
    nr <- nrow(x)
    if(n>=nr) {
        x
    } else {
        x[(nr-n+1):nr,]
    }
}

cumsum.fts <- function(x) {
    fts(data=apply(x,2,cumsum),
        dates=dates(x))
}

cumprod.fts <- function(x) {
    fts(data=apply(x,2,cumprod),
        dates=dates(x))
}

cummax.fts <- function(x) {
    fts(data=apply(x,2,cummax),
        dates=dates(x))
}

cummin.fts <- function(x) {
    fts(data=apply(x,2,cummin),
        dates=dates(x))
}

###############################################################
################### Date Functions ############################
###############################################################
###############################################################

dates <- function(x) {
    UseMethod("dates")
}

"dates<-" <- function(x, value) {
    UseMethod("dates<-")
}


dates.fts <- function(x) {
    attr(x,"dates")
}

"dates<-.fts" <- function(x, value) {
    ## FIXME: might put something here to convert
    ## POSIXlt to POSIXct
    stopifnot(length(value)==length(dates(x)))
    attr(x,"dates") <- value
    x
}

## return the dates just when the fts is true
event.dates <- function(x) {
    stopifnot(ncol(x)==1)
    dates(x)[as.logical(x)&!is.na(x)]
}

## find date intersection of all tseries
intersect.all <- function(...) {
    x <- list(...)
    dts <- lapply(x,dates)
    ans <- dts[[1]]

    if(length(dts) > 1) {
        for(i in 2:length(x)) {
            ans <- intersect(ans,dts[[i]])
        }
    }

    class(ans) <- c("POSIXt","POSIXct")
    ans
}


###############################################################
################ Calls to External Library ####################
###############################################################
###############################################################


moving.mean <- function(x,periods) {
    .Call("movingMean",x,as.integer(periods),PACKAGE="fts")
}

moving.sum <- function(x,periods) {
    .Call("movingSum",x,as.integer(periods),PACKAGE="fts")
}

moving.product <- function(x,periods) {
    .Call("movingProduct",x,as.integer(periods),PACKAGE="fts")
}

moving.max <- function(x,periods) {
    .Call("movingMax",x,as.integer(periods),PACKAGE="fts")
}

moving.min <- function(x,periods) {
    .Call("movingMin",x,as.integer(periods),PACKAGE="fts")
}

moving.rank <- function(x,periods) {
    .Call("movingRank",x,as.integer(periods),PACKAGE="fts")
}

moving.sd <- function(x,periods) {
    .Call("movingStdev",x,as.integer(periods),PACKAGE="fts")
}

expanding.max <- function(x) {
    .Call("expandingMax",x,PACKAGE="fts")
}

expanding.min <- function(x) {
    .Call("expandingMin",x,PACKAGE="fts")
}


moving.cor <- function(x,y,periods) {
    .Call("movingCor", x, y, as.integer(periods),PACKAGE="fts")
}

moving.cov <- function(x,y,periods) {
    .Call("movingCov", x, y, as.integer(periods),PACKAGE="fts")
}


since.na <- function(x) {
    .Call("sinceNA",x,PACKAGE="fts")
}

lag.fts <- function(x, k, ...) {
    if(k < 0) stop("lag: only positive values of k are allowed")
    .Call("lag", x, as.integer(k),PACKAGE="fts")
}

lead <- function(x, k, ...) {
    UseMethod("lead")
}

lead.fts <- function(x, k, ...) {
    if(k < 0) stop("only positive values of k are allowed")
    .Call("lead",x ,as.integer(k),PACKAGE="fts")
}

fill.fwd <- function(x) {
    .Call("fillForward",x,PACKAGE="fts")
}

fill.bwd <- function(x) {
    .Call("fillBackward",x,PACKAGE="fts")
}

fill.value <- function(x,value) {
    .Call("fillValue",x,value,PACKAGE="fts")
}

to.quarterly <- function(x) {
    .Call("toQuarterly",x,PACKAGE="fts")
}

to.weekly <- function(x) {
    .Call("toWeekly",x,PACKAGE="fts")
}

to.monthly <- function(x) {
    .Call("toMonthly",x,PACKAGE="fts")
}

###############################################################
############ Plotting functions for Fts Objects ###########
###############################################################
###############################################################

plot.fts <- function(x,type="l",...) {

    ## can only plot 1 column for now
    if("close"%in%colnames(x)) x <- x[,"close"]

    plot(dates(x),x,type=type,...)
}

###############################################################
############ Regression for Fts Objects #######################
###############################################################
###############################################################

lm.fts <- function(y,...,origin=F) {
    x <- list(...)

    i.dts <- intersect(dates(y),
                       do.call(intersect.all,x))
    class(i.dts) <- c("POSIXt","POSIXct")

    x <- lapply(x,"[",i.dts,)
    x <- unclass(do.call(cbind,x))
    y <- unclass(y[i.dts,])

    if(origin) {
        ans <- lm( y ~ 0 + x)
    } else {
        ans <- lm(y~x)
    }
    ans
}
