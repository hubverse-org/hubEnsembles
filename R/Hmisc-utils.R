# All functions adapted from Hmisc pkg functions
# Compute weighted quantiles
# Source: https://github.com/harrelfe/Hmisc/blob/f80ecfb27e1deb88b3c6cc075c7252e3c577f66b/R/wtd.stats.s#L48
wtd.quantile <- function(x, weights=NULL, probs=c(0, .25, .5, .75, 1), 
                         type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                         normwt=FALSE, na.rm=TRUE)
{
  if(! length(weights))
    return(stats::quantile(x, probs=probs, na.rm=na.rm))

  type <- match.arg(type)
  if(any(probs < 0 | probs > 1))
    stop("Probabilities must be between 0 and 1 inclusive")

  nams <- paste(format(round(probs * 100, if(length(probs) > 1) 
                             2 - log10(diff(range(probs))) else 2)), 
                "%", sep = "")

  i <- is.na(weights) | weights == 0
  if(any(i)) {
    x <- x[! i]
    weights <- weights[! i]
    }
  if(type == 'quantile') {
    w <- wtd.table(x, weights, na.rm=na.rm, normwt=normwt, type='list')
    x     <- w$x
    wts   <- w$sum.of.weights
    n     <- sum(wts)
    order <- 1 + (n - 1) * probs
    low   <- pmax(floor(order), 1)
    high  <- pmin(low + 1, n)
    order <- order %% 1
    ## Find low and high order statistics
    ## These are minimum values of x such that the cum. freqs >= c(low,high)
    allq <- stats::approx(cumsum(wts), x, xout=c(low,high), 
                   method='constant', f=1, rule=2)$y
    k <- length(probs)
    quantiles <- (1 - order)*allq[1:k] + order*allq[-(1:k)]
    names(quantiles) <- nams
    return(quantiles)
  } 
  w <- wtd.Ecdf(x, weights, na.rm=na.rm, type=type, normwt=normwt)
  structure(stats::approx(w$ecdf, w$x, xout=probs, rule=2)$y, 
            names=nams)
}


# Compute weighted empirical distribution function
# Source: https://github.com/harrelfe/Hmisc/blob/f80ecfb27e1deb88b3c6cc075c7252e3c577f66b/R/wtd.stats.s#L92
wtd.Ecdf <- function(x, weights=NULL, 
                     type=c('i/n','(i-1)/(n-1)','i/(n+1)'), 
                     normwt=FALSE, na.rm=TRUE)
{
  type <- match.arg(type)
  switch(type,
         '(i-1)/(n-1)'={a <- b <- -1},
         'i/(n+1)'    ={a <- 0; b <- 1},
         'i/n'        ={a <- b <- 0})

  if(! length(weights)) {
    ##.Options$digits <- 7  ## to get good resolution for names(table(x))
    oldopt <- options('digits')
    options(digits=7)
    on.exit(options(oldopt))
    cumu <- table(x)    ## R does not give names for cumsum
    isdate <- testDateTime(x)  ## 31aug02
    ax <- attributes(x)
    ax$names <- NULL
    x <- as.numeric(names(cumu))
    if(isdate) attributes(x) <- c(attributes(x),ax)
    cumu <- cumsum(cumu)
    cdf <- (cumu + a)/(cumu[length(cumu)] + b)
    if(cdf[1]>0) {
      x <- c(x[1], x);
      cdf <- c(0,cdf)
    }

    return(list(x = x, ecdf=cdf))
  }

  w <- wtd.table(x, weights, normwt=normwt, na.rm=na.rm)
  cumu <- cumsum(w$sum.of.weights)
  cdf <- (cumu + a)/(cumu[length(cumu)] + b)
  list(x = c(if(cdf[1]>0) w$x[1], w$x), ecdf=c(if(cdf[1]>0)0, cdf))
}


# Compute weighted frequency table
# Source: https://github.com/harrelfe/Hmisc/blob/f80ecfb27e1deb88b3c6cc075c7252e3c577f66b/R/wtd.stats.s#L130
wtd.table <- function(x, weights=NULL, type=c('list','table'), 
                      normwt=FALSE, na.rm=TRUE)
{
  type <- match.arg(type)
  if(! length(weights))
    weights <- rep(1, length(x))

  isdate <- testDateTime(x)  ## 31aug02 + next 2
  ax <- attributes(x)
  ax$names <- NULL
  
  if(is.character(x)) x <- as.factor(x)
  lev <- levels(x)
  x <- unclass(x)
  
  if(na.rm) {
    s <- ! is.na(x + weights)
    x <- x[s, drop=FALSE]    ## drop is for factor class
    weights <- weights[s]
  }

  n <- length(x)
  if(normwt)
    weights <- weights * length(x) / sum(weights)

  i <- order(x)  # R does not preserve levels here
  x <- x[i]; weights <- weights[i]

  if(anyDuplicated(x)) {  ## diff(x) == 0 faster but doesn't handle Inf
    weights <- tapply(weights, x, sum)
    if(length(lev)) {
      levused <- lev[sort(unique(x))]
      if((length(weights) > length(levused)) &&
         any(is.na(weights)))
        weights <- weights[! is.na(weights)]

      if(length(weights) != length(levused))
        stop('program logic error')

      names(weights) <- levused
    }

    if(! length(names(weights)))
      stop('program logic error')

    if(type=='table')
      return(weights)

    x <- all.is.numeric(names(weights), 'vector')
    if(isdate)
      attributes(x) <- c(attributes(x),ax)

    names(weights) <- NULL
    return(list(x=x, sum.of.weights=weights))
  }

  xx <- x
  if(isdate)
    attributes(xx) <- c(attributes(xx),ax)

  if(type=='list')
    list(x=if(length(lev))lev[x]
           else xx, 
         sum.of.weights=weights)
  else {
    names(weights) <- if(length(lev)) lev[x]
                      else xx
    weights
  }
}


# Determine if variable is a date, time, or date/time variable
# Source: https://github.com/harrelfe/Hmisc/blob/f80ecfb27e1deb88b3c6cc075c7252e3c577f66b/R/Misc.s#L1125
testDateTime <- function(x, what=c('either','both','timeVaries'))
{
  what <- match.arg(what)
  cl <- class(x)
  if(!length(cl))
    return(FALSE)

  dc <- c('Date', 'POSIXt','POSIXct','dates','times','chron')
  
  dtc <- c('POSIXt','POSIXct','chron')
  
  switch(what,
         either = any(cl %in% dc),
         both   = any(cl %in% dtc),
         timeVaries = {
           if('chron' %in% cl || 'Date' %in% cl) { 
             ## chron or S+ timeDate
             y <- as.numeric(x)
             length(unique(round(y - floor(y),13))) > 1
           }
           else length(unique(format(x,'%H%M%S'))) > 1
         })
}


# Check if all elements in a character vector are numeric
# Source: https://github.com/harrelfe/Hmisc/blob/f80ecfb27e1deb88b3c6cc075c7252e3c577f66b/R/Misc.s#L241
#' @exportS3Method NULL
all.is.numeric <- function(x, what=c('test','vector','nonnum'),
                           extras=c('.','NA'))
{
  what <- match.arg(what)
  x <- sub('[[:space:]]+$', '', x)
  x <- sub('^[[:space:]]+', '', x)
  xs <- x[x %nin% c('', extras)]
  if(! length(xs) || all(is.na(x)))
    return(switch(what, test = FALSE, vector=x, nonnum=x[0]))
  isnon <- suppressWarnings(! is.na(xs) & is.na(as.numeric(xs)))
  isnum <- ! any(isnon)
  # suppressWarnings below handles extras present in x
  switch(what,
         test   = isnum,
         vector = if(isnum) suppressWarnings(as.numeric(x)) else x,
         nonnum = xs[isnon])
}


# Find matching (or non-matching) elements
# Source: https://github.com/harrelfe/Hmisc/blob/f80ecfb27e1deb88b3c6cc075c7252e3c577f66b/R/in.operator.s#L1
"%nin%" <- function(x, table) match(x, table, nomatch = 0) == 0
