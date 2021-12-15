tstr_to_sec <- function(t_str) {
  #"09:00:00" to sec of day
  as.numeric(as.POSIXct(paste("1970-01-01", t_str), "UTC")) %% 86400L
}

sec_to_tstr <- function(sec){
  hours <- sec %/% (60*60)
  if(hours == 0){
    hours <- '00'
  }
  mins <- (sec%%(60*60)) %/% 60
  if(mins < 10 ){
    mins <- paste0(0,mins)
  }
  secs <- sec %% 60
  if(secs  < 10){
    secs <- paste0(0,secs)
  }
  return(paste0(hours,':',mins,':',secs))
}


#' Adds time when trading is permitted
#'
#' @param this Strategy
#' @param type character, one of enter/open (when may open position),
#' close (when must close position),
#' rebalance (when may rebalance position)
#' @param ... characters of start and end dates, each element is a vector of two strings that represents time of start and end
#'
#' @rdname addTradeTime
#' @method addTradeTime Strategy
#' @export
#' @example
#' \dontrun{
#' # Open position only between 10 and 12, 14:00 and 14:30
#' addTradeTime(this, 'open', c('10:00:00','12:00:00'), c('14:00:00', '14:30:00'))
#' # Close all positions and don't open between 10 and 12
#' addTradeTime(this, 'close', '10:00:00','12:00:00')
#' # Close all positions after 20:00
#' addTradeTime(this, 'close', '20:00:00')
#' }
addTradeTime.Strategy <- function(this, type, ...){
  #type can be open or close
  #l is time of start and end
  ind <- switch(type,
                enter =,
                open = 'enter',
                close = 'close',
                rebalance = 'rebalance',
                {
                  return()
                })
  l <- rlang::enexprs(...)
  if(is.null(this[['tradeTime']][[ind]])){
    this[['tradeTime']][[ind]] <- list()
  }
  this[['tradeTime']][[ind]][[length(this[['tradeTime']][[ind]]) + 1]] <- l
  return(invisible(this))
}


num_to_tstr <- function(this, x){
  vapply(x, function(y){
    y <- eval(y, envir = getParams(this, 'all'))
    if(is.numeric(y)){
      y <- paste0(y %/% 1, ":", ((y %% 1) * 60) %/% 1)
    }
    y
  }, FUN.VALUE = character(1))
}


#' Gets tarding time of strategy
#'
#' @param this Strategy
#' @param type character, one of  c('all', 'enter', 'open', 'close', 'rebalance')
#'
#' @rdname getTradeTime
#' @method getTradeTime Strategy
#' @export
getTradeTime.Strategy <- function(this, type = 'all'){
  tradeTime <- list()
  for(ind in names(this[['tradeTime']])){
    for(l in this[['tradeTime']][[ind]]){
      if(all(vapply(l, length, numeric(1)) == 2)){
        for(i in seq_len(length(l))){
          tradeTime[[ind]][[length(tradeTime[[ind]]) + 1]] <- lapply(num_to_tstr(this, l[[i]]), tstr_to_sec)
        }
      }else if(length(l) == 2 && all(vapply(l, length, numeric(1)) == 1)){
        tradeTime[[ind]][[length(tradeTime[[ind]]) + 1]] <- lapply(num_to_tstr(this, l[1:2]), tstr_to_sec)
      }else if(length(l) == 1 && length(l[[1]]) == 1){
        tradeTime[[ind]][[length(tradeTime[[ind]]) + 1]] <- lapply(num_to_tstr(this, list(l[[1]], '23:59:59')), tstr_to_sec)
      }
    }
  }
  
  switch(type,
         all = {
           tradeTime
         },
         enter =,
         open = {
           tradeTime[['enter']]
         },
         close = {
           tradeTime[['close']]
         },
         rebalance= {
           tradeTime[['rebalance']]
         }
  )
}

#' Remove trading time by type
#'
#' @param this Strategy
#' @param type character, one of c('all', 'enter', 'open', 'close', 'rebalance')
#'
#' @rdname clearTradeTime
#' @method clearTradeTime Strategy
#' @export
clearTradeTime.Strategy <- function(this, type = 'all'){
  switch(type,
         all = {
           this$tradeTime <- list()
         },
         enter =,
         open = {
           this$tradeTime[['enter']] <- NULL
         },
         close = {
           this$tradeTime[['close']] <- NULL
         },
         rebalance = {
           this$tradeTime[['rebalance']] <- NULL
         }
  )
}

#' Prints trading time in format of time
#'
#' @param this Strategy
#' @param type character, one of c('all', 'enter', 'open', 'close', 'rebalance')
#'
#' @rdname printTradeTime
#' @method printTradeTime Strategy
#' @export
printTradeTime.Strategy <- function(this, type = 'all'){
  ind <- switch(type,
                all = {
                  names(getTradeTime(this))
                },
                enter =,
                open = {
                  'enter'
                },
                close = {
                  'close'
                },
                rebalance = 'rebalance',
                {
                  return()
                })
  x <- lapply(ind, function(x){
    print(paste0(x, ' trade time:'))
    x <- lapply(this$tradeTime[[x]], function(l){
      print(paste0('start : ', sec_to_tstr(l[[1]]), ', end : ', sec_to_tstr(l[[2]])))
    })
  })
}
