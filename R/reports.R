#' @export
#' @method calcStat Strategy
#' @rdname calcStat
calcStat.Strategy <- function(this, s, start, end){
  args <- rlang::enexprs(s=s, start=start, end=end)
  eval(rlang::call2('precalcStat', this=quote(this), !!!args))
  start <- get_backtest_start_index(this, start)
  end <- get_backtest_end_index(this, end)
  period <- paste('per', start, end, sep = '_')
  if(!is.null(this$backtest$stats)){
    this$backtest$stats <- list()
  }
  if(is.null(this$backtest$stats[[period]])){
    this$backtest$stats[[period]] <- list()
  }
  calcStat_(this, s, start, end)
}




#' @method calcStat_ Strategy
#' @rdname calcStat_
calcStat_.Strategy <- function(this, s, start, end){
  period <- paste('per', start, end, sep = '_')
  if(!is.null(s[['depends']])){
    for(x in s[['depends']]){
      if(is.null(this$backtest$stats[[period]][[x]])){
        if(is.null(this$report_stats[[x]])){
          s1 <- acceptable_stats[[x]]
        }else{
          s1 <- this$report_stats[[x]]
        }
        calcStat_(this, s1, start, end)
      }
    }
  }
  data <- this$data
  value <- with(this$backtest$results, {
    with(this$backtest$stats[[period]],{
      environment(s$func) <- environment()
      s$func(this, start, end)
    })
  })
  if(s$general){
    this$backtest$results[[s$name]] <- value
  }else{
    this$backtest$stats[[period]][[s$name]] <- value
  }
  return(value)
}




get_switch_stats <- function(in_report=NULL){
  x <- quote(switch(x, a=))
  l <- list()
  for(name in names(acceptable_stats)){
    stat <- acceptable_stats[[name]]
    if(!is.null(in_report)){
      if(in_report != stat$in_report){
        next
      }
    }
    #print(name)
    if(!is.null(stat$keywords)){
      for(key in stat[['keywords']]){
        l[[key]] <- x[[3]]
      }
    }
    l[[name]] <- name
  }
  rlang::call2('switch', quote(x), !!!l)
}


#' Add statistic to report of strategy
#'
#' @param this modelStrategy
#' @param ... params, function that accept one argument of modelStrategy object, each argument in dots should be named
#'
#' @examples
#' \dontrun{
#' addToReport(this, double_sharpe=function(this, start, end){
#'    pnl <- getPnL(this)
#'    dates <- getDateByIndex(this, c(start, end))
#'    pnl <- pnl[paste0(dates[1], '/', dates[2])]
#'    sharpes <- rollapply(diff(pnl), width = 252, FUN=SharpeRatio.mean, by=30, align='right')  %>% na.omit
#'    SharpeRatio.mean(sharpes)
#' })
#' }
#'
#' @export
#' @rdname addToReport
#' @method addToReport Strategy
addToReport.Strategy <- function(this, ...){
  dots <- list(...)
  sw <- get_switch_stats(in_report = TRUE)
  for(i in seq_along(dots)){
    name <- names(dots[i])
    if(is.null(name)){
      name <- ''
    }
    if(class(dots[[i]]) == 'Stat'){
      s <- dots[[i]]
      if(name == ''){
        name <- s$name
      }
      this$report_stats[[name]] <- s
      next
    }
    if(name == ''){
      for(x in dots[[i]]){
        if(class(x) == 'Stat'){
          this$report_stats[[x$name]] <-x
        }else{
          res <- eval(pryr::substitute_q(sw, env = list(x=x)))
          if(!is.null(res)){
            s <-  Stats[[res]]
            this$report_stats[[s$name]] <- s
          }
        }
      }
    }else if(is.function(dots[[i]])){
      s <- Stat(name = name, in_report = TRUE, func = dots[[i]])
      this$report_stats[[s$name]] <- s
    }
  }
}


#' @export
get_backtest_start_index <- function(this, start){
  if(missing(start)){
    start <- 1
  }else if(class(start)[1] %in% c("character", 'Date', 'POSIXct', 'POSIXlt')){
    start <- which(as.Date(getDateByIndex(this)) >= as.Date(start))[1]
  }
  start <- max(start, this$backtest$activeField['start'])
  return(start)
}

#' @export
get_backtest_end_index <- function(this, end){
  if(missing(end)){
    end <- Inf
  }else if(class(end)[1] %in% c("character", 'Date', 'POSIXct', 'POSIXlt')){
    end <- which(as.Date(getDateByIndex(this)) <= as.Date(end) ) %>% tail(1)
  }
  end <- min(end, this$backtest$activeField['end'])
  return(end)
}

#' Get report of Strategy
#'
#' @param this Strategy
#' @param start numeric / Date / character, start of the period
#' @param end numeric / Date / character, end of the period
#'
#' @export
#' @method getReport Strategy
getReport.Strategy <- function(this, start, end, returns = 'tibble'){
  res <- calcStat(this, acceptable_stats$report, start, end)
  if(returns == 'tibble'){
    return(res %>% {tibble::as_tibble(.)})
  }
  return(res)
}

#' Get report of trades of Strategy
#'
#' This method returns data.frame, each row is statistic for a one trade
#'
#' @param this Strategy
#' @param start numeric / Date / character, start of the period
#' @param end numeric / Date / character, end of the period
#' @export
#' @method getTrades Strategy
getTrades.Strategy <- function(this, start, end){
  return(calcStat(this, acceptable_stats$trades, start, end))
}
