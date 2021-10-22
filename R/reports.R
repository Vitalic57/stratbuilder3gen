#' @export
#' @method calcStat Strategy
#' @rdname calcStat
calcStat.Strategy <- function(this, s, start, end, recalc=FALSE, ...){
  if(is.function(s)){
    ss <- Stat(func = s, ...)
  }else{
    ss <- s
  }
  start <- get_backtest_start_index(this, start)
  end <- get_backtest_end_index(this, end)
  if(start > end){
    return(NULL)
  }
  period <- paste('per', start, end, sep = '_')
  if(is.null(this$backtest$stats)){
    this$backtest$stats <- list()
  }
  if(is.null(this$backtest$stats[[period]])){
    this$backtest$stats[[period]] <- list()
  }
  if(!recalc){
    if(!ss$general && ss$name %in% names(this$backtest$stats[[period]])){
      return(this$backtest$stats[[period]][[ss$name]])
    }else if(ss$general && ss$name %in% names(this$backtest$results)){
      return(this$backtest$results[[ss$name]])
    }
  }else{
    eraseStat(this, ss, start, end)
  }
  precalcStat(this, ss, start, end, recalc)
  if(!ss$general && ss$name %in% names(this$backtest$stats[[period]])){
    return(this$backtest$stats[[period]][[ss$name]])
  }else if(ss$general && ss$name %in% names(this$backtest$results)){
    return(this$backtest$results[[ss$name]])
  }
  calcStat_(this, ss, start, end)
}




#' @method calcStat_ Strategy
#' @rdname calcStat_
#' @export
calcStat_.Strategy <- function(this, s, start, end){
  period <- paste('per', start, end, sep = '_')
  if(!is.null(s[['depends']])){
    nms <- sapply(this$report_stats, "[[", 'name')
    for(x in s[['depends']]){
      ind <- which(nms == x)
      if(length(ind) == 0){
        s1 <- acceptable_stats[[x]]
      }else{
        s1 <- this$report_stats[[ind]]
      }
      if(is.null(s1)){
        stop(paste('No such stat', x))
      }
      if(!s1$general && is.null(this$backtest$stats[[period]][[x]])){
        calcStat_(this, s1, start, end)
      }else if(s1$general && is.null(this$backtest$results[[x]])){
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
    this$backtest$results[s$name] <- list(value)
  }else{
    this$backtest$stats[[period]][s$name] <- list(value)
  }
  return(value)
}



#' @method eraseStat Strategy
#' @rdname eraseStat
eraseStat.Strategy <- function(this, s, start, end){
  period <- paste('per', start, end, sep = '_')
  if(!is.null(s[['depends']])){
    for(x in s[['depends']]){
      if(!is.null(this$backtest$stats[[period]][[x]])){
        if(is.null(this$report_stats[[x]])){
          s1 <- acceptable_stats[[x]]
        }else{
          s1 <- this$report_stats[[x]]
        }
        eraseStat(this, s1, start, end)
      }
    }
  }
  if(!s$general){
    this$backtest$stats[[period]][[s$name]] <- NULL
  }
  return(invisible(NULL))
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
#' @param this Strategy
#' @param ... Stats or function objects. Function should have arguments as follows function(this, start, end), where
#'
#' this - Strategy
#'
#' start - numeric index of starting point
#'
#' end - numeric index of ending point
#'
#' @examples
#' \dontrun{
#' addToReport(this, Stats$sharpe,  double_sharpe=function(this, start, end){
#'    pnl <- getPnL(this)
#'    dates <- getDateByIndex(this, c(start, end))
#'    pnl <- pnl[paste0(dates[1], '/', dates[2])]
#'    sharpes <- rollapply(diff(pnl), width = 252, FUN=SharpeRatio.mean, by=30, align='right')  %>% na.omit
#'    sharpes %>% {mean(.) / sd(.) * sqrt(12)}
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
      for(j in seq_along(dots[[i]])){
        x <- dots[[i]][[j]]

        if(class(x) == 'Stat'){
          if(is.null(names(dots[[i]])) || names(dots[[i]])[j] == ''){
            name_ <- x$name
          }else{
            name_ <- names(dots[[i]])[j]
          }
          this$report_stats[[name_]] <-x
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
  return(invisible(this))
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


#' Get report of trades of Strategy
#'
#' This method returns data.frame, each row is statistic for a one trade
#'
#' @param this Strategy
#' @param start numeric / Date / character, start of the period
#' @param end numeric / Date / character, end of the period
#' @export
#' @method getTrades Strategy
#' @rdname getTrades
getTrades.Strategy <- function(this, start, end){
  return(calcStat(this, acceptable_stats$trades, start, end))
}
