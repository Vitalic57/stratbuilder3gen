#' Aggregate results from backtests
#'
#' @param this Strategy
#' @rdname calcBacktestResults
#' @method calcBacktestResults Strategy
calcBacktestResults.Strategy <- function(this){
  res_list <- this$paramset[['results']]
  this$paramset[['report']] <- lapply(res_list, function(x){
    tibble::as_tibble(c(list(index = rownames(x[['param.combo']])), as.list(x[['param.combo']]),  x[['report']]))
  }) %>% {do.call(dplyr::bind_rows, .)}
}


#' Returns aggregated results of backtests
#'
#' @param this Strategy
#' @param recalc bool, if true then it rerun calcBacktestResults function
#'
#' @return tibble
#' @rdname getBacktestResults
#' @method getBacktestResults Strategy
#' @export
getBacktestResults.Strategy <- function(this, recalc=TRUE){
  if(is.null(this[['paramset']][['report']]) || recalc){
    calcBacktestResults(this)
  }
  return(this[['paramset']][['report']])
}


#' Returns aggregated PnL from applyParamset backtests
#' 
#' This function would work only if applyParamset was called with save.pnls = TRUE
#'
#' @param this Strategy
#' @param ind numeric, one or multiple indexes of backtest from applyParamset
#'
#' @return xts
#' @rdname getBacktestPnL
#' @method getBacktestPnL Strategy
#' @export
getBacktestPnL.Strategy <- function(this, ind = NULL){
  if(is.null(ind)){
    ind <- names(this[['paramset']][['results']])
  }else{
    ind <- as.character(ind)
  }
  if(!is.null(this[['paramset']][['results']][[1]][['pnl']])){
    res <- do.call(cbind, lapply(this[['paramset']][['results']][ind], '[[', 'pnl')) %>% 
      set_colnames(ind)
  }else if(!is.null(this[['paramset']][['results']][[1]][['backtest']])){
    init_backtest <- this$backtest
    res <- do.call(cbind, lapply(this[['paramset']][['results']][ind], function(l){
      this[['backtest']] <- l[['backtest']]
      getPnL(this)
    })) %>% 
      set_colnames(ind)
    this$backtest <- init_backtest
  }else{
    stop('No PnL info in paramset results')
  }
  return(res)
}


