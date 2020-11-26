#' Aggregate results from backtests
#'
#' @param this Strategy
#' @rdname calcBacktestResults
#' @method calcBacktestResults Strategy
calcBacktestResults.Strategy <- function(this){
  res_list <- this$paramset[['results']]
  this$paramset[['report']] <- lapply(res_list, function(x){
    tibble::as_tibble(c(list(index = rownames(x$param.combo)), as.list(x$param.combo),  x$report))
  }) %>%
    Reduce('rbind', .)
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
  if(is.null(this$paramset[['report']]) || recalc){
    calcBacktestResults(this)
  }
  return(this$paramset[['report']])
}
