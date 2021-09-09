#' Get pnl as xts series
#'
#' This method should be called only after backtest is done
#'
#' @param this Strategy
#' @param ... params
#'
#' @return getPnL.Strategy: xts series
#' @export
#' @rdname getPnL
#' @method getPnL Strategy
getPnL.Strategy <- function(this,  ...){
  return(plotPnL(this, return_type = 'data', ...))
}

#' Get all transactions
#'
#' Return data.frame containing columns:
#' Date - position change date
#' Rule_name - name rule
#' Instrument - traiding instrument
#' Change_pos - position change number
#' @param this Strategy
#' @param start numeric / Date / character, start of the period
#' @param end numeric / Date / character, end of the period
#'
#' @return getTradingLog: data.frame
#' @export
#' @rdname getTradingLog
#' @method getTradingLog Strategy
getTradingLog.Strategy <- function(this, start, end){
  start <- get_backtest_start_index(this, start)
  end <- get_backtest_end_index(this, end)
  calcStat(this, acceptable_stats$trading_log, start, end)
}




