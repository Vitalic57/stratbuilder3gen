#' @import datastorage3pub
NULL

#' Current price
#' 
#' Numeric vector. 
#' It is equivalent to \code{data[[data$price_table]][i, ]}. 
#' So it is equal to close price on given moment inside backtester cycle. 
#' Often used in rule's position/position_const block.
#' 
#' @export
cur_price <- NULL

#' Size of current positions
#' 
#' Numeric vector.
#' It is equal to the number of shares bought long or sold short at the moment inside backtester cycle. 
#'
#' @export
position_last <- NULL

#' Current unrealized profit or loss
#' 
#' Numeric vector.
#' It is equal to profit or loss for each asset in the model at the moment inside backtester cycle.
#' Often used in rule's expr block for takeprofit or stoploss rules. 
#' So if one want to get full pnl, one should use \code{sum(unrealized_money_last)}.
#'
#' @export
unrealized_money_last <- NULL

#' Initial amount of money
#' 
#' Numeric scalar, constant.
#' It is equivalent to \code{getMoney(this)}. As it is scalar it is much faster.
#' @seealso \code{\link{money_last}}
#' 
#' @export
money_init <- NULL

#' Current amount of money.
#' 
#' Numeric scalar, variable.
#' Total price of portfolio at the moment that includes cash and all bought long and sold short assets.
#' It can be used interchangeably with \link{money_init} if one wants to use compounded returns.
#' 
#' @seealso \code{\link{money_init}}
#' @export
money_last <- NULL

#' Current working indexes.
#' 
#' Numeric vector.
#' Indexes for passing into main tables as adjusted, close, high, low, splits and dividends.
#' Often used in indicator's expr.
#' 
#' Because vector operations in R is working much faster than iterating through cycle we prefer using them for calculating indicators and rules. 
#' Some indicators may depend on others and others can be for example constants that recalculated every p iteration. 
#' For instance, indicator named N, calculated somehow, that returns integer more than 0 and recalculated every 100 iterations. Next indicator named ma,
#' calculated as SMA(closing prices, N). There multiple ways to calculate it:
#' 1. We can on each iteration pass last N close prices and get scalar ma, then use it in pathwise=TRUE rules. 
#' The first drawback there will be 100 calls to SMA and backtest will be slow. 
#' The second drawback that we tied to use pathwise=TRUE rules, so expr will be calculated on each iteration, that decrease speed of backtest too.
#' 1. We can pass all the closing prices to SMA. But we only need this indicator for the next 100 points and then N will change, so value of SMA should change. 
#' 1. And the last only pass to indicator last N - 1 points, current point and next 100. So we don't waste resources and calculate only what wa need. 
#' 
#'  range is responsible for giving indexes for the last case. So formula will be \code{SMA(data$adjusted[range,], N)}. 
#'  If indicator calculated so, we get vector sized as length(range). If one want to get current value of ma, one should use ii scalar index. 
#'  It works as current period for indicators that calculated on range indexes.
#' 
#' @export
range <- NULL

#' Data object.
#' 
#' Data. That object is passed through setData method. It contains prices and dates.
#' It can include such tables adjusted, close, high, low, dividends, splits and vector dates.
#' Often used in indicator's expr.
#' 
#' @export
data <- Data()

#' Strategy object.
#' 
#' Strategy. 
#'
#' @export
this <- NULL

#' Current index for range based indicators.
#' 
#' Numeric scalar, changes over time.
#' Often used in rule's expr when pathwise=TRUE mode or in rule's position/position_const. 
#' 
#' @seealso \code{\link{range}, \link{i}}
#' 
#' @export
ii <- NULL

#' Current index for main tables in data object.
#' 
#' Numeric scalar, changes over time.
#' Often used in rule's position. 
#' 
#' @seealso \code{\link{data}, \link{ii}}
#'
#' @export
i <- NULL

#' Vector of dates.
#' 
#' Date vector. Vector copied from data$dates.
#' 
#' @seealso \code{\link{data}}
#' @export
dates <- NULL

#' Change of position from start of period to the end.
#' 
#' Numeric vector. How many of each asset were sold or bought in that period.
#' It used only in commission block.
#' 
#' @export
pos_change <- NULL

#' Indicator of corporative event
#' 
#' This variable should be used in indicator's lookforward. If used then indicator will be recalculated as soon as corporative event occured.
#' 
#' @export
corp_event <- NULL

.onLoad <- function(libname, pkgname){
  options('tibble.width'= Inf)
}

.onUnload <- function (libpath) {
  library.dynam.unload("stratbuilder3gen", libpath)
}
