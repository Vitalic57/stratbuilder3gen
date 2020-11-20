#' @export
#' @rdname commission
setCommission <- function(this, q){
  UseMethod("setCommission", this)
}


#' @export
#' @rdname commission
getCommission <- function(this){
  UseMethod("getCommission", this)
}


#' @export
#' @rdname lookback
addLookback <- function(this, ...){
  UseMethod("addLookback", this)
}


#' @export
#' @rdname lookback
getLookbacks <- function(this){
  UseMethod("getLookbacks", this)
}


#' @rdname getVersion
getVersion <- function(this){
  UseMethod("getVersion", this)
}


#' @export
#' @rdname cloneStrategy
cloneStrategy <- function(this, ...){
  UseMethod("cloneStrategy", this)
}


#' @export
#' @rdname setData
setData <- function(this, ...){
  UseMethod("setData", this)
}


#' @export
#' @rdname getData
getData <- function(this){
  UseMethod("getData", this)
}

#' @export
#' @rdname setMoney
setMoney <- function(this, x){
  UseMethod("setMoney", this)
}


#' @export
#' @rdname getMoney
getMoney <- function(this){
  UseMethod("getMoney", this)
}


#' @export
#' @rdname addToReport
addToReport <- function(this, ...){
  UseMethod("addToReport", this)
}

#' @export
#' @rdname setParams
setParams <- function(this, ...){
  UseMethod("setParams", this)
}


#' @export
#' @rdname getParams
getParams <- function(this, type){
  UseMethod("getParams", this)
}


#' @export
#' @rdname stat
addStat <- function(this, ...){
  UseMethod("addStat", this)
}

#' @export
#' @rdname stat
removeStat <- function(this, name){
  UseMethod("removeStat", this)
}


#' @export
#' @rdname stat
reinitStat <- function(this){
  UseMethod("reinitStat", this)
}


#' @export
#' @rdname addObject
addObject <- function(this, ...){
  UseMethod("addObject", this)
}

#' Simulate strategy
#'
#' @param this Strategy
#' @param start numeric / Date / character, index of modelData where calculation starts
#' @param end numeric / Date / character, index of modelData whre calculation ends
#' @param closeAllPositionsAtTheEnd bool, if true, then strategy closes all positions at the end of simulation
#' @param index numeric, index of backtesting param
#' @param install logical, install params in this model or create new one
#' @param calc_reports logical whether calculate reports right after backtest or not
#' @param ... args
#'
#' @export
#' @rdname perform
perform <- function(this,
                    start,
                    end,
                    closeAllPositionsAtTheEnd,
                    index,
                    install,
                    calc_reports,
                    ...){
  UseMethod("perform", this)
}

#' @export
#' @rdname ProgramPart
addProgramPart <- function(this, ...){
  UseMethod("addProgramPart", this)
}


#' @export
#' @rdname ProgramPart
getProgramParts <- function(this, ...){
  UseMethod("getProgramParts", this)
}


#' @export
#' @rdname ProgramPart
getProgramPart <- function(this, part, recalc){
  UseMethod("getProgramPart", this)
}


#' @export
#' @rdname Signal
addIndicators <- function(this, ...){
  UseMethod("addIndicators", this)
}


#' @export
#' @rdname Signal
addIndicator <- function(this, ...){
  UseMethod("addIndicators", this)
}


#' @export
#' @rdname Signal
addRule <- function(this, ...){
  UseMethod("addRule", this)
}


#' @export
#' @rdname Signal
addRuleConstraint <- function(this, ...){
  UseMethod("addRuleConstraint", this)
}


#' @export
#' @rdname Signal
getIndicators <- function(this){
  UseMethod("getIndicators", this)
}


#' @export
#' @rdname Signal
getRules <- function(this, pathwise){
  UseMethod("getRules", this)
}


#' @export
#' @rdname Signal
getRuleConstraints <- function(this){
  UseMethod("getRuleConstraints", this)
}


#' @export
#' @rdname Signal
getRule <- function(this, name){
  UseMethod("getRule", this)
}

#' @export
#' @rdname Signal
getSignals <- function(this){
  UseMethod("getSignals", this)
}

#' @export
#' @rdname Signal
removeRule <- function(this, name){
  UseMethod("removeRule", this)
}

#' @export
#' @rdname Signal
removeIndicator <- function(this, name){
  UseMethod("removeIndicator", this)
}


#' @export
#' @rdname addVariables
addVariables <- function(this, ...){
  UseMethod("addVariables", this)
}


#' @export
plotPnL <- function(this, ...){
  UseMethod("plotPnL", this)
}

#' @export
getPnL <- function(this){
  UseMethod("getPnL", this)
}

#' @export
getTradingLog <- function(this){
  UseMethod("getTradingLog", this)
}

#' @export
plotCalendar <- function(this, ...){
  UseMethod("plotCalendar", this)
}

#' @export
plotDrawdowns <- function(this, ...){
  UseMethod("plotDrawdowns", this)
}

#' @export
plotParamset <- function(this, ...){
  UseMethod("plotParamset", this)
}

#' @export
plotReturns <- function(this, ...){
  UseMethod("plotReturns", this)
}


#' @export
getReport <- function(this, start, end){
  UseMethod("getReport", this)
}

#' @export
getTrades <- function(this, start, end){
  UseMethod("getTrades", this)
}

#' @export
addDistribution <- function(this,
                            component.type,
                            component.label,
                            variable,
                            label){
  UseMethod("addDistribution", this)
}

#' @export
addDistributionConstraint <- function(this,
                                       expr,
                                       label
){
  UseMethod("addDistributionConstraint", this)

}

#' @export
getDistributions <- function(this){
  UseMethod("getDistributions", this)
}

#' @export
applyParamset <- function(this, ...){
  UseMethod("applyParamset", this)
}

#' @export
getBacktestResults <- function(this, ...){
  UseMethod("getBacktestResults", this)
}


#' @export
installParams <- function(this, param.combo){
  UseMethod("installParams", this)
}

#' Return Saved Models
#'
#' @param this Strategy
#' @export
getSavedModels <- function(this){
  UseMethod('getSavedModels', this)
}


#' Calculate statistic
#'
#' Statistic and all dependent statistics will be calculated
#'
#' @param this Strategy
#' @param s Stat
#' @param start numeric, start of the period
#' @param end numeric, end of the period
#' @rdname commission
#' @export
calcStat <- function(this, s, start, end){
  UseMethod("calcStat", this)
}
