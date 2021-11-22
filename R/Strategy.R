#' Create Strategy object
#'
#' @return Strategy object
#' @export
#' @rdname Strategy
Strategy <- function(){
  this <- environment()
  with(this, {
    stats_init <- list()
    stats <- list()
    report_stats <- list()
    money <- 10^7
    created <- Sys.Date()
    expand_lookback <- FALSE
    version <- list(major = packageVersion('stratbuilder3gen')$major,
                    minor = packageVersion('stratbuilder3gen')$minor)
    # call for getting list of evaluated rules
    rules_calls <- NULL
    # if rules call needs to be recalculated at every evaluation
    recalc_rules_calls <- FALSE
    variables <- list()
    indicators <- list()
    futchains <- list()
    rules <- list()
    rule_constraints <- list()
    objects <- list()
    lookbacks <- list()
    paramset <- NULL
    backtest <- NULL
    pps <- list()
    program <- list()
    params <- list(
      all = list()
    )
  })

  class(this) <- c("Strategy")

  return(this)
}



#' Return version under which model was created
#'
#' @param this Strategy
#'
#' @return list
#' @export
#' @rdname getVersion
#' @method getVersion Strategy
getVersion.Strategy <- function(this){
  if(is.null(this$version)){
    this$version <- list(major = 1, minor = 0)
  }
  return(this$version)
}


#' @export
#' @rdname getSavedModels
#' @method getSavedModels Strategy
getSavedModels.Strategy <- function(this){
  this$save_strategy
}

#' Sets amount of money, that we have at the beginning
#'
#' @param this Strategy
#' @param x numeric, amount of money
#'
#' @export
#' @rdname setMoney
#' @method setMoney Strategy
setMoney.Strategy <- function(this, x){
  this$money <- x
  return(invisible(this))
}

#' Gets amount of money that we have at the beginning in specific backtest
#'
#' @param this Strategy
#' @param from character, name of folder in backtests
#'
#' @return numeric, amount of money
#' @export
#' @rdname getMoney
#' @method getMoney Strategy
getMoney.Strategy <- function(this){
  return(this[['money']])
}

#' @export
cloneObj <- function(obj){
  tmp <- tempfile()
  saveRDS(obj, tmp)
  readRDS(tmp)
}


#' Copy Strategy object
#'
#' @param this Strategy
#' @param clone_data logical, if TRUE then clone modelData object
#' @param clone_backtests logical, if TRUE then all backtests will be cloned
#' @param ... params
#'
#' @return Strategy
#' @export
#' @rdname cloneStrategy
#' @method cloneStrategy Strategy
cloneStrategy.Strategy <- function(this, clone_data = FALSE, clone_backtest = FALSE, ...){
  if(!clone_data){
    data <- getData(this)
    setData(this, NULL)
  }
  if(!clone_backtest){
    backtest <- this$backtest
    this$backtest <- NULL
  }
  this_cloned <- cloneObj(this)
  if(!clone_data){
    this$data <- data
    setData(this_cloned, data, only=TRUE)
  }
  if(!clone_backtest){
    this$backtest <- backtest
  }
  parent.env(this_cloned) <- parent.env(this)
  return(this_cloned)
}


#' Gets modelData object from Strategy object
#'
#' @param this Strategy
#'
#' @export
#' @rdname getData
#' @method getData Strategy
getData.Strategy <- function(this){
  return(this$data)
}


#' Set modelData object to Strategy object
#'
#' @param this Strategy
#' @param x modelData
#' @param clearBacktests logical, if true then all backtests will be erased
#' @param ... params
#' @param only logical, if it is true then method only change modelD field
#'
#' @export
#' @rdname setData
#' @method setData Strategy
setData.Strategy <- function(this, x, clearBacktest = TRUE, only=FALSE, ...){
  e <- this
  if(is.null(x)){
    e$data <- x
    return(invisible(this))
  }
  if(class(x)[1] == 'xts'){
    x <- model_from_xts(x)
  }
  e$data <- x
  if(!only){
    if(clearBacktest){
      e$backtest <- NULL
    }
    e$paramset$results <- NULL
    e$paramset$report <- NULL
  }
  return(invisible(this))
}


#' Expand lookback to the whole available data
#'
#' @param this Strategy
#' @param x logical
#'
#' @export
#' @rdname expandLookback
#' @method expandLookback Strategy
expandLookback.Strategy <- function(this, x){
  this$expand_lookback <- x
  return(invisible(this))
}


#' Gets dates by indexes from modelData
#'
#' @param this Strategy
#' @param indexes numeric vector or NULL
#'
#' @return vector of dates
#' @export
#' @rdname getDateByIndex
#' @method getDateByIndex Strategy
getDateByIndex.Strategy <- function(this, indexes = NULL){
  fun <- function(model, indexes = NULL){
    if(!is.null(indexes)){
      if(any(indexes < 0)){
        N <- model$nrow
        indexes[indexes < 0] <-  N + indexes[indexes < 0] + 1
      }
      return(getDateByIndex(model)[indexes])
    }else{
      return(getDateByIndex(model))
    }
  }
  return(fun(getData(this), indexes))
}


