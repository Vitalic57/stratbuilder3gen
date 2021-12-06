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
    tradeTime <- list()
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


#' @export
#' @method format Strategy
format.Strategy <- function(strategy){
  def_strategy <- Strategy()
  var_list <- list('variables' = 'addVariables', 
                   'objects' = 'addObjects',
                   'stats_init' = 'addStats')
  nextline <- ' %>%\n'
  text <- paste0('Strategy()', nextline)
  
  # params
  if(length(strategy$params) > 0){
    if(length(strategy$params$all) > 0){
      text <- paste0(text, 'setParams(\n')
      for(name in names(strategy$params$all)){
        text <- paste0(text, "\t", name, " = ", paste(deparse(strategy[['params']][['all']][[name]]), collapse = '\n\t'), ',\n')
      }
      text <- paste0(substr(text, 1, nchar(text) - 2), ')', nextline)
    }
    for(name in names(strategy$params)){
      if(name == 'all'){
        next
      }
      text <- paste0(text, 'setParams(\n',
                     "\ttype = '", name, "',\n",
                     "\targs = ", paste(deparse(strategy[['params']][[name]]), collapse = '\n\t'), "\n)", nextline
                     )
    }
  }
  
  # signals
  for(sig in getSignals(strategy)){
    text <- paste0(text, format(sig), nextline)
  }
  
  # var list
  {
    for(vr in names(var_list)){
      if(length(strategy[[vr]]) > 0){
        text <- paste0(text, var_list[[vr]],  "(\n")
        for(name in names(strategy[[vr]])){
          text <- paste0(text, "\t", name, " = ", paste(deparse(strategy[[vr]][[name]]), collapse = '\n\t'), ',\n')
        }
        text <- paste0(substr(text, 1, nchar(text) - 2), ')', nextline)
      }
    }
  }
  
  # program parts
  if(length(strategy$pps) > 0){
    include_name <- length(strategy$pps) > 1
    for(pp in strategy$pps){
      text <- paste0(text, "addProgramPart(\n")
      if(include_name){
        text <- paste0(text, "\tname = ", pp[['name']],',\n')
      }
      for(name in names(pp[['evolution']])){
        text <- paste0(text, "\t", name, " = ", paste(deparse(pp[['evolution']][[name]]), collapse = '\n\t'), ',\n')
      }
      text <- paste0(substr(text, 1, nchar(text) - 2), ')', nextline)
    }
  }
  
  # trade time
  if(length(strategy$tradeTime) > 0){
    for(tp in names(strategy$tradeTime)){
      for(i in seq_along(strategy$tradeTime[[tp]])){
        text <- paste0(text, "addTradeTime(",
                       "type='", tp, "', '", 
                       sec_to_tstr(strategy$tradeTime[[tp]][[i]][[1]]), "', '",
                       sec_to_tstr(strategy$tradeTime[[tp]][[i]][[2]]), "')", nextline)
      }
    }
  }
  
  # constants
  {
    var_fun <- list(
      expand_lookback = "expandLookback",
      money = "setMoney",
      commission_quote = 'setCommission'
    )
    for(name in names(var_fun)){
      if(is.null(strategy[[name]])){
        next
      }
      if(length(def_strategy[[name]]) == 0 && length(strategy[[name]]) == 0){
        next
      }
      tryCatch({
        if(strategy[[name]] == def_strategy[[name]]){
          next
        }
      }, error = function(e){})
      text <- paste0(text, var_fun[[name]], "(", paste0(deparse(strategy[[name]]), collapse = '\n'), ")", nextline)
    }
  }
  
  # report stats
  if(length(strategy$report_stats) > 0){
    text <- paste0(text, 'addToReport(\n')
    for(name in names(strategy$report_stats)){
      if(name == strategy$report_stats[[name]][['name']]){
        text <- paste0(text, "\t", 'Stats$', name, ',\n')
      }else{
        text <- paste0(text, "\t", name, " = ", 'Stats$', strategy$report_stats[[name]][['name']], ',\n')
      }
    }
    text <- paste0(substr(text, 1, nchar(text) - 2), ')', nextline)
  }
  
  # distributions
  {}
  
  # distribution constraints
  if(length(strategy$paramset$constraints)){
    text <- paste0(text, 'addDistributionConstraint(\n')
    for(i in seq_along(strategy$paramset$constraints)){
      text <- paste0(text, '\t', paste(deparse(strategy$paramset$constraints[[i]][['expr']]), collapse = '\n'), ",\n")
    }
    text <- paste0(substr(text, 1, nchar(text) - 2), ')', nextline)
  }
  
  substr(text, 1, nchar(text) - nchar(nextline))
}

#' @export
#' @method print Strategy
print.Strategy <- function(strategy){
  cat(format(strategy))
}

#' @export
is.Strategy <- function(strategy){
  inherits(strategy, 'Strategy')
}

