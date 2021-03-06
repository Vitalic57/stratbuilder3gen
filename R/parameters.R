params_switcher <- function(x){
  type <- switch (tolower(x),
                  all='all',
                  rule = ,
                  rules = 'rules',
                  indicator =,
                  indicators = 'indicators',
                  ruleconstraint=,
                  rule_constraints=,
                  rules_constraints= 'rule_constraints',
                  pp = ,
                  pps = ,
                  programparts =,
                  programpart =,
                  program = 'pps',
                  com=,
                  commission=,
                  coms=,
                  commissions='commission',
                  var =  ,
                  vars =,
                  variables = 'variables'

  )
  return(type)
}


#' Set list of parameters
#'
#' Here you can define parameters for rules, indicators, program part and managers of position
#'
#' @param type character, it can be equal to rules, indicators, pp, pm, com, pymodel, all
#' @param args list, define your params here
#' @param this Strategy
#' @param ... args
#'
#' @export
#' @examples
#' \dontrun{
#' setParams(this,
#'        type = 'rules',
#'        args = list(n = 5, a = 2)
#' )
#' }
#' @rdname setParams
#' @method setParams Strategy
setParams.Strategy <- function(this,
                               args=list(),
                               type = 'all',
                               ...){
  type <- params_switcher(type)
  this$params[[type]] <- c(args, list(...))
  return(invisible(this))
}


#' Get list of parameters according to type.
#'
#' @param type character, it can be equal to rules, indicators, pp or pm
#' @param this Strategy
#'
#' @export
#' @rdname getParams
#' @method getParams Strategy
getParams.Strategy <- function(this, type){
  type <- params_switcher(type)
  return(c(this$params[['all']], this$params[[type]]))
}



#' This variable will be included to program and after calling perform function  it will be updated
#'
#' @param this Strategy
#' @param ... params
#' @export
#' @rdname stat
#' @method addStat Strategy
addStat.Strategy <- function(this, ...){
  l <- list(...)
  for(name in names(l)){
    this$stats_init[[name]] <- l[[name]]
    this$stats[[name]] <- l[[name]]
  }
  return(invisible(this))
}

#' Deletes variable from stats
#'
#' @param this Strategy
#' @param name character
#' @export
#' @rdname stat
#' @method removeStat Strategy
removeStat.Strategy <- function(this, name){
  this$stats_init[[name]] <- NULL
  this$stats[[name]] <- NULL
}


#' Reinit all variables in stats
#'
#' @param this Strategy
#' @export
#' @rdname stat
#' @method reinitStat Strategy
reinitStat.Strategy <- function(this){
  if('stats' %in% names(this)){
    for(name in names(this$stats)){
      this$stats[[name]] <- this$stats_init[[name]]
    }
  }
}



#' Add user-defined objects to Strategy for future usage in backtest
#'
#' @param this Strategy
#' @param ... named args
#'
#' @export
#' @rdname addObject
#' @method addObject Strategy
addObject.Strategy <- function(this, ...){
  dots <- list(...)
  if(is.null(names(dots)) || any(names(dots) == '')){
    stop("object must have a name.")
  }
  for(name in names(dots)){
    this[['objects']][[name]] <- dots[[name]]
  }
}

