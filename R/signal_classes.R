#'  Signal constructor
#'
#'  Base class for indicators, rules and more
#'
#' @param expr expression
#' @param name character, name of signal
#' @param lookback numeric / expression, how many periods is needed for calculation signal
#' @param args list, parameters for a signal
#' @param ... args passed to subclasses
#'
#' @export
#' @rdname SignalClass
Signal <- function(expr,
                   name,
                   lookback = 0,
                   args = list(),
                   ...){
  this <- environment()
  parent.env(this) <- parent.frame()
  expr <- rlang::enexpr(expr)
  lookback <- rlang::enexpr(lookback)
  env <- new.env()
  list2env(list(...), envir = this)
  class(this) <- 'Signal'
  return(this)
}

# @param history logical, whether it expressed as matrix of previous and future values or expressed as statistic that recalculated
# when lookforward triggers

#' Indicator constructor
#'
#' Indicators serves for calculating rules
#'
#' @param lookforward numeric / expression. If it is numeric then after that number of points all indicators will be recalculated.
#' If it is expression then it should return logical. It will indicate recalculate indicators or not.
#' @param vars character vector, which names should be exported from expr
#' @param ... args passed to Signal
#'
#' @export
#' @rdname IndicatorClass
Indicator <- function(lookforward=Inf,
                      # history=TRUE,
                      vars=NULL,
                      ...){
  signal <- Signal(...)
  with(signal,{
    lookforward <- rlang::enexpr(lookforward)
    # history <- history
    updated <- TRUE
    vars <- vars
  })
  class(signal) <- c('Indicator', class(signal))
  return(signal)
}


#' Rule constructor
#'
#' This class serves for describing moments when model should open position or exit from it.
#'
#' @param type character, 'exit' or 'enter'
#' @param block character, it is needed for combining bunch of rules in one namespace. For example
#' rule with type exit will be triggered only if it has the same block variable as entry rule.
#' @param pathwise logical, whether expr will be calculated on each iteration or not. In other words if pathwise is FALSE
#' then expr will be calculated once with indicators, it should return logical vector, then in each iteration element of this
#' vector will be used, if pathwise is TRUE then expr will be calculated at each iteration, it should return logical scalar.
#' @param position numeric vector, what position for each instrument should be at each iteration when model in position
#' @param position_const numeric vector, what position for each instrument should be at entry to position
#' @param price numeric vector, what prices should be used for change position
#' @param reopen logical, whether rule can be reopened on the same bar
#' @param ... args passed to Signal
#'
#' @export
#' @rdname RuleClass
Rule <- function(type='enter',
                 block,
                 pathwise = FALSE,
                 position = NULL,
                 position_const = NULL,
                 price= NULL,
                 on_success=NULL,
                 reopen=FALSE,
                 ...){
  if(!type %in% c('enter','exit')){
    stop('wrong type! It must be enter or exit')
  }
  position <- rlang::enexpr(position)
  position_const <- rlang::enexpr(position_const)
  if(type =='enter' && is.null(position) && is.null(position_const) ){
    position_const <- quote(floor(getMoney(this) / data$mat[[data$price_table]][i,] / ncol(data$mat[[data$price_table]])))
  }
  dots <- rlang::enexprs(...)
  dots[['name']] <- gsub('\\.','_', dots[['name']])
  if(dots[['name']] == 'init'){
    stop('name of rule can not be equal to "init", this name is reserved')
  }
  if(missing(block) && type == 'enter'){
    block <- dots[['name']]
  }else if(missing(block) && type == 'exit'){
    block <- 'all'
  }
  if(block == 'enter'){
    stop('Block can not be equal to "enter"')
  }
  if(type == 'enter' && block == 'all'){
    stop('Block can not be equal to "all" when type is "enter"')
  }
  if(type == 'enter'){
    if(block == 'all'){
      stop("block can't be equal to all when type is enter")
    }
  }
  signal <- Signal(...)
  with(signal, {
    price <- rlang::enexpr(price)
    position <- position
    position_const <- position_const
    on_success <- rlang::enexpr(on_success)
    pathwise <- pathwise
    block <- block
    type <- type
    reopen <- reopen
  })
  class(signal) <- c('Rule', class(signal))
  return(signal)
}


#' RuleConstraint constructor
#'
#' This class is needed to add constraints for expr for rules. It can be treated as operator AND for expr of rules.
#'
#' @param rules character vector, names of rules
#' @param rule_type character, type is 'exit' or 'enter'. If it is not missed it will be applied to all rules in this group,
#' rules argument will be ignored.
#' @param ... args passed to Signal
#'
#' @export
#' @rdname RuleConstraintClass
RuleConstraint <- function(
                 rules=NULL,
                 rule_type=NULL,
                 ...){
  if(missing(rules) && missing(rule_type)){
    stop("One of rules or rule_type must be defined")
  }
  if(!missing(rule_type) && !rule_type %in% c('enter','exit')){
    stop('wrong rule_type! It must be enter or exit')
  }
  dots <- rlang::enexprs(...)
  dots[['name']] <- gsub('\\.','_', dots[['name']])
  signal <- Signal(...)
  with(signal, {
    rules <- rules
    rule_type <- rule_type
  })
  class(signal) <- c('RuleConstraint', class(signal))
  return(signal)
}
