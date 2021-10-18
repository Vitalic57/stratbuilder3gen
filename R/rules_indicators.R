#' Add multiple indicators to strategy
#'
#' Define variable via expr and name it via name. It will be recalculated each lookforward steps.
#'
#' If it needs multiple variables it can also be done via expr, but names should be pointed in vars.
#'
#'
#' @param this model
#' @inheritParams Indicator
#' @inheritParams Signal
#'
#' @rdname addIndicators
#' @method addIndicators Strategy
#' @export
addIndicators.Strategy <- function(this,
                                   expr,
                                   name,
                                   lookback = NULL,
                                   args = list(),
                                   lookforward=NULL,
                                   vars=NULL
                                   ){
  nms <- sapply(this$indicators, '[[', 'name')
  i <- 0
  while(TRUE){
    if(missing(name)){
      tmp <- paste0('indicator', length(this$indicators) + 1 + i)
      if(!tmp %in% nms){
        name <- tmp
        break
      }
    }else{
      break
    }
    i <- i + 1
  }
  args <- rlang::enexprs(expr=expr,
                         lookback =lookback,
                         args = args,
                         lookforward=lookforward,
                         vars=vars)
  args[['name']] <- name
  this$indicators[[name]] <- do.call('Indicator', args = args)
  return(invisible(this))
}


#' Add rule to strategy
#'
#'
#' @param this Strategy
#' @inheritParams Signal
#' @inheritParams Rule
#' @rdname addRule
#' @method addRule Strategy
#' @export
addRule.Strategy <- function(this,
                             expr = TRUE,
                             name,
                             lookback = 0,
                             args = list(),
                             type = 'enter',
                             block,
                             pathwise = FALSE,
                             position = NULL,
                             position_const = NULL,
                             price = NULL,
                             on_success = NULL,
                             reopen = FALSE
){
  nms <- sapply(this$rules, '[[', 'name')
  i <- 0
  while(TRUE){
    if(missing(name)){
      tmp <- paste0('rule', length(this$rules) + 1 + i)
      if(!tmp %in% nms){
        name <- tmp
        break
      }
    }else{
      break
    }
    i <- i + 1
  }
  args <- rlang::enexprs(expr = expr,
                          lookback = lookback,
                          args = args,
                          type = type,
                          block = block,
                          pathwise = pathwise,
                          position = position,
                          position_const = position_const,
                          price = price,
                          on_success = on_success,
                          reopen = reopen)
  args[['name']] <- name
  this$rules[[name]] <- do.call('Rule', args = args)
  return(invisible(this))
}


#' Add rule's constraint
#'
#'
#' @param this Strategy
#' @inheritParams Signal
#' @inheritParams RuleConstraint
#'
#' @rdname addRuleConstraint
#' @method addRuleConstraint Strategy
#' @export
addRuleConstraint.Strategy <- function(this,
                                       expr,
                                       name,
                                       lookback = 0,
                                       args = list(),
                                       rules,
                                       rule_type
){
  nms <- sapply(this$rule_contraints, '[[', 'name')
  i <- 0
  while(TRUE){
    if(missing(name)){
      tmp <- paste0('rule_constraint', length(this$rule_contraints) + 1 + i)
      if(!tmp %in% nms){
        name <- tmp
        break
      }
    }else{
      break
    }
    i <- i + 1
  }
  args <- rlang::enexprs(expr = expr,
                          name = name,
                          lookback = lookback,
                          args = args,
                          rules = rules,
                          rule_type = rule_type)
  args[['name']] <- name
  this$rule_contraints[[name]] <- do.call("RuleConstraint", args = args)
  return(invisible(this))
}


#' Gets inforamtion about indicators in the model
#'
#' @param this Strategy
#'
#' @return list of inforamation
#' @rdname Signal
#' @method getIndicators Strategy
#' @export
getIndicators.Strategy <- function(this){
  return(this$indicators)

}

#' Get information about rules in the model
#'
#' @param this Strategy
#' @param pathwise logical, if true then return only pathwise=TRUE rules
#'
#' @return list of information
#' @rdname Signal
#' @method getRules Strategy
#' @export
getRules.Strategy <- function(this, pathwise){
  if(missing(pathwise)){
    return(this[['rules']])
  }else{
    l <- lapply(this$rules, function(rule) if(rule[['pathwise']] == pathwise) rule else NULL)
    l[sapply(l, is.null)] <- NULL
    return(l)
  }
}


#' Gets rule constraints
#'
#' @param this Strategy
#'
#' @return list of information
#' @rdname Signal
#' @method getRuleConstraints Strategy
#' @export
getRuleConstraints.Strategy <- function(this){
  return(this[['rule_contraints']])
}


#' Get information about one rule in the strategy
#'
#' @param this Strategy
#' @param name character
#'
#' @return list of information
#' @rdname Signal
#' @method getRule Strategy
#' @export
getRule.Strategy <- function(this, name){
  if(name %in% names(this[['rules']])){
    return(this[['rules']][[name]])
  }
}


#' Get indicators and rules
#'
#' @param this Strategy
#' @param name character
#'
#' @return list of information
#' @rdname Signal
#' @method getSignals Strategy
#' @export
getSignals.Strategy <- function(this){
  c(getRules(this), getIndicators(this), getRuleConstraints(this))
}


#' Calculate vector rules and indicators
#'
#' @param this Strategy
#' @param env environment, result environment, if NULL then new environment will be created
#' @param ... variables
#'
#' @return environment
#' @rdname Signal
#' @method calcVecSignals Strategy
#' @export 
calcVecSignals.Strategy <- function(this, env=NULL, ...){
  if(is.null(env)){
    res <- new.env()
    parent.env(res) <- parent.frame()
  }else{
    res <- env
  }
  res$data <- this$data
  dots <- list(...)
  for(x in names(dots)){
    res[[x]] <- dots[[x]]
  }
  if(!'range' %in% names(dots)){
    res[['range']] <- 1:res[['data']][['nrow']]
  }
  
  signals <- c(getIndicators(this), getRules(this, pathwise = FALSE))
  for(x in signals){
    x[['env']] <- new.env()
    parent.env(x[['env']]) <- res
    if(!is.null(x[['vars']])){
      # lazy evaluation
      if(is.null(x[['qexpr']])){
        x[['qexpr']] <- pryr::substitute_q(call('within', list(), x[['expr']]), env = c(getParams(this, 'indicators'), x[['args']]))
      }
      rlang::env_bind_lazy(x[['env']],
                    !!x[['name']] := !!x[['qexpr']],
                    .eval_env = x[['env']]
      )
      for(var in x[['vars']]){
          rlang::env_bind_lazy(res,
                        !!var := (!!x[['env']])[[!!x[['name']]]][[!!var]],
                        .eval_env = res
          )
      }
    }else{
      # lazy evaluation
      if(is.null(x[['qexpr']])){
        if(class(x)[1] == 'Indicator'){
          x[['qexpr']] <- pryr::substitute_q(x[['expr']], env = c(getParams(this, 'indicators'), x[['args']]))
        }else{
          x[['qexpr']] <- pryr::substitute_q(x[['expr']], env = c(getParams(this, 'rules'), x[['args']]))
        }
      }
      rlang::env_bind_lazy(x[['env']],
                    !!x[['name']] := !!x[['qexpr']],
                    .eval_env = x[['env']]
      )
      rlang::env_bind_lazy(res,
                    !!x$name := (!!x[['env']])[[!!x[['name']]]],
                    .eval_env = res
      )
    }
  } 
  return(invisible(res))
}




