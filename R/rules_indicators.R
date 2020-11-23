#' Add multiple indicators to strategy
#'
#' These indicators will be calculated each time when spread updates coefficients
#' indicators must return data.frame or matrix or array
#'
#' @param this model
#' @param ... params
#'
#' @rdname Signal
#' @method addIndicators Strategy
#' @export
addIndicators.Strategy <- function(this,
                                   ...){
  dots <- rlang::enexprs(...)
  nms <- sapply(this$indicators, '[[', name)
  i <- 0
  while(TRUE){
    if(! 'name' %in% names(dots)){
      tmp <- paste0('rule', length(this$indicators) + 1 + i)
      if(!tmp %in% nms){
        dots[['name']] <- tmp
        break
      }
    }
    i <- i + 1
  }
  this$indicators[[dots[['name']]]] <- do.call('Indicator', args = dots)
  return(invisible(this))
}


#' Add rule to strategy
#'
#'
#' @param this Strategy
#' @param ... params
#' @rdname Signal
#' @method addRule Strategy
#' @export
addRule.Strategy <- function(this,
                             ...
){
  dots <- rlang::enexprs(...)
  nms <- sapply(this$rules, '[[', name)
  i <- 0
  while(TRUE){
    if(! 'name' %in% names(dots)){
      tmp <- paste0('rule', length(this$rules) + 1 + i)
      if(!tmp %in% nms){
        dots[['name']] <- tmp
        break
      }
    }
    i <- i + 1
  }
  this$rules[[dots[['name']]]] <- do.call('Rule', args = dots)
  return(invisible(this))
}


#' Add rule's constraint
#'
#'
#' @param this Strategy
#' @param ... params
#' @rdname Signal
#' @method addRuleConstraint Strategy
#' @export
addRuleConstraint.Strategy <- function(this,
                                       ...
){
  dots <- rlang::enexprs(...)
  nms <- sapply(this$rule_contraints, '[[', name)
  i <- 0
  while(TRUE){
    if(! 'name' %in% names(dots)){
      tmp <- paste0('rule', length(this$rule_contraints) + 1 + i)
      if(!tmp %in% nms){
        dots[['name']] <- tmp
        break
      }
    }
    i <- i + 1
  }
  this$rule_contraints[[dots[['name']]]] <- do.call('RuleConstraint', args = dots)
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
    return(this$rules)
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
  return(this$rule_contraints)
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
  if(name %in% names(this$rules)){
    return(this$rules[[name]])
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
