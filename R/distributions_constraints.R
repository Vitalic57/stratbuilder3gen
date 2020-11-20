#' Install parameters to model
#'
#' @param this model
#' @param param.combo data.frame/list/vector - parameters
#'
#' @export
#' @rdname installParams
#' @method installParams Strategy
installParams.Strategy <- function(this, param.combo){
  if(!is.data.frame(param.combo)){
    if(is.list(param.combo)){
      if(is.null(names(param.combo))){
        stop("list should have names")
      }
      param.combo <- as.data.frame(param.combo)
    }else if(is.vector(param.combo)){
      if(is.null(names(param.combo))){
        stop("vector should have names")
      }
      param.combo <- as.data.frame(as.list(param.combo))
    }
  }
  install.param.combo(this$thisEnv, param.combo)
}


#' Installs param combination into strategy
#'
#' @param strategy environment, strategy environment
#' @param param.combo data.frame with one row
#' @export
install.param.combo <- function (strategy, param.combo){
  if (is.null(dim(param.combo))) {
    stop("'param.combo' must have a dim attribute")
  }
  ind_names <- sapply(strategy$indicators, function(x) x$name)
  rule_names <- sapply(strategy$rules, function(x) x$name)
  rule_constraint_names <- sapply(strategy$rule_constraints, function(x) x$name)
  # pymodel_names <- strategy$pymodel$as
  for (param.label in colnames(param.combo)) {
    distribution <- paramset$distributions[[param.label]]
    component.type <- distribution$component.type
    component.labels <- distribution$component.label
    for(component.label in component.labels){
      variable.name <- names(distribution$variable)
      switch(component.type,
             indicators=,
             indicator = {
               res <- which(ind_names == component.label)
               if(length(res) == 0){
                 stop('component label is not found')
               }else{
                 if(variable.name %in% names(strategy$indicators[[res]]$args)){
                   if(is.character(param.combo[,param.label]) && 'env' %in% names(distribution)){
                     strategy$indicators[[res]]$args[[variable.name]] <- get(param.combo[,param.label], distribution[['env']])
                   }else{
                     strategy$indicators[[res]]$args[[variable.name]] <- param.combo[,param.label]
                   }
                 }else{
                   stop('wrong variable name')
                 }
               }
             },
             rule =,
             rules = {
               res <- which(rule_names == component.label)
               if(length(res) == 0){
                 stop('component label is not found')
               }else{
                 if(variable.name %in% names(strategy$rules[[res]]$args)){
                   if(is.character(param.combo[,param.label]) && 'env' %in% names(distribution)){
                     strategy$rules[[res]]$args[[variable.name]] <- get(param.combo[,param.label], distribution[['env']])
                   }else{
                     strategy$rules[[res]]$args[[variable.name]] <- param.combo[,param.label]
                   }
                 }else{
                   stop('wrong variable name')
                 }
               }
             },
             rule_constraint=,
             rules_constraint=,
             rules_constrints=,
             rule_constrains = {
               res <- which(rule_constraint_names == component.label)
               if(length(res) == 0){
                 stop('component label is not found')
               }else{
                 if(variable.name %in% names(strategy$rule_constraints[[res]]$args)){
                   if(is.character(param.combo[,param.label]) && 'env' %in% names(distribution)){
                     strategy$rule_constraints[[res]]$args[[variable.name]] <- get(param.combo[,param.label], distribution[['env']])
                   }else{
                     strategy$rule_constraints[[res]]$args[[variable.name]] <- param.combo[,param.label]
                   }
                 }else{
                   stop('wrong variable name')
                 }
               }
             },
             # pymodel = {
             #   if(is.character(param.combo[,param.label]) && 'env' %in% names(distribution)){
             #     strategy$thisEnv$pymodel$args[[variable.name]] <- get(param.combo[,param.label], distribution[['env']])
             #   }else{
             #     strategy$thisEnv$pymodel$args[[variable.name]] <- param.combo[,param.label]
             #   }
             # },
             params = {
               if(is.character(param.combo[,param.label]) && 'env' %in% names(distribution)){
                 strategy$params[[component.label]][[variable.name]] <- get(param.combo[,param.label], distribution[['env']])
               }else{
                 strategy$params[[component.label]][[variable.name]] <- param.combo[,param.label]
               }

             }
      )
    }
  }
}



#' Adds distribution
#'
#' @param this Strategy
#' @param component.type character, one of rule, indicator, params
#' @param component.label character vector, name of component, argument 'as' is resposible for that
#' @param variable list, each cell should be named and this name should coincise with one of your arguments.
#' @param label character, name for this distribution
#' @param ... arguments for variable
#'
#' @export
#' @rdname addDistribution
#' @method addDistribution Strategy
#' @examples
#' \dontrun{
#' addIndicator(this, name = BBands, args = list(HLC = quote(spread), n = 20, sd = 1), as = 'bb')
#' paramset <- "TEST"
#' addDistribution(this,
#'     paramset.label = paramset,
#'     component.type = 'indicator',
#'     component.label = 'bb',
#'     variable = list(sd = seq(0.5,3,0.05)),
#'     label = 'bb.sd'
#' )
#'
#' addRule(this, as = 'bb_up_dn',
#'      condition = (Lag(spread, 1) > Lag(bb[, 'up'], 1)) &
#'                   (spread < bb[,'up']) &
#'                   (spread > bb[,'mavg']) &
#'                   (abs(spread - bb[,'mavg'])  > n) ,
#'      type = 'enter',
#'      args = list(n = 0.005),
#'      side = -1,
#'      oco = 'short')
#' addDistribution(this,
#'     component.type = 'rule',
#'     component.label = 'bb_up_dn',
#'     variable = list(n = seq(0.005,0.03,0.002)),
#'     label = 'my_distr'
#' )
#' }
addDistribution.Strategy <- function(this,
                                     component.type,
                                     component.label = NULL,
                                     variable = list(),
                                     label
){
  if(!is.list(variable)){
    stop("Variable should be a list")
  }
  variable <- c(variable, list(...))
  if(length(variable) == 0){
    stop('Variable should have length more than 0')
  }

  if(missing(label)){
    label <- paste0('distribution', length(this$paramset[['distributions']]) + 1)
  }else{
    label <- make.names(label)
  }
  if(length(variable) > 1){
    for(i in seq_along(variable)){
      cl <- call('addDistribution',
                 this = this,
                 component.type=component.type,
                 component.label=component.label,
                 label = paste(label, names(variable)[i], sep = '.'),
                 variable = rlang::call2('list',  !!names(variable)[i] := variable[[i]])
      )
      eval(cl)
    }
    return(invisible(NULL))
  }

  component.type <- switch(component.type,
                           rule = ,
                           rules ={
                             if(!all(component.label %in% names(this$rules))){
                               return()
                             }
                             'rules'
                           },
                           rule_constraints = ,
                           rules_constraints =,
                           rules_constraint=,
                           rule_constraint ={
                             if(!all(component.label %in% names(this$rule_constraints))){
                               return()
                             }
                             'rule_constraints'
                           },
                           indicator =,
                           indicators = {
                             if(!all(component.label %in% names(getIndicators(this)))){
                               return()
                             }
                             'indicators'
                           },
                           params =,
                           param =,
                           par =,
                           pars =
                             {
                               if(is.null(component.label)){
                                 component.label <- 'all'
                               }
                               component.label <- switch (tolower(component.label),
                                                          rule = ,
                                                          rules = 'rules',
                                                          pp =,
                                                          pps =,
                                                          programparts =,
                                                          programpart =,
                                                          program = 'pps',
                                                          indicator=,
                                                          indicators='indicators',
                                                          rule_constraints = ,
                                                          rules_constraints =,
                                                          rules_constraint=,
                                                          rule_constraint = 'rule_constraints',
                                                          pymodel='pymodel',
                                                          all='all',
                                                          {warning('wrong component label');return()}
                               )
                               'params'
                             },
                           {
                             if (is.null(component.label)) {
                               component.label <- "x"
                             }
                             component.type
                           })
  if(is.list(variable[[1]]) && any(sapply(variable[[1]], is.function))){
    ee <- new.env()
    q <- substitute(variable)[[-1]]
    if (is.symbol(q)) {
      if (!is.null(names(variable[[1]]))) {
        nms <- names(variable[[1]])
        nms[nms == ""] <- paste(q, which(nms == ""), sep = '')
      } else {
        nms <- paste(q, 1:length(variable[[1]]), sep = '')
      }
    } else {
      nms <- sapply(q[-1], deparse)
    }
    for(i in 1:length(nms)){
      assign(nms[i], variable[[1]][[i]], envir = ee)
    }
    nms <- list(nms)
    names(nms) <- names(variable)
    l <- list(component.type = component.type,
              component.label = component.label,
              env = ee,
              variable = nms)
  } else  if (is.function(variable[[1]])) {
    ee <- new.env()
    func_name <- deparse(substitute(variable[[1]]))
    assign(func_name, variable[[1]], envir = ee)
    l <- list(component.type = component.type,
              component.label = component.label,
              env = ee,
              variable = func_name)

  } else {
    l <- list(component.type = component.type,
              component.label = component.label,
              variable = variable)
  }
  this$paramset[['distributions']][[label]] <- l
}


#' Adds contraints to distibutions
#'
#' @param this Strategy
#' @param expr expression, that contains names from labels of distributions
#' @param label character, name of the constraint
#'
#' @export
#' @rdname addDistributionConstraint
#' @method addDistributionConstraint Strategy
#' @examples
#' \dontrun{
#' addIndicator(this, name = BBands, args = list(HLC = quote(spread), n = 20, sd = 1), as = 'bb',
#'         lookback = 100, plot = 'main', columns = 1:3, col = "red")
#' addDistribution(this,
#'     component.type = 'indicator',
#'     component.label = 'bb',
#'     variable = list(sd = seq(0.5,3,0.05)),
#'     label = 'bb.sd'
#' )
#'
#' addRule(this, as = 'bb_up_dn',
#'      condition = (Lag(spread,1) > Lag(bb.up,1)) &
#'                   (spread < bb.up) &
#'                   (spread > bb.mavg) &
#'                   (abs(spread - bb.mavg)/spread_price  > n) ,
#'      type = 'enter',
#'      args = list(n = 0.005),
#'      side = -1,
#'      oco = 'short',
#'      osFun = sameMoneyOs,
#'      osFun_args = alist(amount = 5000000))
#' addDistribution(this,
#'     component.type = 'rule',
#'     component.label = 'bb_up_dn',
#'     variable = list(n = seq(0.005,0.03,0.002)),
#'     label = 'my_distr'
#' )
#'
#' addDistributionConstraint(this,
#'     expr = bb.sd > my_distr * 100
#' )
#' }
addDistributionConstraint.Strategy <- function(this,
                                               expr,
                                               label
){
  if(missing(label)){
    label <- paste0('constraint', length(this$paramset[['constraints']]) + 1)
  }
  this$paramset[['constraints']][[label]] <- list(expr = substitute(expr))
}


#' Get list of distributions
#'
#' @param this Strategy
#'
#' @return list of distributions
#' @export
#' @rdname getDistributions
#' @method getDistributions Strategy
getDistributions.Strategy <- function(this){
  return(this$paramset$distributions)
}

