#' Add lookback
#'
#' Add dependency lookback from variable
#'
#' @param this Strategy
#' @param ... variables
#'
#' @export
#' @rdname lookback
#' @method addLookback Strategy
addLookback.Strategy <- function(this, ...){
  vars <- rlang::enexprs(...)
  for(name in names(vars)){
    this$lookbacks[[name]] <- vars[[name]]
  }
  return(invisible(this))
}


#' Get lookbacks
#'
#' Get variables from which whole lookback depends
#'
#' @param this
#'
#' @return
#' @export
#' @rdname lookback
#' @method getLookbacks Strategy
getLookbacks.Strategy <- function(this){
  return(this$lookbacks)
}


dfs <- function(node, adj, e){
  e$vis[node] <- TRUE
  for(i in seq_along(adj[[node]])){
    if(! e$vis[adj[[node]][[i]][1]]){
      dfs(adj[[node]][[i]][1], adj, e)
    }
    e$dp[node] <- max(e$dp[node], adj[[node]][[i]][2] + e$dp[adj[[node]][[i]][1]])
  }
}

findLongestPath <- function(adj){
  e <- environment()
  n <- length(adj)
  dp <- numeric(n)
  vis <- logical(n)
  for(i in seq_len(n)){
    if(!vis[i]){
      dfs(i, adj, e)
    }
  }
  ans <- 0
  for(i in seq_len(n)){
    ans <- max(ans, dp[i])
  }
  return(ans)
}


#' Get lookback
#' 
#' Calculate current window for calculating all indicators and variables in model
#'
#' @param this Strategy
#' @param env environment
#'
#' @return numeric
#' @method getLookback Strategy
#' @export
getLookback.Strategy <- function(this, env = parent.frame()){
  signal_nodes <- getNodesInfo(this)
  this_nodes <- list('!this' = getLookbacks(this))
  nodes <- c(signal_nodes, this_nodes)
  nms <- c(names(nodes), 'data')
  nms_inv <- 1:length(nms)
  names(nms_inv) <- nms
  adj <- lapply(nms, function(x) list())
  for(i in seq_along(nodes)){
    for(j in seq_along(nodes[[i]])){
      if(names(nodes[[i]])[j] %in% nms){
        look <- nodes[[i]][[j]]
        if(is.language(look)){
          look <- eval(look, envir = env)
        }
        if(look > 0){
          u <- i
          v <- nms_inv[names(nodes[[i]])[j]]
          w <- look  
          adj[[u]][[length(adj[[u]]) + 1]] <- c(v, w)
        }
      }
    }
  }
  return(findLongestPath(adj))
}

#' @export
getNodesInfo <- function(this){
  objects <- getSignals(this)
  nodes <- list()
  for(object in objects){
    lookback <- getObjectLookback(this, object, type = class(object)[1])
    default_lookback <- 0
    if(!is.list(lookback)){
      default_lookback <- lookback 
      lookback <- list()
    }
    vars <- all.vars(object[['expr']]) 
    vars_list <- lapply(vars, function(var){
      if(var == object[['name']] || var %in% object[['vars']]){
        0
      }else if(var %in% names(lookback)){
        lookback[[var]]
      }else{
        default_lookback
      }
    })
    names(vars_list) <- vars
    if(class(object)[1] == 'Indicator' && !is.null(object$vars)){
      for(var in object$vars){
        nodes[[var]] <- vars_list
      }
    }else{
      nodes[[object[['name']]]] <- vars_list
    }
  }
  return(nodes)
}


getObjectLookback <- function(this, object, type){
  x <- object$lookback
  if(is.null(x)){
    x <- pryr::substitute_q(object[['expr']], c(getParams(this, type), object[['args']])) %>% 
      deparse %>%
      paste(collapse = '\n') %>% {
        m <- gregexpr("[0-9]*\\.?[0-9]+", .)
        if(m[[1]][1] == -1){
          0
        }else{
          floor(max(as.numeric(regmatches(., m)[[1]])))
        }
      }
  }
  if(is.language(x)){
    return(pryr::substitute_q(x, c(getParams(this, type=type), object[['args']])))
  }else{
    return(x)
  }
}


switch_objects <- function(type){
  switch(type,
         'rules' = getRules(this),
         'indicators' = getIndicators(this),
         'signals' = getSignals(this)) 
}


# rules <- list(
#   list(name = 'rule1', expr = quote(Lag(spread, 3)), lookback = 3),
#   list(name = 'rule2', expr = quote(Lag(rule1, 10) & Lag(spread, 1)), lookback = list(rule1 = 10, spread = 1))
# )
# 
# 
# nodes <- list(
#   x1 = list(
#     data = 100
#   ),
#   x2 = list(
#     data = 50,
#     x1 = 100
#   ),
#   x3 = list(
#     x1 = 30,
#     x2 = 2
#   ),
#   x4 = list(
#     x3 = 100,
#     data = 250
#   )
# )
