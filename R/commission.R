#' Set commission to model
#'
#' @param this Strategy
#' @param q quote/expression, it should depend from pos_change argument, but it can include any variable from model
#'
#' @export
#' @rdname commission
#' @method setCommission Strategy
setCommission.Strategy <- function(this, q){
  q <- rlang::enexpr(q)
  if(as.character(q[[1]]) == 'quote'){
    q <- eval(q)
  }
  this$commission_quote <- q
  return(invisible(this))
}

#' Get commission expression of model
#'
#' @param this Strategy
#'
#' @export
#' @rdname commission
#' @method getCommission Strategy
getCommission.Strategy <- function(this){
  return(this$commission_quote)
}
