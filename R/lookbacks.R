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
