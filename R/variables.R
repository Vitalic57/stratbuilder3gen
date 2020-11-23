#' Add instruction for updating variables
#'
#' These instruction will be applied at each update of indicators and rules.
#' Mainly it is needed when actions like splits and dividend happen.
#'
#' @param this Strategy
#' @param ... expression
#'
#' @export
#' @rdname addVariables
#' @method addVariables Strategy
addVariables.Strategy <- function(this, ...){
  dots <- rlang::enexprs(...)
  for(name in names(dots)){
    this$variables[[name]] <- dots[[name]]
  }
}
