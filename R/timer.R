#' Set up counter
#' 
#' Variables that you defined in dots will be set to 0 and will be updated by additing 1 every time updateTimer called.
#'
#' @param this Strategy
#' @param ... variables
#' @param env environment where variables will be defined
#' @rdname timer
#' @method  timer Strategy
#' @export
timer.Strategy <- function(this, ..., env=parent.frame()){
  dots <- rlang::enexprs(...)
  for(i in seq_along(dots)){
    x <- names(dots)[i]
    if(x == ''){
      x <- as.character(dots[i])
      assign(x, 0, envir = env)
    }else{
      assign(x, eval(dots[[i]]), envir = env)
    }
    this$timers[[x]] <- env
  }
}