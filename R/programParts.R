#' Adds variables to strategy
#'
#' @param this Strategy
#' @param name character, name of your set of program parts
#' @param ... quote / expression callbacks
#' There are multiple places where you can add your code :
#'
#' i. full name / short name -- description.
#'
#' 1. init -- at initialization period before main cycle
#'
#' 2. after_tables / tables -- after initialization of rules tables
#'
#' 3. after_coefs / coefs -- after initialization of coefficients
#'
#' 4. each_iter / iter -- at the beginning of each iteration, before execution of rules
#'
#' 5. after_enter_to_pos / enter -- Right after entering to position
#'
#' 6. after_indicators / inds -- after initialization of indicators
#'
#' 7. before_enter_to_pos / bexit -- before enter to position
#'
#' 8. before_exit_from_pos / bexit -- before exit from position
#'
#' 9. after_exit_from_pos / exit -- after exit from position
#'
#' 10. data -- initialization of tables, derivative datasets and libraries
#'
#' 11. start_cycle / on_start -- at the start of cycle pass
#'
#' @example
#' \dontrun{
#' # count number of closed trades and store it in variable x.
#' addProgramPart(this,
#'     init = {
#'         x <- 0
#'     },
#'     exit = {
#'         x <- x + 1
#'     }
#' )
#' }
#' @export
#' @rdname ProgramPart
#' @method addProgramPart Strategy
addProgramPart.Strategy <- function(this, name, ...){
  e <- this
  if(missing(name)){
    name <- paste0('pp', length(e[['pps']]) + 1)
  }
  x <- quote_list(rlang::enexprs(...), parent.frame())
  e[['pps']][[name]] <- list(name = name, evolution = x)
  return(invisible(this))
}

#' Gets list of variables(program parts)
#'
#' @param this Strategy
#'
#' @export
#' @rdname ProgramPart
#' @method getProgramParts Strategy
getProgramParts.Strategy <- function(this){
  return(this$pps)
}
