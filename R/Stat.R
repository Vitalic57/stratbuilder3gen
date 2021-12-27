#' Class for one statistic
#'
#' @param description character description of stat
#' @param in_report logical if TRUE then it will can be accessed in report
#' @param func function how your stats should be calculated, this function should has three arguments
#'
#' * this - Strategy object
#'
#' * start - index of starting period of backtest
#'
#' * end - index of ending period of backtest
#' @param keywords character additional names for your stat
#' @param depends character names of other defined stats, they will be calculated before this stat
#' @param name character name that will be used in report
#' @param general logical, if TRUE then stat will be calculated once for all periods
#'
#' @return Stat object
#' @export
Stat <- function(description, in_report, func, keywords, depends, name, general){
  this <- environment()
  if(missing(in_report)){
    in_report <- FALSE
  }
  if(missing(name)){
    name <- ''
  }
  if(missing(general)){
    general <- FALSE
  }
  if(missing(depends)){
    depends <- NULL
  }
  if(missing(keywords)){
    keywords <- NULL
  }
  if(missing(description)){
    description <- ''
  }
  if(missing(func)){
    func <- NULL
  }else{
    environment(func) <- emptyenv()
  }
  
  class(this) <- c("Stat")
  #parent.env(this) <- emptyenv()
  return(this)
}

#' @export
#' @method format Stat
format.Stat <- function(stat){
  text <- class(stat)
  if(!is.null(stat$name)){
    text <- paste(text, stat$name, '\n')
  }
  for(key in names(stat)){
    if(key %in% c('this', 'general', 'in_report', 'name')){
      next
    }
    if(!is.null(stat[[key]])){
      if(is.function(stat[[key]])){
        tmp <- paste0(capture.output(stat[[key]]), collapse = '\n')
      }else{
        tmp <- stat[[key]]
      }
      text <- paste0(text, '    ',  key, ': ', paste0(tmp, collapse = ', '), '\n')
    }
  }
  return(text)
}

#' @export
#' @method print Stat
print.Stat <- function(stat){
  cat(format(stat))
}

#' @export
is.Stat <- function(stat){
  inherits(stat, 'Stat')
}
