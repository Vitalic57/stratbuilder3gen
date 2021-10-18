#' Adjust close table for dividends and splits
#'
#' @param close matrix / xts, table of close prices
#' @param splits matrix / xts, table of splits with default value 1
#' @param dividends matrix / xts, table of dividends with default value 0
#'
#' @return
#' @export
adjust_close <- function(close, splits, dividends){
  res <- close
  for(i in seq_len(ncol(close))){
    x <- .Call("adjRatios", splits[,i], dividends[,i], close[,i], PACKAGE = "TTR")
    res[,i] <- close[,i] * x[[2]] / x[[1]]
  }
  return(res)
}


#' Adjusted close from Data
#'
#' @param data Data
#' @param inds numeric vector
#'
#' @return
#' @export
get_adjusted <- function(data, inds=NULL){
  if(is.null(inds)){
    inds <- 1:data[['nrow']]
  }
  if(!is.null(data[['adjusted']])){
    return(data[['adjusted']][inds,,drop=FALSE])
  }
  if(is.null(data[['splits']])){
    splits <- matrix(1, ncol = data[['ncol']], nrow=length(inds))
  }else{
    splits <- data[['splits']][inds,,drop=FALSE]
  }
  if(is.null(data[['dividends']])){
    dividends <- matrix(0, ncol = data[['ncol']], nrow=length(inds))
  }else{
    dividends <- data[['dividends']][inds,,drop=FALSE]
  }
  return(adjust_close(data[['close']][inds,,drop=FALSE], splits, dividends))
}