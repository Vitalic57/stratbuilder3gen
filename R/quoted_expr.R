quote_list <- function(args, env=NULL){
  res <- list()
  if(length(args) > 0){
    nms <- names(args)
    if(!is.null(nms)){
      for(i in 1:length(args)){
        if(nms[i] == ""){
          next
        }
        if(is.call(args[[i]]) && as.character(args[[i]][[1]]) == "quote" || is.symbol(args[[i]])){
          res[[nms[i]]] <- eval(args[[i]], envir = env)
        }else{
          res[[nms[i]]] <- args[[i]]
        }
      }
    }

  }
  return(res)
}
