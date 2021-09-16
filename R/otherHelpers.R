#' @param x matrix
#' @param k numeric, integer lag
#'
#' @export
#' @rdname Lag
Lag.matrix <- function(x, k = 1){
  if(abs(k) >= nrow(x)){
    return(matrix(NA, nrow=nrow(x), ncol=ncol(x)))
  }else if (k != as.integer(k)){
    stop("k must be an integer")
  }else if(k < 0){
    m.na <- matrix(NA, nrow = abs(k), ncol = ncol(x))
    n <- nrow(x)
    ret <- rbind(x[min(abs(k) + 1, n):n,,drop=FALSE], m.na)
  }else if (k == 0) {
    return(x)
  }else{
    m.na <- matrix(NA, nrow = k, ncol = ncol(x))
    n <- nrow(x)
    ret <- rbind(m.na, x[1:max(1, n - k),,drop=FALSE])
  }
  rownames(ret) <- NULL
  return(ret)
}

lag_fun <- function(x, k = 1){
  n <- length(x)
  if(k > n){
    stop("k > nrow(x)")
  }else if (k != as.integer(k)){
    stop("k must be an integer")
  }else if (k < 0){
    m.na <- rep(NA, abs(k))
    ret <- c(x[(abs(k) + 1):n], m.na)
  }else if (k == 0) {
    return(x)
  }else{
    m.na <- rep(NA, k)
    ret <- c(m.na, `length<-`(x, n - k))
  }
  return(ret)
}

#' @param x object
#' @param k numeric, integer lag
#'
#' @export
#' @rdname Lag
Lag.default <- function(x, k = 1){
  lag_cpp(x, k)
}

#' Works as function lag, but remains NA in the beginnig.
#'
#' @param x object
#' @param ... params
#'
#' @export
#' @rdname Lag
Lag <- function(x, ...){
  UseMethod('Lag', x)
}

tail_cpp <- function(x, n=5L){
  if(!is.null(dim(x))){
    xlen <- dim(x)[1]
  }else{
    xlen <- length(x)
  }
  n <- if (n < 0L)
    max(xlen + n, 0L)
  else min(n, xlen)
  subsequence(x, xlen - n + 1, xlen)
}

head_cpp <- function(x, n=5L){
  if(!is.null(dim(x))){
    xlen <- dim(x)[1]
  }else{
    xlen <- length(x)
  }
  n <- if (n < 0L)
    max(xlen + n, 0L)
  else min(n, xlen)
  subsequence(x, 1, n)
}

#' Works as function tail, erase rownames, works faster
#'
#' @param x object
#' @param n int
#'
#' @export
#' @rdname Tail
Tail <- function(x, n){
  UseMethod('Tail', x)
}

#' @param n int
#'
#' @export
#' @rdname Tail
Tail.default <- function(x, n = 5L){
  tail(x, n)
}

#' @param n int
#'
#' @export
#' @rdname Tail
Tail.numeric <- function(x, n = 5L){
  tail_cpp(x, n)
}

#' @param n int
#'
#' @export
#' @rdname Tail
Tail.logical <- function(x, n = 5L){
  tail_cpp(x, n)
}

#' @param n int
#'
#' @export
#' @rdname Tail
Tail.character <- function(x, n = 5L){
  tail_cpp(x, n)
}

#' @param n int
#'
#' @export
#' @rdname Tail
Tail.matrix <- function(x, n = 5L){
  tail_cpp(x, n)
}

#' Works as function head, erase rownames, works faster
#'
#' @param x object
#' @param n int
#'
#' @export
#' @rdname Head
Head <- function(x, n){
  UseMethod('Head', x)
}

#' @param n int
#'
#' @export
#' @rdname Head
Head.default <- function(x, n = 5L){
  head(x, n)
}

#' @param n int
#'
#' @export
#' @rdname Head
Head.numeric <- function(x, n = 5L){
  head_cpp(x, n)
}

#' @param n int
#'
#' @export
#' @rdname Head
Head.logical <- function(x, n = 5L){
  head_cpp(x, n)
}

#' @param n int
#'
#' @export
#' @rdname Head
Head.character <- function(x, n = 5L){
  head_cpp(x, n)
}

#' @param n int
#'
#' @export
#' @rdname Head
Head.matrix <- function(x, n = 5L){
  head_cpp(x, n)
}


#' Works as function diff, but remains NA in the beginnig.
#'
#' @param x object
#' @param ... params
#'
#' @export
#' @rdname Diff
Diff <- function(x, ...){
  UseMethod('Diff', x)
}


#' @param k numeric, integer lag
#'
#' @export
#' @rdname Diff
Diff.default <- function(x, k = 1){
  x - Lag(x, k)
}

#' @export
#' @rdname Diff
Diff.matrix <- function(x, k = 1){
  x - Lag(x, k)
}
