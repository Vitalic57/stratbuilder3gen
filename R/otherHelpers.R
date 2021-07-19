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
    if(ncol(x) == 1){
      ret <- rbind(cbind(x[min(abs(k) + 1, n):n,]), m.na)
    }else{
      ret <- rbind(rbind(x[min(abs(k) + 1, n):n,]), m.na)
    }
  }else if (k == 0) {
    return(x)
  }else{
    m.na <- matrix(NA, nrow = k, ncol = ncol(x))
    n <- nrow(x)
    if(ncol(x) == 1){
      ret <- rbind(m.na, cbind(x[1:max(1, n - k),]))
    }else{
      ret <- rbind(m.na, rbind(x[1:max(1, n - k),]))
    }
  }
  rownames(ret) <- NULL
  return(ret)
}

lag_fun <- function(x, k = 1){
  n <- length(x)
  if(k > n){
    stop("k > nrow(x)")
  }else if (k != as.integer(k)){
    stop("k must be a non-negative integer")
  }else if (k < 0){
    m.na <- rep(NA, abs(k))
    ret <- c(x[(abs(k) + 1):n], m.na)
  }else if (k == 0) {
    return(x)
  }else{
    m.na <- rep(NA, k)
    ret <- c(m.na, x[1:(n - k)])
  }
  return(ret)
}

#' @param x object
#' @param k numeric, integer lag
#'
#' @export
#' @rdname Lag
Lag.character <- function(x, k = 1){
  lag_fun(x, k)
}

#' @param x object
#' @param k numeric, integer lag
#'
#' @export
#' @rdname Lag
Lag.numeric <- function(x, k = 1){
  lag_fun(x, k)
}

#' @param x object
#' @param k numeric, integer lag
#'
#' @export
#' @rdname Lag
Lag.logical <- function(x, k = 1){
  lag_fun(x, k)
}


#' Work as function diff, but remains NA in the beginnig.
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
