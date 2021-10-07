#' @import datastorage3pub
NULL

#' @export
cur_price <- NULL

#' @export
position_last <- NULL

#' @export
unrealized_money_last <- NULL

#' @export
money_init <- NULL

#' @export
money_last <- NULL

#' @export
range <- NULL

#' @export
data <- Data()

#' @export
this <- NULL

#' @export
ii <- NULL

#' @export
i <- NULL


.onLoad <- function(libname, pkgname){
  options('tibble.width'= Inf)
}

.onUnload <- function (libpath) {
  library.dynam.unload("stratbuilder3gen", libpath)
}
