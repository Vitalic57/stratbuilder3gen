#' @import datastorage3pub
NULL


.onLoad <- function(libname, pkgname){
  options('tibble.width'= Inf)
}

.onUnload <- function (libpath) {
  library.dynam.unload("stratbuilder3gen", libpath)
}
