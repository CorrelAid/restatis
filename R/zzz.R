.onLoad <- function(libname, pkgname) {
  gen_api <<- memoise::memoise(gen_api)
}