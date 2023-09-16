.onLoad <- function(libname, pkgname) {
  gen_api <<- memoise::memoise(gen_api)

  if (!nzchar(Sys.getenv("GENESIS_LANG"))) Sys.setenv(GENESIS_LANG = "en")
}

.onLoad_zensus <- function(libname, pkgname) {
  gen_zensus_api <<- memoise::memoise(gen_zensus_api)

  if (!nzchar(Sys.getenv("GENESIS_LANG"))) Sys.setenv(GENESIS_LANG = "en")
}
