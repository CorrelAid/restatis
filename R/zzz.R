.onLoad <- function(libname, pkgname) {
  gen_api <<- memoise::memoise(gen_api)
  gen_zensus_api <<- memoise::memoise(gen_zensus_api)
  gen_regio_api <<- memoise::memoise(gen_regio_api)

  if (!nzchar(Sys.getenv("GENESIS_LANG"))) Sys.setenv(GENESIS_LANG = "en")
}
