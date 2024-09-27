.onLoad <- function(libname, pkgname) {

  # Set the functions whose results are to be cached
  gen_genesis_api <<- memoise::memoise(gen_genesis_api)
  gen_zensus_api <<- memoise::memoise(gen_zensus_api)
  gen_regio_api <<- memoise::memoise(gen_regio_api)

  # Set the default language of the package
  if (!nzchar(Sys.getenv("RESTATIS_LANG"))) Sys.setenv(RESTATIS_LANG = "en")

  # Set the default URL for the GENESIS database
  if (!nzchar(Sys.getenv("RESTATIS_GENESIS_URL"))) {

    Sys.setenv(RESTATIS_GENESIS_URL = "https://www-genesis.destatis.de/genesisWS/rest/2020")

  }

  # Set the default URL for the www.regionalstatistik.de database
  if (!nzchar(Sys.getenv("RESTATIS_REGIO_URL"))) {

    Sys.setenv(RESTATIS_REGIO_URL = "https://www.regionalstatistik.de/genesisws/rest/2020/")

  }

  # Set the default URL for the ZENSUS 2022 database
  if (!nzchar(Sys.getenv("RESTATIS_ZENSUS_URL"))) {

    Sys.setenv(RESTATIS_ZENSUS_URL = "https://ergebnisse.zensus2022.de/api/rest/2020")

  }

}
