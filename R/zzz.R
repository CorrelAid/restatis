.onAttach <- function(libname, pkgname) {

  packageStartupMessage("Default option of 'restatis.use_cache' has been set to TRUE, that means all API calls will be cached.\nTo disable caching altogether use 'options(restatis.use_cache = FALSE)'. See README for more information.")

}

.onLoad <- function(libname, pkgname) {

  # Set a function whose results are to be cached
  .gen_api_cached <<- memoise::memoise(.gen_api_core)

  # Set the option of use_cache for the gen_api function
  options(restatis.use_cache = TRUE)

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

  # Set the default URL for the www.statistikdaten.bayern.de database
  if (!nzchar(Sys.getenv("RESTATIS_BAYERN_URL"))) {

    Sys.setenv(RESTATIS_BAYERN_URL = "https://www.statistikdaten.bayern.de/genesisWS/rest/2020/")

  }

  # Set the default URL for the www.landesdatenbank.nrw.de database
  if (!nzchar(Sys.getenv("RESTATIS_NRW_URL"))) {

    Sys.setenv(RESTATIS_NRW_URL = "https://www.landesdatenbank.nrw.de/ldbnrwws/rest/2020/")

  }

  # Set the default URL for the www.bildungsmonitoring.de database
  if (!nzchar(Sys.getenv("RESTATIS_BILDUNG_URL"))) {

    Sys.setenv(RESTATIS_BILDUNG_URL = "https://www.bildungsmonitoring.de/bildungws/rest/2020/")

  }

  # Set the default URL for the genesis.sachsen-anhalt.de database
  if (!nzchar(Sys.getenv("RESTATIS_SA_URL"))) {

    Sys.setenv(RESTATIS_SA_URL = "https://genesis.sachsen-anhalt.de/webservice/rest/2020/")

  }

  # Set the default URL for the ZENSUS 2022 database
  if (!nzchar(Sys.getenv("RESTATIS_ZENSUS_URL"))) {

    Sys.setenv(RESTATIS_ZENSUS_URL = "https://ergebnisse.zensus2022.de/api/rest/2020")

  }

}
