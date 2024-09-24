#' gen_genesis_api
#'
#' @description Low-level function to interact with the GENESIS API
#'
#' @param endpoint Character string. The endpoint of the API that is to be queried.
#'
#' @importFrom httr2 `%>%`
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' gen_genesis_api("helloworld/logincheck") %>%
#'  httr2::resp_body_json()
#' }
#'
gen_genesis_api <- function(endpoint, ...) {

  url <- "https://www-genesis.destatis.de/genesisWS/rest/2020"
  user_agent <- "https://github.com/CorrelAid/restatis"

  body_parameters <- list(...)

  if (length(body_parameters) > 0) {

    req <- httr2::request(url) %>%
      httr2::req_body_form(!!!body_parameters)

  } else {

    req <- httr2::request(url) %>%
      httr2::req_body_form(!!!list("foo" = "bar"))

  }

  req %>%
    httr2::req_user_agent(user_agent) %>%
    httr2::req_url_path_append(endpoint) %>%
    httr2::req_headers("Content-Type" = "application/x-www-form-urlencoded",
                       "username" = gen_auth_get(database = "genesis")$username,
                       "password" = gen_auth_get(database = "genesis")$password) %>%
    httr2::req_retry(max_tries = 3) %>%
    httr2::req_perform()

}

#-------------------------------------------------------------------------------

#' gen_regio_api
#'
#' @description Low-level function to interact with the regionalstatistik.de API
#'
#' @param endpoint Character string. The endpoint of the API that is to be queried.
#'
#' @importFrom httr2 `%>%`
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' gen_regio_api("helloworld/logincheck") %>%
#'   httr2::resp_body_json()
#' }
#'
gen_regio_api <- function(endpoint, ...) {

  httr2::request("https://www.regionalstatistik.de/genesisws/rest/2020/") %>%
    httr2::req_user_agent("https://github.com/CorrelAid/restatis") %>%
    httr2::req_url_path_append(endpoint) %>%
    httr2::req_url_query(!!!gen_auth_get(database = "regio"), ...) %>%
    httr2::req_retry(max_tries = 3) %>%
    httr2::req_perform()

}

#-------------------------------------------------------------------------------

#' gen_zensus_api
#'
#' @description Low-level function to interact with the Zensus 2022 database
#'
#' @param endpoint Character string. The endpoint of the API that is to be queried.
#'
#' @importFrom httr2 `%>%`
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' gen_zensus_api("helloworld/logincheck") %>%
#'   httr2::resp_body_json()
#' }
#'
gen_zensus_api <- function(endpoint, ...) {

  url <- "https://ergebnisse.zensus2022.de/api/rest/2020"
  user_agent <- "https://github.com/CorrelAid/restatis"

  body_parameters <- list(...)

  if (length(body_parameters) > 0) {

    req <- httr2::request(url) %>%
      httr2::req_body_form(!!!body_parameters)

  } else {

    req <- httr2::request(url) %>%
      httr2::req_body_form(!!!list("foo" = "bar"))

  }

  req %>%
    httr2::req_user_agent(user_agent) %>%
    httr2::req_url_path_append(endpoint) %>%
    httr2::req_headers("Content-Type" = "application/x-www-form-urlencoded",
                       "username" = gen_auth_get(database = "zensus")$username,
                       "password" = gen_auth_get(database = "zensus")$password) %>%
    httr2::req_retry(max_tries = 3) %>%
    httr2::req_perform()

}
