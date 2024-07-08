#' gen_api
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
#' gen_api("helloworld/logincheck") %>%
#'   httr2::resp_body_json()
#' }
#'
gen_api <- function(endpoint, ...) {

  httr2::request("https://www-genesis.destatis.de/genesisWS/rest/2020") %>%
    httr2::req_user_agent("https://github.com/CorrelAid/restatis") %>%
    httr2::req_url_path_append(endpoint) %>%
    httr2::req_url_query(!!!gen_auth_get(database = "genesis"), ...) %>%
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

  httr2::request("https://ergebnisse2011.zensus2022.de/api/rest/2020") %>%
    httr2::req_user_agent("https://github.com/CorrelAid/restatis") %>%
    httr2::req_url_path_append(endpoint) %>%
    httr2::req_url_query(!!!gen_auth_get(database = "zensus"), ...) %>%
    httr2::req_retry(max_tries = 3) %>%
    httr2::req_perform()

}
