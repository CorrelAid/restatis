#' Low-level function to interact with the Zensus 2022 GENESIS API
#'
#' @param endpoint Self-explanatory
#'
#' @importFrom httr2 `%>%`
#'
#' @noRd
#'
#' @examples
#' gen_api("helloworld/logincheck") %>%
#'   httr2::resp_body_json()
gen_zensus_api <- function(endpoint, ...) {
  httr2::request("https://ergebnisse2011.zensus2022.de/api/rest/2020") %>%
    httr2::req_user_agent("https://github.com/CorrelAid/restatis") %>%
    httr2::req_url_path_append(endpoint) %>%
    httr2::req_url_query(!!!gen_zensus_auth_get(), ...) %>%
    httr2::req_retry(max_tries = 3) %>%
    httr2::req_perform()
}
