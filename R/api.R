#' Low-level function to interact with the Destatis GENESIS API
#'
#' @param endpoint Self-explanatory
#'
#' @noRd
#'
#' @examples
#' gen_api("helloworld/whoami") |>
#'   httr2::resp_body_json()
gen_api <- function(endpoint, ...) {
  httr2::request("https://www-genesis.destatis.de/genesisWS/rest/2020") |>
    httr2::req_user_agent("https://github.com/CorrelAid/restatis") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(...) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
}
