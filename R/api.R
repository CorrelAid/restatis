#' Low-level function to interact with the Destatis GENESIS API
#'
#' @examples
#' gen_api("helloworld/whoami") |>
#'   httr2::resp_body_json()
gen_api <- function(method, ...) {
  httr2::request("https://www-genesis.destatis.de/genesisWS/rest/2020") |>
    httr2::req_url_path_append(method) |>
    httr2::req_url_query(...) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
}
