#' gen_api
#'
#' @description Wrapper function to either use cached version of gen_api or un-cached version
#'
#' @param ... Parameters passed on to the API call
#' @param use_cache Get the option value on whether the call should be cached or not
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' gen_api(endpoint = "helloworld/logincheck", database = "genesis") %>%
#'  httr2::resp_body_json()
#' }
#'
gen_api <- function(...,
                    use_cache = getOption("restatis.use_cache", TRUE)) {

  if (isTRUE(use_cache)) {

    return(.gen_api_cached(...))

  } else {

    return(.gen_api_core(...))

  }

}

#-------------------------------------------------------------------------------

#' .gen_api_core
#'
#' @description Low-level function to interact with the one of the APIs
#'
#' @param endpoint Character string. The endpoint of the API that is to be queried.
#' @param database The database the query should be sent to.
#' @param ... Further parameters passed on to the final API call.
#'
#' @importFrom httr2 `%>%`
#'
.gen_api_core <- function(endpoint,
                          database,
                          ...) {

  #-----------------------------------------------------------------------------

  # Define URLs

  if (database == "genesis") {

    url <- Sys.getenv("RESTATIS_GENESIS_URL")

  } else if (database == "zensus") {

    url <- Sys.getenv("RESTATIS_ZENSUS_URL")

  } else if (database == "regio") {

    url <- Sys.getenv("RESTATIS_REGIO_URL")

  }

  user_agent <- "https://github.com/CorrelAid/restatis"

  #-----------------------------------------------------------------------------

  # First try to request with POST
  # If POST errors, try GET

  tryCatch(

    error = function(cnd) {

      httr2::request(url) %>%
        httr2::req_user_agent("https://github.com/CorrelAid/restatis") %>%
        httr2::req_url_path_append(endpoint) %>%
        httr2::req_url_query(!!!gen_auth_get(database = database), ...) %>%
        httr2::req_retry(max_tries = 3) %>%
        httr2::req_perform()

    }, {

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
                           "username" = gen_auth_get(database = database)$username,
                           "password" = gen_auth_get(database = database)$password) %>%
        httr2::req_retry(max_tries = 3) %>%
        httr2::req_perform()

    })

  #-----------------------------------------------------------------------------


}
