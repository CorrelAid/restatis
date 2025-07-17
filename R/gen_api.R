#' Upper level basic API request function
#'
#' @description Upper-level function to interact with the one of the APIs
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

  # Choose executing function based on cache option
  if (isTRUE(use_cache)) {

    return(.gen_api_cached(...))

  } else {

    return(.gen_api_core(...))

  }

}

#-------------------------------------------------------------------------------

#' Basic API request function
#'
#' @description (Uncached) Low-level function to interact with the one of the APIs
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

  # Define URLs depending on database chosen

  if (database == "genesis") {

    url <- Sys.getenv("RESTATIS_GENESIS_URL")

  } else if (database == "zensus") {

    url <- Sys.getenv("RESTATIS_ZENSUS_URL")

  } else if (database == "regio") {

    url <- Sys.getenv("RESTATIS_REGIO_URL")

  }

  # Set user agent
  user_agent <- "https://github.com/CorrelAid/restatis"

  #-----------------------------------------------------------------------------

  # First try to request with POST
  # If POST errors, try GET
  # This allows flexibility across different database instances

  tryCatch( # tryCatch to try POST

    expr = {

      # Catch API parameter values for ...
      body_parameters <- list(...)

      # Check if there are any items in ...
      if (length(body_parameters) > 0) {

        req <- httr2::request(url) %>%
          httr2::req_body_form(!!!body_parameters)

      } else {

        # To make a request work with empty ... we need a fake body
        req <- httr2::request(url) %>%
          httr2::req_body_form(!!!list("foo" = "bar"))

      }

      # Perform API call with POST
      req %>%
        httr2::req_user_agent(user_agent) %>%
        httr2::req_url_path_append(endpoint) %>%
        httr2::req_headers("Content-Type" = "application/x-www-form-urlencoded",
                           "username" = gen_auth_get(database = database)$username,
                           "password" = gen_auth_get(database = database)$password) %>%
        httr2::req_retry(max_tries = 3) %>%
        httr2::req_perform()

    }, error = function(e) {

      tryCatch( # tryCatch to try GET

        expr = {

          # Perform API call with GET (deprecated in GENESIS and Zensus 2022)
          httr2::request(url) %>%
            httr2::req_user_agent(user_agent) %>%
            httr2::req_url_path_append(endpoint) %>%
            httr2::req_url_query(!!!gen_auth_get(database = database), ...) %>%
            httr2::req_retry(max_tries = 3) %>%
            httr2::req_perform()

        }, error = function(e) {

          stop(paste0("The API call(s) have been tried with GET and POST methods, but were unsuccessful (error message: '", e$message, "'). Check your specifications or try again later."),
               call. = FALSE)

      })

    })

  #-----------------------------------------------------------------------------

}
