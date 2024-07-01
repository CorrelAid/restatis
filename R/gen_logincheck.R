#' gen_logincheck
#'
#' @description Function to check if a login is possible for a certain database.
#' @param database Takes the values 'genesis', 'regio', 'zensus' and 'all'.
#'
#' @return Leads to an informative error message if the login check failed. Invisibly returns TRUE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' gen_logincheck("zensus")
#' }
#'
gen_logincheck <- function(database, verbose = FALSE) {

  if (database == "genesis") {

    response <- gen_api("helloworld/logincheck")
    warn_if_http_error(response, database, verbose)

  } else if (database == "zensus") {

    response <- gen_zensus_api("helloworld/logincheck")
    warn_if_http_error(response, database, verbose)

  } else if (database == "regio") {

    response <- gen_regio_api("helloworld/logincheck")
    warn_if_http_error(response, database, verbose)

  } else if (database == "all") {

    databases <- list("genesis", "zensus", "regio")

    response_list <- list(response_genesis = gen_api("helloworld/logincheck"),
                          response_zensus = gen_zensus_api("helloworld/logincheck"),
                          response_regio = gen_regio_api("helloworld/logincheck"))

    purrr::walk2(.x = response_list,
                 .y = databases,
                 .f = ~ warn_if_http_error(response = .x,
                                           database = .y,
                                           verbose = verbose))

  } else {

    stop("Misspecified parameter 'database' for function 'gen_logincheck'.",
         call. = FALSE)

  }

}
