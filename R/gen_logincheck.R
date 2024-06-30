#' gen_logincheck
#'
#' @description Function to check if a login is possible for a certain database.
#' @param database Takes the values 'genesis', 'regio' and 'zensus'.
#'
#' @return Leads to an informative error message if the login check failed. Invisibly returns TRUE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' gen_logincheck("zensus")
#' }
#'
gen_logincheck <- function(database) {

  if (database == "genesis") {

    response <- gen_api("helloworld/logincheck")

  } else if (database == "zensus") {

    response <- gen_zensus_api("helloworld/logincheck")

  } else if (database == "regio") {

    response <- gen_regio_api("helloworld/logincheck")

  } else {

    stop("Misspecified parameter 'database' for function 'perform_logincheck'.",
         call. = FALSE)

  }

  #-----------------------------------------------------------------------------

  if (response$status_code != 200) {

    stop(paste0("There seems to be an issue with the authentication process (logincheck upon credential specification failed with code ",
                response$status_code,
                "). ",
                "Please retry specifying your credentials or check whether the API is currently down."),
         call. = FALSE)

  } else {

  invisible(TRUE)

  }

}
