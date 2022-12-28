#' Get active jobs
#'
#' Function to list all active jobs.
#'
#' @param
#'
#' @return
#' @export
#'
#' @examples
#'

list_jobs <- function(selection = NULL,
                      #type = c("all"),
                      ...
                      ) {

  results_raw <- gen_api("catalogue/jobs",
                         username = gen_auth_get()$username,
                         password = gen_auth_get()$password,
                         #selection = selection
                         )

  if (httr2::resp_content_type(results_raw) == "application/json") {

    results_json <- httr2::resp_body_json(results_raw)

  } # else {}

  # (YAB): There is an error message missing in the 'else' case
  # depending on the stability of the API, use tryCatch

  return(results_json)

}
