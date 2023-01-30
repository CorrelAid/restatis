#' Get active jobs
#'
#' @description Function to list all current jobs connected to the given user.
#'
#' @param selection Filter the list of jobs for matching codes.
#' @param sortcriterion Allows to sort the resulting list of jobs by their Code ("content"), the time of completion ("time") or status ("status")
#'
#' @return A list of all current jobs connected to the given user.
#' @export
#'
#' @examples
#'


list_jobs <- function(selection = NULL,
                      sortcriterion = c("content", "time", "status"),
                      ...
                      ) {

  results_raw <- gen_api("catalogue/jobs",
                         username = gen_auth_get()$username,
                         password = gen_auth_get()$password,
                         selection = selection,
                         sortcriterion = sortcriterion,
                         ...
                         )

  results_json <- test_if_json(results_raw)

  # else {}

  # (YAB): There is an error message missing in the 'else' case
  # depending on the stability of the API, use tryCatch

  return(results_json)

}
