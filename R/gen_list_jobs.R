#' Search for Current Jobs of Your User Account
#'
#' @description Function to list all current jobs connected to the given user.
#'
#' @param selection Filter the list of jobs for matching codes.
#' @param sortcriterion Allows to sort the resulting list of jobs by their Code ("content"), the time of completion ("time") or status ("status")
#' @param ... Additional parameter of the GENESIS API call. These parameters are only affecting the GENESIS API call itself, no further processing.
#'
#' @return A list of all current jobs connected to the given user.
#' @export
#'
gen_list_jobs <- function(selection = NULL,
                          sortcriterion = c("content", "time", "status"),
                          ...) {
  results_raw <- gen_api("catalogue/jobs",
    selection = selection,
    sortcriterion = sortcriterion,
    ...
  )

  results_json <- test_if_json(results_raw)

  return(results_json)
}
