#' gen_list_jobs: Explore current jobs of your user account
#'
#' @description Function to list all current jobs connected to the given user.
#'
#' @param selection Filter the list of jobs for matching codes.
#' @param sortcriterion Allows to sort the resulting list of jobs by their Code ("content"), the time of completion ("time") or status ("status")
#' @param database Character string. Indicator if the Destatis or Zensus database is called.
#' @param ... Additional parameters for the Genesis API call. These parameters are only affecting the Genesis call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list of all current jobs connected to the given user.
#' @export
#'
gen_list_jobs <- function(selection = NULL,
                          sortcriterion = c("content", "time", "status"),
                          database = c("zensus", "destatis"),
                          ...
                      ) {

  if( length(database) == 1 & database == "zensus" ){

    results_raw <- gen_zensus_api("catalogue/jobs",
                           username = gen_zensus_auth_get()$username,
                           password = gen_zensus_auth_get()$password,
                           selection = selection,
                           sortcriterion = sortcriterion,
                           ...)

  } else if ( length(database) == 1 & database == "destatis" ){

    results_raw <- gen_api("catalogue/jobs",
                           username = gen_auth_get()$username,
                           password = gen_auth_get()$password,
                           selection = selection,
                           sortcriterion = sortcriterion,
                           ...)

  } else {

    stop("Parameter 'database' has to be 'zensus' or 'destatis'.",
         call. = FALSE)

  }

  results_json <- test_if_json(results_raw)

  return(results_json)

}
