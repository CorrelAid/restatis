#' gen_list_jobs: Explore current jobs of your user account
#'
#' @description Function to list all current jobs connected to the given user.
#'
#' @param code Filter the list of jobs for matching codes.
#' @param sortcriterion Allows to sort the resulting list of jobs for Genesis by their Code ("content"), the time of completion ("time") or status ("status") and for Zensus by the time of completion ("time") or status ("status"). Default option is 'status'.
#' @param database Character string. Indicator if the Genesis or Zensus database is called. Default option is 'genesis'.
#' @param ... Additional parameters for the Genesis/Zensus API call. These parameters are only affecting the Genesis/Zensus call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list of all current jobs connected to the given user.
#' @export
#'
gen_list_jobs <- function(code = NULL,
                          sortcriterion = c("status", "time", "content"),
                          database = c("genesis", "zensus"),
                          ...
                      ) {

  gen_fun <- test_database_function(database)

  sortcriterion <- match.arg(sortcriterion)

  if( sortcriterion == "content" & gen_fun == "gen_zensus_api"){

    stop("Parameter 'sortcriterion' has to be 'status' or 'time' for 'zensus'-database.", call. = FALSE)

  }

  if(gen_fun == "gen_api"){

    par_list <-  list(
      endpoint = "catalogue/jobs",
      selection = selection,
      sortcriterion = sortcriterion,
      ...
    )

  } else if ( gen_fun == "gen_zensus_api"){

    par_list <-  list(
      endpoint = "catalogue/jobs",
      selection = selection,
      sortcriterion = sortcriterion,
      ...
    )

  }

  results_raw <- do.call(gen_fun, par_list)

  results_json <- test_if_json(results_raw)

  return(results_json)

}
