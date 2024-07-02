#' gen_list_jobs: Explore Current Jobs of Your User Account
#'
#' @description Function to list all current jobs connected to the given user in Genesis or Zensus. Important note: For this function it is also possible to use `searchcriterion`-parameter and `selection`-parameter, making it possible to filter the job list based on 'type','time','status' or 'code'. For more details see `vignette("additional_parameter")`.
#'
#' @param database Character string. Indicator if the Genesis or Zensus database is called. Default option is 'genesis'.
#' @param sortcriterion A string. Indicator if the output should be sorted by 'type','time','status' or 'code'. This is a parameter of the Genesis/Zensus API call itself. The default is 'type'.
#' @param ... Additional parameters for the Genesis/Zensus API call. These parameters are only affecting the Genesis/Zensus call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list of all current jobs connected to the given user.
#' @export
#'
gen_list_jobs <- function(sortcriterion = c("type", "time", "status", "code"),
                          database = c("genesis", "zensus", "regio"),
                          ...) {

  gen_fun <- test_database_function(database)

  if (!is.character(sortcriterion)) {
    stop("Parameter 'sortcriterion' has to be of type 'character'.",
         call. = FALSE
    )
  }

  sortcriterion <- match.arg(sortcriterion)

  if(! sortcriterion %in% c("type", "time", "status", "code")){
    stop("Parameter 'sortcriterion' has to be 'type', 'time', 'status', or 'code'.",
         call. = FALSE
         )
  }

  #-----------------------------------------------------------------------------

  if(gen_fun == "gen_api"){

    par_list <-  list(
      endpoint = "catalogue/jobs",
      sortcriterion = sortcriterion,
      ...
    )

  } else if (gen_fun == "gen_zensus_api"){

    par_list <-  list(
      endpoint = "catalogue/jobs",
      sortcriterion = sortcriterion,
      ...
    )

  } else if (gen_fun == "gen_regio_api") {

    par_list <-  list(
      endpoint = "catalogue/jobs",
      sortcriterion = sortcriterion,
      ...
    )

  }

  results_raw <- do.call(gen_fun, par_list)

  results_json <- test_if_json(results_raw)

  res <- list("Output" = tibble::as_tibble(binding_lapply(results_json$List,
                                        characteristics = c("State",
                                                            "Code",
                                                            "Date",
                                                            "Time",
                                                            "Content"))))


  attr(res, "Database") <- database[1]
  attr(res, "Sortcriterion") <- results_json$Parameter$sortcriterion
  attr(res, "Language") <- results_json$Parameter$language
  attr(res, "Copyright") <- results_json$Copyright

  return(res)

}
