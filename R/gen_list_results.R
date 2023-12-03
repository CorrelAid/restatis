#' gen_list_results: Get List of Results of Your User Account
#'
#' @description Function to list all current results connected to the given user in Genesis or Zensus. Important note: For this function it is also possible to use `selection`-parameter, making it possible to filter the results based on the 'code' of the object. For more details see `vignette("additional_parameter")`.
#'
#' @param database Character string. Indicator if the Genesis or Zensus database is called. Default option is 'genesis'.
#' @param area A string. Indicator from which area of the database the results are called. In general, "all" is the appropriate solution. Default option is 'all'.
#' @param ... Additional parameters for the Genesis/Zensus API call. These parameters are only affecting the Genesis/Zensus call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list of all current jobs connected to the given user.
#' @export
#'
gen_list_results <- function(database = c("genesis", "zensus"),
                             area = c("all", "public", "user"),
                             ...
) {

  gen_fun <- test_database_function(database)

  ##############################################################

  if(gen_fun == "gen_api"){

    par_list <-  list(
      endpoint = "catalogue/results",
      area = area,
      ...
    )

  } else if ( gen_fun == "gen_zensus_api"){

    par_list <-  list(
      endpoint = "catalogue/results",
      area = area,
      ...
    )

  }

  results_raw <- do.call(gen_fun, par_list)

  results_json <- test_if_json(results_raw)

  res <- list("Output" = tibble::as_tibble(binding_lapply(results_json$List,
                                                 characteristics = c("Code",
                                                                     "Content",
                                                                     "Values"))))


  attr(res, "Database") <- database[1]
  attr(res, "Database") <- results_json$Parameter$area
  attr(res, "Language") <- results_json$Parameter$language
  attr(res, "Copyright") <- results_json$Copyright

  return(res)

}
