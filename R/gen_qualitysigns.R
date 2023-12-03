#' gen_signs: Explore Meaning of Special Signs in the Objects
#'
#' @description Function to list all current used special signs (e.g., 0, *, X, (), p, ...) and their meaning in Genesis or Zensus.
#'
#' @param database Character string. Indicator if the Genesis or Zensus database is called. Default option is 'genesis'.
#' @param ... Additional parameters for the Genesis/Zensus API call. These parameters are only affecting the Genesis/Zensus call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list of all current used special signs.
#' @export
#'
gen_signs <- function(database = c("genesis", "zensus"),
                      ...
) {

  gen_fun <- test_database_function(database)

  if(gen_fun == "gen_api"){

    par_list <-  list(
      endpoint = "catalogue/qualitysigns",
      ...
    )

  } else if ( gen_fun == "gen_zensus_api"){

    par_list <-  list(
      endpoint = "catalogue/qualitysigns",
      ...
    )

  }

  results_raw <- do.call(gen_fun, par_list)

  results_json <- test_if_json(results_raw)

  res <- list("Output" = tibble::as_tibble(binding_lapply(results_json$List,
                        characteristics = c("Code",
                                            "Content"))))


  attr(res, "Database") <- database[1]
  attr(res, "Language") <- results_json$Parameter$language
  attr(res, "Copyright") <- results_json$Copyright

  return(res)

}
