#' gen_signs
#'
#' @description Function to list all currently used special signs (e.g., 0, *, X, (), p, ...) and their meaning in GENESIS, Zensus 2022 and/or regionalstatistik.de.
#'
#' @param database Character string. Indicator if the GENESIS ('genesis'), Zensus 2022 ('zensus') or regionalstatistik.de ('regio') database is called. Default option is 'all'.
#' @param ... Additional parameters for the API call. These parameters are only affecting the call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list of all current used special signs.
#' @export
#'
gen_signs <- function(database = c("all", "genesis", "zensus", "regio"),
                      ...) {

  # database_vector will hold a vector of the specified databases to query
  database_vector <- test_database_function(database,
                                            error.input = error.ignore,
                                            text = verbose)

  res <- lapply(database_vector, function(db){

    results_raw <- gen_api(endpoint = "catalogue/qualitysigns",
                           database = db,
                           ...)

    results_json <- test_if_json(results_raw)

    mid_res <- list("Output" = tibble::as_tibble(binding_lapply(results_json$List,
                                                                characteristics = c("Code",
                                                                                    "Content"))))

    attr(mid_res, "Database") <- db
    attr(mid_res, "Language") <- results_json$Parameter$language
    attr(mid_res, "Copyright") <- results_json$Copyright

    return(mid_res)

  })

  res <- check_results(res)

  return(res)

}
