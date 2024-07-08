#' gen_list_results
#'
#' @description Function to list all current results connected to the given user in the GENESIS, Zensus 2022 or regionalstatistik.de database. Important note: For this function it is also possible to use `selection` parameter, making it possible to filter the results based on the 'code' of the object. For more details see `vignette("additional_parameter")`.
#'
#' @param database Character string. Indicator if the GENESIS ('genesis'), Zensus 2022 ('zensus') or regionalstatistik.de ('regio') database is called. Only one database can be addressed per function call. Default option is 'genesis'.
#' @param area Character string. Indicator from which area of the database the results are called. In general, 'all' is the appropriate solution. Default option is 'all'. Not used for 'statistics'.
#' @param ... Additional parameters for the API call. These parameters are only affecting the call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list of all current results connected to the given user.
#' @export
#'
gen_list_results <- function(database = c("genesis", "zensus", "regio"),
                             area = c("all", "public", "user"),
                             ...) {

  gen_fun <- test_database_function(database)

  area <- match.arg(area)

  area <- switch(area,
                 all = "all",
                 public = "\u00F6ffentlich",
                 user = "benutzer")

  par_list <-  list(endpoint = "catalogue/results",
                    area = area,
                    ...)

  results_raw <- do.call(gen_fun, par_list)

  results_json <- test_if_json(results_raw)

  if (results_json$Status$Code %in% c(103, 104)) {

    stop("There are not (yet) any objects matching your request.",
         call. = FALSE)

  }

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
