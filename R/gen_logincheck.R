#' gen_logincheck
#'
#' @description Function to check if a login is possible for a certain database.
#'
#' @param database Character string. Indicator if the GENESIS ('genesis'), Zensus 2022 ('zensus'), regionalstatistik.de ('regio'), statistikdaten.bayern.de ('bayern'), landesdatenbank.nrw.de ('nrw'), bildungsmonitoring.de ('bildung') or genesis.sachsen-anhalt.de ('sa') database is called. If all databases should be checked, use 'all'. Default option is 'all'.
#' @param verbose Boolean. In case of success, should a message be printed? Defaults to 'TRUE'.
#' @param ... Additional parameters for the API call. These parameters are only affecting the call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return Leads to an informative error message if the login check failed and returns FALSE invisibly. Invisibly returns TRUE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' gen_logincheck("zensus")
#' }
#'
gen_logincheck <- function(database = "all",
                           verbose = TRUE,
                           ...) {

  # database_vector will hold a vector of the specified databases to query
  database_vector <- test_database_function(database,
                                            error.input = TRUE,
                                            text = verbose)

  logincheck_http_error(database_vector,
                        verbose,
                        ...)

}
