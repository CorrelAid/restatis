#' gen_logincheck
#'
#' @description Function to check if a login is possible for a certain database.
#'
#' @param database Character string. Indicator if the GENESIS ('genesis'), Zensus 2022 ('zensus') or regionalstatistik.de ('regio') database is called. Only one database can be addressed per function call. Default option is 'genesis'.
#' @param verbose Boolean. In case of success, should a message be printed? Defaults to FALSE.
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
gen_logincheck <- function(database,
                           verbose = FALSE,
                           ...) {

  logincheck_http_error(database,
                        verbose,
                        ...)

}
