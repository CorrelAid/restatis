#' gen_logincheck
#'
#' @description Function to check if a login is possible for a certain database.
#' @param database Takes the values 'genesis', 'regio', 'zensus' and 'all'.
#' @param verbose Boolean. In case of success, should a message be printed? Defaults to FALSE.
#'
#' @return Leads to an informative error message if the login check failed. Invisibly returns TRUE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' gen_logincheck("zensus")
#' }
#'
gen_logincheck <- function(database, verbose = FALSE) {

  logincheck_http_error(database, verbose)

}
