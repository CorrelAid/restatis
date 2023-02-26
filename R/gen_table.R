#' gen_table
#'
#' @description Download a table with data from Genesis
#'
#' @param name Name of the table
#' @param ... TODO
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gen_table("21311-0001")
#' }
#'
gen_table <- function(name, ...) {

  gen_api("data/tablefile",
          name = name,
          ...,
          format = "ffcsv") %>%
    httr2::resp_body_string() %>%
    readr::read_delim(delim = ";",
                      show_col_types = FALSE)
}
