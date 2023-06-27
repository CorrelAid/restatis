#' Get the Data of a Table From GENESIS
#'
#' @description Download a table with data from GENESIS.
#'
#' @param name a string. Name of the table. Use of wildcards (`*`) allowed.
#' @param ... Optional parameters passed on to the GENESIS API call:
#'   \describe{
#'     \item{\code{area}}{a string. The area in which the table is stored. Possible values:
#'     \itemize{
#'       \item \code{"public"}: table in the public catalogue
#'       \item \code{"user"}: table in the user's account
#'     }}
#'     \item{\code{compress}}{a logical. Should empty rows and columns be discarded?}
#'     \item{\code{transpose}}{a logical. Reshape the table between "wide" and
#'       "long" format.}
#'     \item{\code{startyear,endyear}}{a number. Only retrieve data between these years.}
#'     \item{\code{regionalvariable}}{character. Code of the regional variable
#'     whose value is specified in \code{regionalkey} to filter the results.}
#'     \item{\code{regionalkey}}{character. One or more regional keys. Multiple
#'       values can be supplied as a character vector or as a single string,
#'       with the regional keys separated by commas. Use of wildcard (`*`) allowed.}
#'     \item{\code{classifyingvariable1,classifyingvariable2
#'       ,classifyingvariable3}}{character. Code of the subject classification
#'       (SK-Merkmal) to which the selection by means of the corresponding
#'       `classifyingkey` parameter is to be applied.}
#'     \item{\code{classifyingkey1,classifyingkey2,classifyingkey3}}{character.
#'       One or more values of a subject classification (e.g. "WZ93012"). Applied
#'       to the corresponding `classifyingvariable` parameter. Multiple
#'       keys can be supplied as a character vector or as a single string,
#'       with the keys separated by commas. Use of wildcard (`*`) allowed.}
#'     \item{\code{stand}}{a string \code{"DD.MM.YYYY"}. Only retrieve data
#'       updated after this #' date.}
#'     \item{\code{language}}{Search terms, returned messages and data
#'       descriptions in German (`"de"`) or English (`"en"`)?}
#'   }
#'
#' @return A [tibble][tibble::tibble()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gen_table("21311-0001")
#' }
#'
gen_table <- function(name, ...) {
  gen_table_(name, ...)
}

#-------------------------------------------------------------------------------

gen_table_ <- function(name,
                       area = c("public", "user"),
                       compress = FALSE,
                       transpose = FALSE,
                       startyear = 1900,
                       endyear = 2100,
                       regionalvariable = NULL,
                       regionalkey = NULL,
                       classifyingvariable1 = NULL,
                       classifyingkey1 = NULL,
                       classifyingvariable2 = NULL,
                       classifyingkey2 = NULL,
                       classifyingvariable3 = NULL,
                       classifyingkey3 = NULL,
                       stand = NULL,
                       language = Sys.getenv("GENESIS_LANG")) {
  area <- match.arg(area)

  if (!isTRUE(language == "en")) {
    area <- switch(area,
      public = "\u00F6ffentlich",
      user = "benutzer"
    )
  }

  param_check_year(startyear)
  param_check_year(endyear)

  regionalkey <- param_collapse_vec(regionalkey)
  classifyingkey1 <- param_collapse_vec(classifyingkey1)
  classifyingkey2 <- param_collapse_vec(classifyingkey2)
  classifyingkey3 <- param_collapse_vec(classifyingkey3)

  resp <- gen_api("data/tablefile",
    name = name,
    area = area,
    compress = compress,
    transpose = transpose,
    startyear = startyear,
    endyear = endyear,
    regionalvariable = regionalvariable,
    regionalkey = regionalkey,
    classifyingvariable1 = classifyingvariable1,
    classifyingkey1 = classifyingkey1,
    classifyingvariable2 = classifyingvariable2,
    classifyingkey2 = classifyingkey2,
    classifyingvariable3 = classifyingvariable3,
    classifyingkey3 = classifyingkey3,
    stand = stand,
    language = language,
    format = "ffcsv",
    job = FALSE
  )

  resp_check_data_csv(resp)

  resp %>%
    httr2::resp_body_string() %>%
    readr::read_delim(
      delim = ";",
      show_col_types = FALSE
    )
}
