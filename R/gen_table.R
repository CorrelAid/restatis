#' gen_table
#'
#' @description Download a table with data from GENESIS, Zensus 2022 or regionalstatistik.de databases
#'
#' @param name Character string. Name/code of the table. Use of wildcards (`*`) is possible.
#' @param ... Optional parameters passed on to the Genesis API call:
#'   \describe{
#'     \item{\code{area}}{Character string. The area in which the table is stored. Possible values:
#'     \itemize{
#'       \item \code{"public"}: table in the public catalogue
#'       \item \code{"user"}: table in the user's account
#'       \item \code{"all"}: both of the above
#'     }}
#'     \item{\code{compress}}{Boolean. Should empty rows and columns be discarded?}
#'     \item{\code{transpose}}{Boolean. Reshape the table between "wide" and
#'       "long" format.}
#'     \item{\code{startyear,endyear}}{Four-digit integers. Only retrieve data between these years.}
#'     \item{\code{regionalvariable}}{Character string. Code of the regional variable
#'     whose value is specified in \code{regionalkey} to filter the results.}
#'     \item{\code{regionalkey}}{Character string. One or more regional keys. Multiple
#'       values can be supplied as a character vector or as a single string,
#'       with the regional keys separated by commas. Use of wildcard (`*`) allowed.}
#'     \item{\code{classifyingvariable1,classifyingvariable2
#'       ,classifyingvariable3}}{Character string. Code of the subject classification
#'       (SK-Merkmal) to which the selection by means of the corresponding
#'       `classifyingkey` parameter is to be applied.}
#'     \item{\code{classifyingkey1,classifyingkey2,classifyingkey3}}{Character string.
#'       One or more values of a subject classification (e.g. "WZ93012"). Applied
#'       to the corresponding `classifyingvariable` parameter. Multiple
#'       keys can be supplied as a character vector or as a single string,
#'       with the keys separated by commas. Use of wildcard (`*`) allowed.}
#'     \item{\code{stand}}{Character string, format: \code{"DD.MM.YYYY"}. Only retrieve data
#'       updated after this date.}
#'     \item{\code{language}}{Search terms, returned messages and data
#'       descriptions in German (`"de"`) or English (`"en"`)?}
#'     \item{\code{job}}{Boolean. Indicate as to whether a job should be created
#'        (not available with the 'Zensus' database).)}
#'     \item{\code{all_character}}{Boolean. Should all variables be imported as
#'        'character' variables? Avoids fuzzy data type conversions if there are
#'        leading zeros or other special characters. Defaults to TRUE.}
#'   }
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
                       database = c("genesis", "zensus", "regio"),
                       area = c("all", "public", "user"),
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
                       language = Sys.getenv("GENESIS_LANG"),
                       job = FALSE,
                       all_character = TRUE) {

  #-----------------------------------------------------------------------------
  # Parameter processing

  database <- match.arg(database)

  area <- match.arg(area)

  if (!isTRUE(language == "en")) {

    area <- switch(area,
                   all = "all",
                   public = "\u00F6ffentlich",
                   user = "benutzer")

  }

  param_check_year(startyear)
  param_check_year(endyear)
  param_check_regionalkey(regionalkey)

  regionalkey <- param_collapse_vec(regionalkey)
  classifyingkey1 <- param_collapse_vec(classifyingkey1)
  classifyingkey2 <- param_collapse_vec(classifyingkey2)
  classifyingkey3 <- param_collapse_vec(classifyingkey3)

  #-----------------------------------------------------------------------------
  # Data download

  if(database == "zensus"){

    response <- gen_zensus_api("data/tablefile",
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
                               job = FALSE)

  #-----------------------------------------------------------------------------

  } else if (database == "genesis"){

    response <- gen_api("data/tablefile",
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
                        job = job)

  #-----------------------------------------------------------------------------

  } else if (database == "regio") {

    response <- gen_regio_api("data/tablefile",
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
                              job = job)

  #-----------------------------------------------------------------------------

  } else {

    stop("Parameter 'database' has to be 'zensus', 'regio' or 'genesis'.",
         call. = FALSE)

  }

  #-----------------------------------------------------------------------------
  # Data processing

  response_type <- resp_check_data(response)

  # Returning the table desired by the user
  return(return_table_object(response = response,
                             response_type = response_type,
                             language = language,
                             all_character = all_character))

}
