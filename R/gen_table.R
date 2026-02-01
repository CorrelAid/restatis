#' gen_table
#'
#' @description Download a cube with data from GENESIS, regionalstatistik.de, Zensus 2022, landesdatenbank.nrw.de, bildungsmonitoring.de, statistikdaten.bayern.de or genesis.sachsen-anhalt.de database
#'
#' @param name Character string. Name/code of the table. Use of wildcards (`*`) is possible.
#' @param database Character string. Indicator if the GENESIS ('genesis'), Zensus 2022 ('zensus'), regionalstatistik.de ('regio'), statistikdaten.bayern.de ('bayern'), landesdatenbank.nrw.de ('nrw'), bildungsmonitoring.de ('bildung') or genesis.sachsen-anhalt.de ('st') database is called.
#' @param credential_list A list containing the credentials for the databases to be accessed. If 'NULL' (default), the function will use the stored credentials from \code{gen_auth_get()}.
#' @param area Character string. The area in which the table is stored.
#'   Possible values:
#'   \itemize{
#'     \item \code{"public"}: table in the public catalogue
#'     \item \code{"user"}: table in the user's account
#'     \item \code{"all"}: both of the above
#'   }
#' @param compress Boolean. Should empty rows and columns be discarded?
#' @param transpose Boolean. Reshape the table between \code{"wide"} and \code{"long"} format.
#' @param startyear Four-digit integer. Only retrieve data from this year onward.
#' @param endyear Four-digit integer. Only retrieve data up to this year.
#' @param regionalvariable Character string. Code of the regional variable whose value
#'   is specified in \code{regionalkey} to filter the results.
#' @param regionalkey Character string. One or more regional keys. Multiple values can be
#'   supplied as a character vector or as a single string, with the regional keys
#'   separated by commas. Use of wildcard (\code{*}) allowed.
#' @param classifyingvariable1 Character string. Code of the subject classification
#'   (SK-Merkmal) to which the selection by means of \code{classifyingkey1} is applied.
#' @param classifyingvariable2 Character string. Code of the subject classification
#'   (SK-Merkmal) to which the selection by means of \code{classifyingkey2} is applied.
#' @param classifyingvariable3 Character string. Code of the subject classification
#'   (SK-Merkmal) to which the selection by means of \code{classifyingkey3} is applied.
#' @param classifyingkey1 Character string. One or more values of a subject classification
#'   (e.g. \code{"WZ93012"}). Applied to \code{classifyingvariable1}. Multiple keys can be
#'   supplied as a character vector or as a single string, with the keys separated by
#'   commas. Use of wildcard (\code{*}) allowed.
#' @param classifyingkey2 Character string. One or more values of a subject classification.
#'   Applied to \code{classifyingvariable2}. Multiple keys can be supplied as a character
#'   vector or as a single string, with the keys separated by commas. Use of wildcard
#'   (\code{*}) allowed.
#' @param classifyingkey3 Character string. One or more values of a subject classification.
#'   Applied to \code{classifyingvariable3}. Multiple keys can be supplied as a character
#'   vector or as a single string, with the keys separated by commas. Use of wildcard
#'   (\code{*}) allowed.
#' @param stand Character string, format \code{"DD.MM.YYYY"}. Only retrieve data updated
#'   after this date.
#' @param language Character string. Search terms, returned messages and data descriptions
#'   in German (\code{"de"}) or English (\code{"en"}).
#' @param job Boolean. Indicate whether a job should be created (not available with the
#'   \code{"Zensus"} database). To set \code{job = TRUE}, username and password must be
#'   saved with \code{gen_auth_save()}; using API tokens with \code{job = TRUE} will
#'   result in an error.
#' @param all_character Boolean. Should all variables be imported as character variables?
#'   Avoids fuzzy data type conversions if there are leading zeros or other special
#'   characters. Defaults to \code{TRUE}.
#' @param ... Additional parameters for the API call (see respective API documentation).
#'   A valid specification of these will not be checked by the function, so wrongful
#'   specification may lead to errors.
#'
#' @return A [tibble][tibble::tibble()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gen_table("21311-0001", database = "genesis")
#' }
#'
gen_table <- function(name,
                      database = c("genesis", "zensus", "regio", "bayern", "nrw", "bildung", "st"),
                      credential_list = NULL,
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
                      language = Sys.getenv("RESTATIS_LANG"),
                      job = FALSE,
                      all_character = TRUE,
                      ...) {

  #-----------------------------------------------------------------------------
  # Parameter processing

  if (missing(database)) {

    stop("It is mandatory to specifiy the 'database' parameter for 'gen_table()'.",
         call. = FALSE)

  }

  database <- match.arg(database)

  check_credential_list_standalone(credential_list = credential_list,
                                   database = database)

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
  # Manage credentials related to jobs

  if (!is.null(credential_list) & isTRUE(job)) {

    warning("Please check if the jobs-related credentials are identical with your provided credentials - otherwise the function will fail.",
            call. = FALSE)

  } else {

    credentials <- gen_auth_get(database = database)

    cred_attr <- credentials %>% attributes %>% names

    if (!("credential_type" %in% cred_attr)) {

      stop("There has been an error specifying your credentials (missing credential type attribute). Please try again using 'gen_auth_save()'.",
           call. = FALSE)

    }

    if (isTRUE(job) & attr(credentials, "credential_type") == "token" & database == "genesis") {

      stop(paste0("It is not possible to set 'job = TRUE' when an API token is used for authentication.\n",
                  "Use 'gen_auth_save(\"", database, "\", use_token = FALSE)' and input username and password to enable creating jobs.\n",
                  "See README for more information."),
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------
  # Data download

  response <- gen_api(endpoint = "data/tablefile",
                      database = database,
                      credential_list = credential_list,
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
                      job = job,
                      ...)

  #-----------------------------------------------------------------------------
  # Data processing

  response_type <- resp_check_data(response)

  # Returning the table desired by the user
  return(return_table_object(response = response,
                             response_type = response_type,
                             language = language,
                             all_character = all_character))

}
