#' gen_list_jobs
#'
#' @description Function to list all current jobs connected to the given user in the GENESIS or regionalstatistik.de database. Important note: For this function it is also possible to use `searchcriterion` parameter and `selection` parameter, making it possible to filter the job list based on 'type','time','status' or 'code'. For more details see `vignette("additional_parameter")`.
#'
#' @param database Character string. Indicator if 'genesis' or 'regionalstatistik.de' database is called. Default option is 'genesis'.
#' @param sortcriterion Character string. Indicator if the output should be sorted by 'type','time','status' or 'code'. This is a parameter of the API call itself. The default is 'type'.
#' @param flat Boolean. Should the function return a list with jobs and metadata ('FALSE') or just a flat data.frame ('TRUE')? Defaults to FALSE.
#' @param ... Additional parameters for the API call. These parameters are only affecting the call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list or data.frame (see parameter 'flat') of all current jobs of the user.
#' @export
#'
#' @examples
#' \dontrun{
#' gen_list_jobs("regio", flat = TRUE)
#' }
#'
gen_list_jobs <- function(database = c("genesis", "regio"),
                          sortcriterion = c("type", "time", "status", "code"),
                          flat = FALSE,
                          ...) {

  gen_fun <- test_database_function(database,
                                    error.input = TRUE,
                                    text = TRUE)

  if (length(database) != 1) {

    stop("This function allows only two values of  'database': 'genesis' or 'regio'.",
         call. = FALSE)

  }

  if (!is.character(sortcriterion)) {

    stop("Parameter 'sortcriterion' has to be of type 'character'.",
         call. = FALSE)

  }

  sortcriterion <- match.arg(sortcriterion)

  if (!(sortcriterion %in% c("type", "time", "status", "code"))) {

    stop("Parameter 'sortcriterion' has to be 'type', 'time', 'status', or 'code'.",
         call. = FALSE)

  }

  #-----------------------------------------------------------------------------

  if (gen_fun == "gen_api"){

    par_list <-  list(endpoint = "catalogue/jobs",
                      sortcriterion = sortcriterion,
                      ...)

  } else if (gen_fun == "gen_regio_api") {

    par_list <-  list(endpoint = "catalogue/jobs",
                      sortcriterion = sortcriterion,
                      ...)

  } else {

    stop("Misspecification of the parameter 'database': Only 'genesis' and 'regio' allowed.",
         call. = FALSE)

  }

  results_raw <- do.call(gen_fun, par_list)

  results_json <- test_if_json(results_raw)

  #-----------------------------------------------------------------------------

  if (flat == FALSE) {

    res <- list("Output" = tibble::as_tibble(binding_lapply(results_json$List,
                                             characteristics = c("State",
                                                                 "Code",
                                                                 "Date",
                                                                 "Time",
                                                                 "Content"))))

    attr(res, "Database") <- database[1]
    attr(res, "Sortcriterion") <- results_json$Parameter$sortcriterion
    attr(res, "Language") <- results_json$Parameter$language
    attr(res, "Copyright") <- results_json$Copyright

    return(res)

  } else if (flat == TRUE) {

    res <- tibble::as_tibble(binding_lapply(results_json$List,
                                            characteristics = c("State",
                                                                "Code",
                                                                "Date",
                                                                "Time",
                                                                "Content")))

    return(res)

  } else {

    stop("Misspecification of the parameter 'flat': Only TRUE or FALSE allowed.",
         call. = FALSE)

  }

}

#-------------------------------------------------------------------------------

#' gen_download_job
#'
#' @param name Character string. The job code retrieved by using gen_list_jobs().
#' @param database Character string. Indicator if the GENESIS ('genesis') or regionalstatistik.de ('regio') database is called. Only one database can be addressed per function call. Default option is 'genesis'.
#' @param area Character string. Indicator from which area of the database the results are called. In general, 'all' is the appropriate solution. Default option is 'all'.
#' @param compress Boolean. Should empty rows and columns be discarded? Default is FALSE.
#' @param language Character string. Defines if the decimal mark and grouping mark of integers should be represented based on the European (e.g.: '100,5', '200.000,5') or American ('100.5', '200,000.5') system. Defaults to 'Sys.getenv("GENESIS_LANG")'.
#' @param all_character Boolean. Should all variables be imported as 'character' variables? Avoids fuzzy data type conversions if there are leading zeros or other special characters. Defaults to TRUE.
#'
#' @return Returns a data.frame with the table content
#' @export
#'
#' @examples
#' \dontrun{
#' gen_download_job("21311-00-01-1_123456789", "regio")
#' }
#'
gen_download_job <- function(name,
                             database = c("genesis", "regio"),
                             area = c("all", "public", "user"),
                             compress = FALSE,
                             language = Sys.getenv("GENESIS_LANG"),
                             all_character = TRUE) {

  #-----------------------------------------------------------------------------

  database <- match.arg(database)

  area <- match.arg(area)

  area <- switch(area,
                 all = "all",
                 public = "\u00F6ffentlich",
                 user = "benutzer")

  #-----------------------------------------------------------------------------
  # Parameter processing of 'all character' for later use in read_delim

  if (isTRUE(all_character)) {

    all_character <- expression(readr::cols(.default = readr::col_character()))

  } else if (isFALSE(all_character)) {

    all_character <- expression(readr::cols())

  } else {

    stop("Misspecification of parameter 'all_character'. Has to be TRUE or FALSE.",
         call. = FALSE)

  }

  #-----------------------------------------------------------------------------

  if (database == "genesis") {

    response <- gen_api("data/resultfile",
                        name = name,
                        area = area,
                        compress = compress,
                        format = "ffcsv",
                        language = language)

    response_type <- resp_check_data(response)

  } else if (database == "regio"){

    response <- gen_regio_api("data/resultfile",
                              name = name,
                              area = area,
                              compress = compress,
                              format = "ffcsv",
                              language = language)

    response_type <- resp_check_data(response)

  } else {

    stop("Misspecification of parameter 'database': Can only be 'genesis' or 'regio'.",
         call. = FALSE)

  }

  #-----------------------------------------------------------------------------

  if (response_type == "text/csv") {

    if (language == "de") {

      result <- response %>%
        httr2::resp_body_string() %>%
        readr::read_delim(delim = ";",
                          show_col_types = FALSE,
                          locale = readr::locale(decimal_mark = ",",
                                                 grouping_mark = "."),
                          name_repair = "minimal",
                          col_types = eval(all_character))

    } else if (language == "en") {

      result <- response %>%
        httr2::resp_body_string() %>%
        readr::read_delim(delim = ";",
                          show_col_types = FALSE,
                          name_repair = "minimal",
                          col_types = eval(all_character))

    } else {

      stop("Error handling the parsing of your request (locale not 'de' or 'en').",
           call. = FALSE)

    } # End of language check

  #-----------------------------------------------------------------------------

    return(result)

  } else {

    stop("The response type of the job request is invalid (not 'text/csv').\n You might have chosen the wrong database to check for your job.",
         call. = FALSE)

  } # End of response type check

}
