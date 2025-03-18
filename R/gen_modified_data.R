#' gen_modified_data
#'
#' @description Function to check for updates, changes, or new objects based on a specific date.
#'
#' @param code A character string with a maximum length of 15 characters. Code from a GENESIS, Zensus 2022 or regionalstatistik.de object. Only one code per iteration.
#' @param database Character string. Indicator if the GENESIS ('genesis'), Zensus 2022 ('zensus') or regionalstatistik.de ('regio') database is called. Default option is 'all'.
#' @param type Character string. Specific GENESIS and regionalstatistik.de object types: 'tables', 'statistics', and 'statisticsUpdates'. Specific Zensus 2022 object types: 'tables' and 'statistics'. All types that are specific for one database can be used together through 'all', which is the default.
#' @param date Character string. Specific date that is used as the last update or upload time to include an object in return. Default option is 'now', which uses the current date of your system. Alternative options are 'week_before', using the current date of your system minus 7 days, 'month_before', using the current date of your system minus 4 weeks, and 'year_before', using the current date of your system minus 52 weeks. Additionally, it is possible to fill in a specific date of format 'DD.MM.YYYY'.
#' @param pagelength Integer. Maximum length of results or objects (e.g., number of tables). Defaults to 500. Maximum of the databases is 25,000 objects.
#' @param verbose Boolean. Indicator if the output of the function should include detailed messages and warnings. Default option is 'TRUE'. Set the parameter to 'FALSE' to suppress additional messages and warnings.
#' @param ... Additional parameters for the API call. These parameters are only affecting the call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list with all recalled elements from the API. Always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing with the data. Attributes are added to the data.frame describing the search configuration for the returned output.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Find objects which were modified last_week
#' object <- gen_modified_data(date = "week_before")
#'
#' # Find tables that were new from 31.03.2020
#' object <- gen_modified_data(type = "tables", date = "31.03.2020")
#'
#' # Find objects related to the topic "Bevoelkerung" (Code: '12*') which were new today
#' object <- gen_modified_data(code = "12*")
#' }
#'
gen_modified_data <- function(code = "",
                              database = c("all", "genesis", "zensus", "regio"),
                              type = c("all", "tables", "statistics", "statisticsUpdates"),
                              date = c("now", "week_before", "month_before", "year_before"),
                              pagelength = 500,
                              verbose = TRUE,
                              ...) {

  type <- match.arg(type)

  date <- check_function_input(code = code,
                               type = type,
                               date = date,
                               pagelength = pagelength,
                               database = database,
                               verbose = verbose)

  # database_vector will hold a vector of the specified databases to query
  database_vector <- test_database_function(database,
                                            error.input = TRUE,
                                            text = verbose)

  #-----------------------------------------------------------------------------

  if (date == "now") {

    date <- format(Sys.Date(), format = "%d.%m.%Y")

  } else if (date == "week_before") {

    date <- format(Sys.Date() - 7, format = "%d.%m.%Y")

  } else if (date == "month_before") {

    date <- format(Sys.Date() - as.difftime(4, units = "weeks"),
                   format = "%d.%m.%Y")

  } else if (date == "year_before") {

    date <- format(as.difftime(52, units = "weeks"), format = "%d.%m.%Y")
  }

  #-----------------------------------------------------------------------------

  # Processing #
  res <- lapply(database_vector, function(db){

    if (isTRUE(verbose)) {

      info <- paste("Started the processing of", db, "database.")

      message(info)

    }

    #---------------------------------------------------------------------------

    if (type == "tables") {

      results_raw <- gen_api(endpoint = "catalogue/modifieddata",
                             database = db,
                             username = gen_auth_get(database = db)$username,
                             password = gen_auth_get(database = db)$password,
                             selection = code,
                             type = "Neue Tabellen",
                             date = date,
                             pagelength = pagelength,
                             ...)

      results_json <- test_if_json(results_raw)

      test_if_error_light(results_json)

    }

    #---------------------------------------------------------------------------

    if (type == "statistics") {

      results_raw <- gen_api(endpoint = "catalogue/modifieddata",
                             database = db,
                             username = gen_auth_get(database = db)$username,
                             password = gen_auth_get(database = db)$password,
                             selection = code,
                             type = "Neue Statistiken",
                             date = date,
                             pagelength = pagelength,
                             ...)

      results_json <- test_if_json(results_raw)

      test_if_error_light(results_json)


    }

    #---------------------------------------------------------------------------

    if (type == "statisticsUpdates") {

      if (db == "genesis" | db == "regio") {

        results_raw <- gen_api(endpoint = "catalogue/modifieddata",
                               database = db,
                               username = gen_auth_get(database = db)$username,
                               password = gen_auth_get(database = db)$password,
                               selection = code,
                               type = "Aktualisierte Statistiken",
                               date = date,
                               pagelength = pagelength,
                               ...)

      }

      results_json <- test_if_json(results_raw)

      test_if_error_light(results_json)

    }

    #---------------------------------------------------------------------------

    if (type == "all") {

      results_raw <- gen_api(endpoint = "catalogue/modifieddata",
                             database = db,
                             username = gen_auth_get(database = db)$username,
                             password = gen_auth_get(database = db)$password,
                             selection = code,
                             type = "all",
                             date = date,
                             pagelength = pagelength,
                             ...)

      results_json <- test_if_json(results_raw)

      test_if_error_light(results_json)

    }

    if (is.null(unlist(results_json$List))) {

      if (isTRUE(verbose)) {

      message(paste0("No modified objects found for your code and date in ", db))

      }

      return("No modified objects found.")

    } else {

      table <- binding_lapply(results_json$List,
                              characteristics = c("Code",
                                                  "Content",
                                                  "Date",
                                                  "Added",
                                                  "Type"))


      table$Date <- as.Date.character(table$Date, format = "%d.%m.%Y")

      table <- tibble::as_tibble(table)
      table <- table[order(table$Date, decreasing = TRUE), ]

      list_resp <- list("Modified" = table)

      attr(list_resp, "Code") <- results_json$Parameter$selection
      attr(list_resp, "Database") <- db
      attr(list_resp, "Type") <- results_json$Parameter$type
      attr(list_resp, "Date") <- results_json$Parameter$date
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyright") <- results_json$Copyright

      return(list_resp)

    }

  })

  #-----------------------------------------------------------------------------

  if (!is.null(unlist(res))) {

    res <- check_results(res)

    return(res)

  }

}
