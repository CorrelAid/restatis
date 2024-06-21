#' gen_modified_data: Explore New Added Objects or Changed Objects in Genesis/Zensus
#'
#' @description Function to check for updates, changes, or new objects in Genesis/Zensus based on a specific date.
#'
#' @param code A string with a maximum length of 15 characters. Code from a Genesis/Zensus object. Only one code per iteration. "*" notations are possible. Empty code (default value) includes all changes, updates, and new added objects.
#' @param database Character string. Indicator if the Genesis or Zensus database is called. Default option is 'genesis'.
#' @param type A string. Specific Genesis object type: 'tables', 'statistics', and 'statisticsUpdates'. Specific Zensus object type: 'tables' and 'statistics'. All types that are specific for one database can be used together through "all", which is the default.
#' @param date A string. Specific date that is used as the last update or upload time in Genesis/Zensus to include a Genesis/Zensus object in return. Default option is 'now', which uses the current date of your system. Alternative options are 'week_before', using the current date of your system minus 7 days, 'month_before', using the current date of your system minus 4 weeks, and 'year_before', using the current date of your system minus 52 weeks. Additionally, it is possible to fill in a specific date of format 'DD.MM.YYYY'.
#' @param ... Additional parameters for the Genesis/Zensus API call. These parameters are only affecting the Genesis/Zensus call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list with all recalled elements from Genesis/Zensus. Always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing with the data. Attributes are added to the data.frame describing the search configuration for the returned output.
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
#' # Find objects related to the topic "Bevölkerung" (Code: '12*') which were new today
#' object <- gen_modified_data(code = "12*")
#' }
#'
gen_modified_data <- function(code = "",
                              database = c("all", "genesis", "zensus", "regio"),
                              type = c("all", "tables", "statistics", "statisticsUpdates"),
                              date = c("now", "week_before", "month_before", "year_before"),
                              ...) {

  gen_fun <- test_database_function(database)

  type <- match.arg(type)

  date <- check_function_input(code = code,
                               type = type,
                               date = date,
                               database = gen_fun)

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

  # Processing ####
  res <- lapply(gen_fun, function(db){

    #---------------------------------------------------------------------------
    if (type == "tables") {

      par_list <-  list(
        endpoint = "catalogue/modifieddata",
        username = gen_auth_get(database = rev_database_function(db))$username,
        password = gen_auth_get(database = rev_database_function(db))$password,
        selection = code,
        type = "Neue Tabellen",
        date = date,
        ...
      )

      results_raw <- do.call(db, par_list)

      results_json <- test_if_json(results_raw)

      test_if_error_light(results_json)

    }

    #---------------------------------------------------------------------------
    if (type == "statistics") {

      par_list <-  list(
          endpoint = "catalogue/modifieddata",
          username = gen_auth_get(database = rev_database_function(db))$username,
          password = gen_auth_get(database = rev_database_function(db))$password,
          selection = code,
          type = "Neue Statistiken",
          date = date,
          ...
        )

      results_raw <- do.call(db, par_list)

      results_json <- test_if_json(results_raw)

      test_if_error_light(results_json)
    }

    #---------------------------------------------------------------------------
    if (type == "statisticsUpdates") {

      if(db == "gen_api" | db == "gen_api_regio"){

        par_list <-  list(
          endpoint = "catalogue/modifieddata",
          username = gen_auth_get(database = rev_database_function(db))$username,
          password = gen_auth_get(database = rev_database_function(db))$password,
          selection = code,
          type = "Aktualisierte Statistiken",
          date = date,
          ...
        )

      }

      results_raw <- do.call(db, par_list)

      results_json <- test_if_json(results_raw)

      test_if_error_light(results_json)
    }

    #---------------------------------------------------------------------------
    if (type == "all") {

      par_list <-  list(
        endpoint = "catalogue/modifieddata",
        username = gen_auth_get(database = rev_database_function(db))$username,
        password = gen_auth_get(database = rev_database_function(db))$password,
        selection = code,
        type = "all",
        date = date,
        ...
      )

      results_raw <- do.call(db, par_list)

      results_json <- test_if_json(results_raw)

      test_if_error_light(results_json)

    }


    if (is.null(unlist(results_json$List))) {

      message(paste0("No modified objects found for your code and date in ", rev_database_function(db)))

      return(NULL)

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
      attr(list_resp, "Database") <- rev_database_function(db)
      attr(list_resp, "Type") <- results_json$Parameter$type
      attr(list_resp, "Date") <- results_json$Parameter$date
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyright") <- results_json$Copyright

      return(list_resp)

  }
  })

  #-----------------------------------------------------------------------------

  if(!is.null(unlist(res))){

    res <- check_results(res)

    return(res)

  }

}
