#' Search for Newly Added Objects or Documented Changes to Objects in GENESIS
#'
#' @description Function to check for updates, changes, or new objects in GENESIS based on a specific date.
#'
#' @param code a string with a maximum length of 15 characters. Code from a GENESIS object. Only one code per iteration. "*" notations are possible. Empty code (default value) includes all changes, updates, and new added objects.
#' @param type a string. Specific GENESIS object type: 'tables', 'statistics', and 'statisticsUpdates'. All three can be accessed through "all", which is the default.
#' @param date a string. Specific date that is used as the last update or upload time in GENESIS to include a GENESIS object in return. Default option is 'now', which uses the current date of your system. Alternative options are 'week_before', using the current date of your system minus 7 days, 'month_before', using the current date of your system minus 4 weeks, and 'year_before', using the current date of your system minus 52 weeks. Additionally, it is possible to fill in a specific date of format 'DD.MM.YYYY'.
#' @param ... Additional parameter of the GENESIS API call. These parameters are only affecting the GENESIS API call itself, no further processing.
#'
#' @return A list with all recalled elements from GENESIS. Always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing with the data. Attributes are added to the data.frame describing the search configuration for the returned output.
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
                              type = c("all", "tables", "statistics", "statisticsUpdates"),
                              date = c("now", "week_before", "month_before", "year_before"),
                              ...) {
  date <- check_function_input(
    code = code,
    type = type,
    date = date
  )

  #-----------------------------------------------------------------------------

  if (date == "now") {
    date <- format(Sys.Date(), format = "%d.%m.%Y")
  } else if (date == "week_before") {
    date <- format(Sys.Date() - 7, format = "%d.%m.%Y")
  } else if (date == "month_before") {
    date <- format(Sys.Date() - as.difftime(4, units = "weeks"),
      format = "%d.%m.%Y"
    )
  } else if (date == "year_before") {
    date <- format(as.difftime(52, units = "weeks"), format = "%d.%m.%Y")
  }

  type <- match.arg(type)

  #-----------------------------------------------------------------------------

  # Processing ####
  if (type == "tables") {
    results_raw <- gen_api("catalogue/modifieddata",
      selection = code,
      type = "Neue Tabellen",
      date = date,
      ...
    )

    results_json <- test_if_json(results_raw)

    test_if_error_light(results_json)
  }

  #-----------------------------------------------------------------------------

  if (type == "statistics") {
    results_raw <- gen_api("catalogue/modifieddata",
      selection = code,
      type = "Neue Statistiken",
      date = date,
      ...
    )

    results_json <- test_if_json(results_raw)

    test_if_error_light(results_json)
  }

  #-----------------------------------------------------------------------------

  if (type == "statisticsUpdates") {
    results_raw <- gen_api("catalogue/modifieddata",
      selection = code,
      type = "Aktualisierte Statistiken",
      date = date,
      ...
    )

    results_json <- test_if_json(results_raw)

    test_if_error_light(results_json)
  }

  #-----------------------------------------------------------------------------

  if (type == "all") {
    results_raw <- gen_api("catalogue/modifieddata",
      selection = code,
      type = "all",
      date = date,
      ...
    )

    results_json <- test_if_json(results_raw)

    test_if_error_light(results_json)
  }

  #-----------------------------------------------------------------------------

  if (is.null(unlist(results_json$List))) {
    message("No modified objects found for your code and date.")
  } else {
    table <- binding_lapply(results_json$List,
      characteristics = c(
        "Code",
        "Content",
        "Date",
        "Added",
        "Type"
      )
    )


    table$Date <- as.Date.character(table$Date, format = "%d.%m.%Y")

    table <- tibble::as_tibble(table)
    table <- table[order(table$Date, decreasing = TRUE), ]

    list_resp <- list("Modified" = table)

    attr(list_resp, "Code") <- results_json$Parameter$selection
    attr(list_resp, "Type") <- results_json$Parameter$type
    attr(list_resp, "Date") <- results_json$Parameter$date
    attr(list_resp, "Language") <- results_json$Parameter$language
    attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
    attr(list_resp, "Copyright") <- results_json$Copyright

    return(list_resp)
  }
}
