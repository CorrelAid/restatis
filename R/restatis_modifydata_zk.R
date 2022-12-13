#' Call For Modified Data
#'
#' Function to check for updates or new objects in Destatis based on a specific date.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Destatis-Object. Only one code per iteration. "*"-Notations are possible.
#' @param type a string. Specific Destatis-Object-type: 'tables', 'statistics', and 'statisticsUpdates'. All three can be accessed through "all" which is the default.
#' @param date a string. Specific date which is used as the last update or upload time point in Destatis to include a Destatis-Object in the return. Default option is 'now' which uses the current date of your R-Studio System. Alternative options are 'week_before' using the current date of your R-Studio System minus 7 days, 'month_before' using the current date of your R-Studio System minus 4 weeks, and "year_before" using the current date of your R-Studio System minus 52 weeks. Additionally, it is possible to fill in a self-chosen specific date which needs to have the form 'DD.MM.YYYY'.
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself, no further processing.
#'
#' @return A list with all recalled elements from Destatis. Always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing with the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find Destatis-Objects which were modified last_week
#' object <- modified_data(date = "week_before")
#'
#' # Find tables that were new from 31.03.2020
#' object <- modified_data(type = "tables", date = "31.03.2020")
#'
#' # Find Destatis-Objects related to the topic "Bevölkerung" (Code: '12*') which were new today
#' object <- modified_data(code = "12*)
#' }
modified_data <- function(code = "",
                          type = c("all", "tables", "statistics", "statisticsUpdates"),
                          date = c("now", "week_before", "month_before", "year_before"),
                          ...) {
  # Checks ####
  if (!(is.character(code)) && length(code) < 1L && is.null(code)) {
    stop("code must be a single string or NULL", call. = FALSE)
  }

  if (!all(type %in% c("all", "tables", "statistics", "statisticsUpdates"))) {
    stop("Available categories are tables, statistics, statistic updates, and all.", call. = FALSE)
  }

  if (all(date %in% c("now", "week_before", "month_before", "year_before"))) {
    message("Please note that this date is calculated automatically and may differ
            from manually entered data. Manually entered data must have
            the format DD.MM.YYYY.")
  }

  if ("now" %in% date) {
    date <- format(Sys.Date(), format = "%d.%m.%Y")
  } else if (date == "week_before") {
    today <- Sys.Date()
    date <- today - 7
    date <- format(date, format = "%d.%m.%Y")
  } else if (date == "month_before") {
    today <- Sys.Date()
    date <- today - as.difftime(4, units = "weeks")
    date <- format(date, format = "%d.%m.%Y")
  } else if (date == "year_before") {
    today <- Sys.Date()
    date <- today - as.difftime(52, units = "weeks")
    date <- format(date, format = "%d.%m.%Y")
  }

  type <- match.arg(type)

  # Processing ####
  if (type == "tables") {
    results_raw <- gen_api("catalogue/modifieddata",
      username = gen_auth_get()$username,
      password = gen_auth_get()$password,
      selection = code,
      type = "Neue Tabellen",
      date = date,
      ...
    )

    if (httr2::resp_content_type(results_raw) == "application/json") {
      results_json <- httr2::resp_body_json(results_raw)
    }

    if (results_json$Status$Code != 0) {
      message(results_json$Status$Content)
    }
  }

  if (type == "statistics") {
    results_raw <- gen_api("catalogue/modifieddata",
      username = gen_auth_get()$username,
      password = gen_auth_get()$password,
      selection = code,
      type = "Neue Statistiken",
      date = date,
      ...
    )

    if (httr2::resp_content_type(results_raw) == "application/json") {
      results_json <- httr2::resp_body_json(results_raw)
    }

    if (results_json$Status$Code != 0) {
      message(results_json$Status$Content)
    }
  }

  if (type == "statisticsUpdates") {
    results_raw <- gen_api("catalogue/modifieddata",
      username = gen_auth_get()$username,
      password = gen_auth_get()$password,
      selection = code,
      type = "Aktualisierte Statistiken",
      date = date,
      ...
    )

    if (httr2::resp_content_type(results_raw) == "application/json") {
      results_json <- httr2::resp_body_json(results_raw)
    }

    if (results_json$Status$Code != 0) {
      message(results_json$Status$Content)
    }
  }

  if (type == "all") {
    results_raw <- gen_api("catalogue/modifieddata",
      username = gen_auth_get()$username,
      password = gen_auth_get()$password,
      selection = code,
      type = "all",
      date = date,
      ...
    )

    if (httr2::resp_content_type(results_raw) == "application/json") {
      results_json <- httr2::resp_body_json(results_raw)
    }

    if (results_json$Status$Code != 0) {
      message(results_json$Status$Content)
    }
  }

  if (length(results_json$List) == 0 | is.null(unlist(results_json$List))) {
    message("No modified objects found for your code or date.")
  } else {
    table <- data.frame()

    lapply(results_json$List, function(x) {
      zwisch <- rbind(c(
        "Code" = x$Code, "Content" = x$Content,
        "Date" = x$Date, "Added" = x$Added,
        "Type" = x$Type
      ))
      table <- rbind(table, zwisch)
    })
    table$Date <- as.Date.character(table$Date, format = "%d.%m.%Y")

    table <- tibble::as_tibble(table)
    table <- table[order(table$Date, decreasing = T), ]

    list_resp <- list("Modified" = table)
    attr(list_resp, "Code") <- results_json$Parameter$selection
    attr(list_resp, "Type") <- results_json$Parameter$type
    attr(list_resp, "Date") <- results_json$Parameter$date
    attr(list_resp, "Language") <- results_json$Parameter$language
    attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
    attr(list_resp, "Copyrigtht") <- results_json$Copyright

    return(list_resp)
  }
}
