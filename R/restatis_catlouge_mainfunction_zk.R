# EVAS codes loaded - Used as the structure for the
load("data/evas_list_long_20220724.RData")

#' Search Destatis Catalogue With According Structural Representation
#'
#' Function to enable searching for tables, statistics, and cubes from Destatis. Additionally, it structures the output based on the internal tree structure of Destatis itself based on the EVAS-numbers.
#'
#' @param code a string with a maximum length of 10 characters. Code from a Destatis-Object. Only one code per iteration. "*"-Notations are possible.
#' @param category a string. Specific Destatis-Object-types: 'tables', 'statistics', and 'cubes'. All three together are possible.
#' @param detailed a logical. Indicator if the function should return the detailed output of the iteration including all object-related information or only a shortened output including only code and object title.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the Destatis call itself.
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself, no further processing.
#'
#' @return A list with all recalled elements from Destatis. Based on the detailed-parameter it contains more or less information, but always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing with the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Scroll through Destatis-Objects under the topic "12*" which is "Bevölkerung" in Destatis from all categories and with a detailed output
#' object <- catalouge(code = "12*", detailed = T)
#'
#' # Search tables under the topic "12*" which is "Bevölkerung" in Destatis without a detailed output
#' object <- catalouge(code = "12*", category = "tables")
#' }
#'
catalogue <- function(code = NULL,
                      category = c("tables", "statistics", "cubes"),
                      detailed = F,
                      sortcriterion = c("code", "content"),
                      ...) {
  # Checks ####
  if (!(is.character(code)) && length(code) < 1L && is.null(code)) {
    stop("code must be a single string or NULL", call. = FALSE)
  }

  if (detailed == FALSE) {
    message("Use detailed = TRUE to obtain the complete output.")
  }

  if (!all(category %in% c("tables", "statistics", "cubes"))) {
    stop("Available categories are tables, statistics, or cubes.")
  }

  if (!(isTRUE(detailed) | isFALSE(detailed))) {
    stop("detailed-parameter must be a TRUE or FALSE", call. = FALSE)
  }

  sortcriterion <- match.arg(sortcriterion)


  # Processing ####
  if ("cubes" %in% category) {
    results_raw <- gen_api("catalogue/cubes",
      username = gen_auth_get()$username, password = gen_auth_get()$password,
      selection = code, ...
    )

    if (httr2::resp_content_type(results_raw) == "application/json") {
      results_json <- httr2::resp_body_json(results_raw)
    }

    if (results_json$Status$Code != 0) {
      message(results_json$Status$Content)
    }

    list_of.cubes <- data.frame()

    if (detailed) {
      lapply(results_json$List, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content,
                          "Time" = x$Time, "Latest_Update" = x$LatestUpdate,
                          "State" = x$State, "Information" = x$Information))
        list_of.cubes <<- rbind(list_of.cubes, zwisch)
      })
      list_of.cubes$Object_Type <- "Cube"
    } else {
      lapply(results_json$List, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content))
        list_of.cubes <<- rbind(list_of.cubes, zwisch)
      })
      list_of.cubes$Object_Type <- "Cube"
    }

    list_of.cubes <- tibble::as_tibble(list_of.cubes)
  }

  if ("statistics" %in% category) {
    results_raw <- gen_api("catalogue/statistics",
      username = gen_auth_get()$username, password = gen_auth_get()$password,
      selection = code, sortcriterion = sortcriterion, ...
    )

    if (httr2::resp_content_type(results_raw) == "application/json") {
      results_json <- httr2::resp_body_json(results_raw)
    }

    if (results_json$Status$Code != 0) {
      message(results_json$Status$Content)
    }

    list_of.stats <- data.frame()

    if (detailed) {
      lapply(results_json$List, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content,
                          "Cubes" = x$Cubes, "Information" = x$Information))
        list_of.stats <<- rbind(list_of.stats, zwisch)
      })
      list_of.stats$Object_Type <- "Statistic"
    } else {
      lapply(results_json$List, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content))
        list_of.stats <<- rbind(list_of.stats, zwisch)
      })
      list_of.stats$Object_Type <- "Statistic"
    }

    list_of.stats <- tibble::as_tibble(list_of.stats)
  }

  if ("tables" %in% category) {
    results_raw <- gen_api("catalogue/tables",
      username = gen_auth_get()$username, password = gen_auth_get()$password,
      selection = code, sortcriterion = sortcriterion, ...
    )

    if (httr2::resp_content_type(results_raw) == "application/json") {
      results_json <- httr2::resp_body_json(results_raw)
    }

    if (results_json$Status$Code != 0) {
      message(results_json$Status$Content)
    }

    list_of.tabs <- data.frame()

    if (detailed) {
      lapply(results_json$List, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content,
                          "Time" = x$Time))
        list_of.tabs <<- rbind(list_of.tabs, zwisch)
      })
      list_of.tabs$Object_Type <- "Table"
    } else {
      lapply(results_json$List, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content))
        list_of.tabs <<- rbind(list_of.tabs, zwisch)
      })
      list_of.tabs$Object_Type <- "Table"
    }

    list_of.tabs <- tibble::as_tibble(list_of.tabs)
  }

  # Summary ####
  if (all(c("tables", "statistics", "cubes") %in% category)) {
    list_resp <- list(
      "Cubes" = list("A" = forming_evas(list_of.cubes)),
      "Statistics" = list("B" = forming_evas(list_of.stats)),
      "Tables" = list("Output" = forming_evas(list_of.tabs))
    )
  } else if (category == "cubes") {
    list_resp <- list("Output" = forming_evas(list_of.cubes))
  } else if (category == "statistics") {
    list_resp <- list("Output" = forming_evas(list_of.stats))
  } else if (category == "tables") {
    list_resp <- list("Output" = forming_evas(list_of.tabs))
  }

  attr(list_resp, "Code") <- results_json$Parameter$selection
  attr(list_resp, "Category") <- category
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <- results_json$Copyright

  return(list_resp)
}
