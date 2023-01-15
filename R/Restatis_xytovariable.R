#' xy_to_variable
#'
#' @description Function to find objects related to a variable in Destatis.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Destatis-Object. Only one code per iteration.
#' @param category a string. Specific Destatis-Object-types: 'tables', 'statistics', and 'cubes'. All three together are possible and the default option.
#' @param detailed a logical. Indicator if function should return the detailed output of the iteration including all object-related information or only a shortened output including only code and object title. The default is detailed = FALSE.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the Destatis call itself. The default is "code".
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself, no further processing.
#'
#' @return A list with all recalled elements from Destatis. Based on the detailed-parameter it contains more or less information, but always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find Tables for Variable "Kreise" and return detailed output
#' object <- xy_to_variable(code = "Kreise", category = "tables", detailed = T)
#'
#' #' # Find everything for Variable "GES"
#' object <- xy_to_variable(code = "GES")
#' # Default of detailed-parameter is FALSE, and default of the category-parameter is to include all object types.
#' }
#'
xy_to_variable <- function(code = NULL,
                           category = c("tables", "statistics", "cubes"),
                           detailed = FALSE,
                           sortcriterion = c("code", "content"),
                           ...) {

  caller <- as.character(match.call())

  check_function_input(code = code,
                       category = category,
                       detailed = detailed,
                       sortcriterion = sortcriterion)

  sortcriterion <- match.arg(sortcriterion)

  #-----------------------------------------------------------------------------

  if ("tables" %in% category) {

    results_raw <- gen_api("catalogue/tables2variable",
                            username = gen_auth_get()$username,
                            password = gen_auth_get()$password,
                            name = code,
                            sortcriterion = sortcriterion,
                            ...)

    if (httr2::resp_content_type(results_raw) == "application/json") {

      results_json <- httr2::resp_body_json(results_raw)

    }

    if (results_json$Status$Code != 0) {

      stop(results_json$Status$Content, call. = FALSE)

    }

    df_tables <- data.frame()

    if (isTRUE(detailed)) {

      lapply(results_json$List, function(x) {

        zwisch <- rbind(c(
          "Code" = x$Code,
          "Content" = x$Content,
          "Time" = x$Time
        ))

        df_tables <<- rbind(df_tables, zwisch)

      })

      df_tables$Object_Type <- "Table"

    } else {

      lapply(results_json$List, function(x) {

        zwisch <- rbind(c("Code" = x$Code,
                          "Content" = x$Content))

        df_tables <<- rbind(df_tables, zwisch)

      })

      df_tables$Object_Type <- "Table"

    }
  }

  #-----------------------------------------------------------------------------

  if ("statistics" %in% category) {

    results_raw <- gen_api("catalogue/statistics2variable",
                            username = gen_auth_get()$username,
                            password = gen_auth_get()$password,
                            name = code,
                            sortcriterion = sortcriterion,
                            ...)

    if (httr2::resp_content_type(results_raw) == "application/json") {

      results_json <- httr2::resp_body_json(results_raw)

    }

    if (results_json$Status$Code != 0) {

      stop(results_json$Status$Content, call. = FALSE)

    }

    df_statistics <- data.frame()

    if (isTRUE(detailed)) {

      lapply(results_json$List, function(x) {

        zwisch <- rbind(c(
          "Code" = x$Code,
          "Content" = x$Content,
          "Cubes" = x$Cubes,
          "Information" = x$Information
        ))

        df_statistics <<- rbind(df_statistics, zwisch)

      })

      df_statistics$Object_Type <- "Statistic"

    } else {

      lapply(results_json$List, function(x) {

        zwisch <- rbind(c("Code" = x$Code,
                          "Content" = x$Content))

        df_statistics <<- rbind(df_statistics, zwisch)

      })

      df_statistics$Object_Type <- "Statistic"
    }
  }

  #-----------------------------------------------------------------------------

  if ("cubes" %in% category) {

    results_raw <- gen_api("catalogue/timeseries2variable",
                            username = gen_auth_get()$username,
                            password = gen_auth_get()$password,
                            name = code,
                            ...)

    if (httr2::resp_content_type(results_raw) == "application/json") {

      results_json <- httr2::resp_body_json(results_raw)

    }

    if (results_json$Status$Code != 0) {

      stop(results_json$Status$Content, call. = FALSE)

    }

    df_cubes <- data.frame()

    if (isTRUE(detailed)) {

      lapply(results_json$List, function(x) {

        zwisch <- rbind(c(
          "Code" = x$Code,
          "Content" = x$Content,
          "Time" = x$Time,
          "State" = x$State,
          "LatestUpdate" = x$LatestUpdate,
          "Information" = x$Information
        ))

        df_cubes <<- rbind(df_cubes, zwisch)

      })

      df_cubes$Object_Type <- "Cube"

    } else {

      lapply(results_json$List, function(x) {

        zwisch <- rbind(c("Code" = x$Code,
                          "Content" = x$Content))

        df_cubes <<- rbind(df_cubes, zwisch)

      })

      df_cubes$Object_Type <- "Cube"
    }
  }

  #-----------------------------------------------------------------------------

  # Summary ####
  if (all(c("tables", "statistics", "cubes") %in% category)) {

    list_resp <- list(
      "Tables" = tibble::as_tibble(df_tables),
      "Statistics" = tibble::as_tibble(df_statistics),
      "Cubes" = tibble::as_tibble(df_cubes)
    )

  } else if (category == "tables") {

    list_resp <- tibble::as_tibble(df_tables)

  } else if (category == "statistics") {

    list_resp <- tibble::as_tibble(df_statistics)

  } else if (category == "cubes") {

    list_resp <- tibble::as_tibble(df_cubes)

  }

  attr(list_resp, "Code") <- results_json$Parameter$term
  attr(list_resp, "Category") <- category
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <- results_json$Copyright

  return(list_resp)

}
