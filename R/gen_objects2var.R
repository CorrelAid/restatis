#' Search for Objects Related to a Variable
#'
#' @description Function to find objects related to a variable in GENESIS.
#'
#' @param code a string with a maximum length of 15 characters. Code from a GENESIS-Object. Only one code per iteration.
#' @param category a string. Specific object-types: 'tables', 'statistics', and 'cubes'. All three together are possible and the default option.
#' @param detailed a logical. Indicator if function should return the detailed output of the iteration including all object-related information or only a shortened output including only code and object title. The default is detailed = FALSE.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the GENESIS API call itself. The default is "code".
#' @param error.ignore  a logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response.
#' @param ... Additional parameter of the GENESIS API call. These parameters are only affecting the GENESIS API call itself, no further processing.
#'
#' @return A list with all recalled elements from GENESIS. Based on the detailed-parameter it contains more or less information, but always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find Tables for Variable "Kreise" and return detailed output
#' object <- gen_objects2var(code = "Kreise", category = "tables", detailed = T)
#'
#' # Find everything for Variable "GES"
#' object <- gen_objects2var(code = "GES")
#' # Default of detailed-parameter is FALSE, and default of the
#' # category-parameter is to include all object types.
#' }
#'
gen_objects2var <- function(code = NULL,
                            category = c("tables", "statistics", "cubes"),
                            detailed = FALSE,
                            sortcriterion = c("code", "content"),
                            error.ignore = FALSE,
                            ...) {
  caller <- as.character(match.call()[1])

  check_function_input(
    code = code,
    category = category,
    detailed = detailed,
    error.ignore = error.ignore,
    sortcriterion = sortcriterion,
    caller = caller
  )

  sortcriterion <- match.arg(sortcriterion)

  #-----------------------------------------------------------------------------

  if ("tables" %in% category) {
    results_raw <- gen_api("catalogue/tables2variable",
      name = code,
      sortcriterion = sortcriterion,
      ...
    )

    results_json <- test_if_json(results_raw)

    empty_object <- test_if_error(results_json, para = error.ignore)

    if (isTRUE(empty_object)) {
      df_tables <- "No 'tables' object found for your request."
    } else if (isFALSE(empty_object)) {
      df_tables <- results_json$Status$Content
    } else if (empty_object == "DONE") {
      if (isTRUE(detailed)) {
        df_tables <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content",
            "Time"
          )
        )
      } else {
        df_tables <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content"
          )
        )
      }

      df_tables$Object_Type <- "Table"

      df_tables <- tibble::as_tibble(df_tables)
    }
  }

  #-----------------------------------------------------------------------------

  if ("statistics" %in% category) {
    results_raw <- gen_api("catalogue/statistics2variable",
      name = code,
      sortcriterion = sortcriterion,
      ...
    )

    results_json <- test_if_json(results_raw)

    empty_object <- test_if_error(results_json, para = error.ignore)

    if (isTRUE(empty_object)) {
      df_statistics <- "No 'statistics' object found for your request."
    } else if (isFALSE(empty_object)) {
      df_statistics <- results_json$Status$Content
    } else if (empty_object == "DONE") {
      if (isTRUE(detailed)) {
        df_statistics <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content",
            "Cubes",
            "Information"
          )
        )
      } else {
        df_statistics <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content"
          )
        )
      }

      df_statistics$Object_Type <- "Statistic"

      df_statistics <- tibble::as_tibble(df_statistics)
    }
  }

  #-----------------------------------------------------------------------------

  if ("cubes" %in% category) {
    results_raw <- gen_api("catalogue/timeseries2variable",
      name = code,
      ...
    )

    results_json <- test_if_json(results_raw)

    empty_object <- test_if_error(results_json, para = error.ignore)

    if (isTRUE(empty_object)) {
      df_cubes <- "No 'cubes' object found for your request."
    } else if (isFALSE(empty_object)) {
      df_cubes <- results_json$Status$Content
    } else if (empty_object == "DONE") {
      if (isTRUE(detailed)) {
        df_cubes <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content",
            "Time",
            "State",
            "LatestUpdate",
            "Information"
          )
        )
      } else {
        df_cubes <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content"
          )
        )
      }

      df_cubes$Object_Type <- "Cube"

      df_cubes <- tibble::as_tibble(df_cubes)
    }
  }

  #-----------------------------------------------------------------------------

  # Summary ####
  if (all(c("tables", "statistics", "cubes") %in% category)) {
    list_resp <- list(
      "Tables" = df_tables,
      "Statistics" = df_statistics,
      "Cubes" = df_cubes
    )
  } else if (category == "tables") {
    list_resp <- df_tables
  } else if (category == "statistics") {
    list_resp <- df_statistics
  } else if (category == "cubes") {
    list_resp <- df_cubes
  }

  attr(list_resp, "Code") <- results_json$Parameter$term
  attr(list_resp, "Category") <- category
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyright") <- results_json$Copyright

  return(list_resp)
}
