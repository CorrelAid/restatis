#' Explore Different Objects and Their Structural Embedding in GENESIS
#'
#' @description Function to enable searching for tables, statistics, and cubes from GENESIS. Additionally, it structures the output based on the internal tree structure of GENESIS itself based on the EVAS-numbers. Time-series are represented as cubes with a specified time span.
#'
#' @param code a string with a maximum length of 10 characters. Code from a GENESIS-Object. Only one code per iteration. "*"-Notations are possible.
#' @param category a string. Specific GENESIS-Object-types: 'tables', 'statistics', and 'cubes'. All three together are possible.
#' @param detailed a logical. Indicator if the function should return the detailed output of the iteration including all object-related information or only a shortened output including only code and object title.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the GENESIS API call itself.
#' @param error.ignore  a logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response.
#' @param ... Additional parameter of the GENESIS API call. These parameters are only affecting the GENESIS API call itself, no further processing.
#'
#' @return A list with all recalled elements from GENESIS API. Based on the detailed-parameter it contains more or less information, but always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing with the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Scroll through Objects under the topic "12*"
#' # which is "Bevölkerung" in Destatis from all categories and
#' # with a detailed output
#' object <- gen_catalogue(code = "12*", detailed = T)
#'
#' # Search tables under the topic "12*" which is "Bevölkerung"
#' # without a detailed output
#' object <- gen_catalogue(code = "12*", category = "tables")
#' }
#'
gen_catalogue <- function(code = NULL,
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
    sortcriterion = sortcriterion,
    error.ignore = error.ignore,
    caller = caller
  )

  sortcriterion <- match.arg(sortcriterion)

  #-----------------------------------------------------------------------------

  # Processing ####
  if ("cubes" %in% category) {
    results_raw <- gen_api("catalogue/cubes",
      selection = code,
      ...
    )

    results_json <- test_if_json(results_raw)

    empty_object <- test_if_error(results_json, para = error.ignore)

    if (isTRUE(empty_object)) {
      list_of_cubes <- "No 'cubes' object found for your request."
    } else if (isFALSE(empty_object)) {
      list_of_cubes <- results_json$Status$Content
    } else if (empty_object == "DONE") {
      if (isTRUE(detailed)) {
        list_of_cubes <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content",
            "Time",
            "LatestUpdate",
            "State",
            "Information"
          )
        )
      } else {
        list_of_cubes <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content"
          )
        )
      }

      list_of_cubes$Object_Type <- "Cube"

      list_of_cubes <- tibble::as_tibble(list_of_cubes)
    }
  }


  #-----------------------------------------------------------------------------

  if ("statistics" %in% category) {
    results_raw <- gen_api("catalogue/statistics",
      selection = code,
      sortcriterion = sortcriterion,
      ...
    )

    results_json <- test_if_json(results_raw)

    empty_object <- test_if_error(results_json, para = error.ignore)

    if (isTRUE(empty_object)) {
      list_of.stats <- "No 'statistics' object found for your request."
    } else if (isFALSE(empty_object)) {
      list_of.stats <- results_json$Status$Content
    } else if (empty_object == "DONE") {
      if (isTRUE(detailed)) {
        list_of.stats <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content",
            "Cubes",
            "Information"
          )
        )
      } else {
        list_of.stats <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content"
          )
        )
      }

      list_of.stats$Object_Type <- "Statistic"

      list_of.stats <- tibble::as_tibble(list_of.stats)
    }
  }



  #-----------------------------------------------------------------------------

  if ("tables" %in% category) {
    results_raw <- gen_api("catalogue/tables",
      selection = code,
      sortcriterion = sortcriterion,
      ...
    )

    results_json <- test_if_json(results_raw)

    empty_object <- test_if_error(results_json, para = error.ignore)

    if (isTRUE(empty_object)) {
      list_of.tabs <- "No 'tables' object found for your request."
    } else if (isFALSE(empty_object)) {
      list_of.tabs <- results_json$Status$Content
    } else if (empty_object == "DONE") {
      if (isTRUE(detailed)) {
        list_of.tabs <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content",
            "Time"
          )
        )
      } else {
        list_of.tabs <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content"
          )
        )
      }

      list_of.tabs$Object_Type <- "Table"

      list_of.tabs <- tibble::as_tibble(list_of.tabs)
    }
  }


  #-----------------------------------------------------------------------------

  # Summary ####
  if (all(c("tables", "statistics", "cubes") %in% category)) {
    list_resp <- list(
      "Cubes" = if (length(list_of_cubes) == 1) {
        list_of_cubes
      } else {
        list("A" = forming_evas(list_of_cubes))
      },
      "Statistics" = if (length(list_of.stats) == 1) {
        list_of.stats
      } else {
        list("B" = forming_evas(list_of.stats))
      },
      "Tables" = if (length(list_of.tabs) == 1) {
        list_of.tabs
      } else {
        list("C" = forming_evas(list_of.tabs))
      }
    )
  } else if (category == "cubes") {
    if (length(list_of_cubes) == 1) {
      list_resp <- list("Output" = list_of_cubes)
    } else {
      list_resp <- list("Output" = forming_evas(list_of_cubes))
    }
  } else if (category == "statistics") {
    if (length(list_of.stats) == 1) {
      list_resp <- list("Output" = list_of.stats)
    } else {
      list_resp <- list("Output" = forming_evas(list_of.stats))
    }
  } else if (category == "tables") {
    if (length(list_of.tabs) == 1) {
      list_resp <- list("Output" = list_of.tabs)
    } else {
      list_resp <- list("Output" = forming_evas(list_of.tabs))
    }
  }

  attr(list_resp, "Code") <- results_json$Parameter$selection
  attr(list_resp, "Category") <- category
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyright") <- results_json$Copyright

  return(list_resp)
}
