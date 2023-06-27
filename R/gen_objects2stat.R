#' Search for Objects Related to a Statistic
#'
#' @description Function to find objects related to a statistic in GENESIS.
#'
#' @param code a string with a maximum length of 6 characters (15 characters if cubes are not used as a category). Code from a GENESIS-Object. Only one code per iteration.
#' @param category a string. Specific object-types: 'tables', 'variables', and 'cubes'. All three together are possible and the default option.
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
#' # Find cubes from the statistic with the code "21111" with a detailed return
#' object <- gen_objects2stat(code = "21111", category = "cubes", detailed = T)
#'
#' # Find all object types from the statistic with the code "12411"
#' object <- gen_objects2stat(code = "12411")
#' # Default of detailed-parameter is FALSE, and default of the
#' # category-parameter is to include all object types.
#' }
#'
gen_objects2stat <- function(code = NULL,
                             category = c("tables", "variables", "cubes"),
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

  #-------------------------------------------------------------------------------

  if ("tables" %in% category) {
    results_raw <- gen_api("catalogue/tables2statistic",
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

  if ("variables" %in% category) {
    results_raw <- gen_api("catalogue/variables2statistic",
      name = code,
      sortcriterion = sortcriterion,
      ...
    )

    results_json <- test_if_json(results_raw)

    empty_object <- test_if_error(results_json, para = error.ignore)

    if (isTRUE(empty_object)) {
      df_variables <- "No 'variables' object found for your request."
    } else if (isFALSE(empty_object)) {
      df_variables <- results_json$Status$Content
    } else if (empty_object == "DONE") {
      if (detailed == TRUE) {
        df_variables <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content",
            "Type",
            "Values",
            "Information"
          )
        )
      } else {
        df_variables <- binding_lapply(results_json$List,
          characteristics = c(
            "Code",
            "Content"
          )
        )
      }

      df_variables$Object_Type <- "Variable"

      df_variables <- tibble::as_tibble(df_variables)
    }
  }

  #-----------------------------------------------------------------------------

  if ("cubes" %in% category) {
    results_raw <- gen_api("catalogue/cubes2statistic",
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
  if (all(c("tables", "variables", "cubes") %in% category)) {
    list_resp <- list(
      "Tables" = df_tables,
      "Variables" = df_variables,
      "Cubes" = df_cubes
    )
  } else if (category == "tables") {
    list_resp <- df_tables
  } else if (category == "variables") {
    list_resp <- df_variables
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
