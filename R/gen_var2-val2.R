#' Search for the Variables From a Statistic
#'
#' @description Function to generate variables from statistics in GENESIS.
#'
#' @param code a string with a maximum length of 15 characters. Code from a GENESIS-Object. Only one code per iteration. "*"-Notations are possibly to be used as a placeholder.
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
#' # Find the variables of the statistic with the code "12411"
#' # with a detailed output
#' object <- gen_var2stat(code = "12411", detailed = T)
#' }
#'
gen_var2stat <- function(code = NULL,
                         detailed = FALSE,
                         sortcriterion = c("code", "content"),
                         error.ignore = FALSE,
                         ...) {
  check_function_input(
    code = code,
    detailed = detailed,
    error.ignore = error.ignore,
    sortcriterion = sortcriterion
  )

  sortcriterion <- match.arg(sortcriterion)

  #-----------------------------------------------------------------------------

  # Processing ####
  results_raw <- gen_api("catalogue/variables2statistic",
    name = code,
    sortcriterion = sortcriterion,
    ...
  )

  results_json <- test_if_json(results_raw)

  empty_object <- test_if_error(results_json, para = error.ignore)

  if (isTRUE(empty_object)) {
    list_of_variables <- "No `variables`- object found for your request."
  } else if (isFALSE(empty_object)) {
    list_of_variables <- results_json$Status$Content
  } else if (empty_object == "DONE") {
    if (isTRUE(detailed)) {
      list_of_variables <- binding_lapply(results_json$List,
        characteristics = c(
          "Code",
          "Content",
          "Type",
          "Values",
          "Information"
        )
      )
    } else {
      list_of_variables <- binding_lapply(results_json$List,
        characteristics = c(
          "Code",
          "Content"
        )
      )
    }

    list_of_variables$Object_Type <- "Variable"

    list_of_variables <- tibble::as_tibble(list_of_variables)
  }

  # Summary ####
  list_resp <- list("Variables" = list_of_variables)

  attr(list_resp, "Code") <- results_json$Parameter$name
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyright") <- results_json$Copyright

  return(list_resp)
}

#-------------------------------------------------------------------------------

#' Search for the Values of a Variable
#'
#' @description Function to extract the possible values from a variable from GENESIS. Values for continuous variables are not extractable, so the function returns a warning message.
#'
#' @param code a string with a maximum length of 15 characters. Code from a GENESIS-Object. Only one code per iteration.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the GENESIS API call itself. The default is "code".
#' @param error.ignore  a logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response.
#' @param ... Additional parameter of the GENESIS API call. These parameters are only affecting the GENESIS API call itself, no further processing.
#'
#' @return A list with all recalled elements from GENESIS. Always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find the values of the variable "DLAND"
#' object <- gen_val2var(code = "DLAND")
#' }
#'
gen_val2var <- function(code = NULL,
                        sortcriterion = c("code", "content"),
                        error.ignore = FALSE,
                        ...) {
  check_function_input(
    code = code,
    error.ignore = error.ignore,
    sortcriterion = sortcriterion
  )

  sortcriterion <- match.arg(sortcriterion)

  #-----------------------------------------------------------------------------

  results_raw <- gen_api("catalogue/values2variable",
    name = code,
    sortcriterion = sortcriterion,
    ...
  )

  results_json <- test_if_json(results_raw)

  empty_object <- test_if_error(results_json, para = error.ignore)

  if (isTRUE(empty_object)) {
    list_of_variables <- "No `values`- object found for your request."
  } else if (isFALSE(empty_object)) {
    list_of_variables <- results_json$Status$Content
  } else if (empty_object == "DONE") {
    list_of_variables <- binding_lapply(results_json$List,
      characteristics = c(
        "Code",
        "Content",
        "Variables",
        "Information"
      )
    )

    list_of_variables$Object_Type <- "Value"

    list_of_variables <- tibble::as_tibble(list_of_variables)
  }

  list_resp <- list("Values" = list_of_variables)

  attr(list_resp, "Name") <- results_json$Parameter$name
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyright") <- results_json$Copyright

  names(list_resp) <- paste("Values of", results_json$Parameter$name)

  return(list_resp)
}

#-------------------------------------------------------------------------------

#' Search for the Variables and Their Values From a Statistic
#'
#' @description Function to extract the possible values from a variable from a statistic in GENESIS. Values for continuous variables are not extractable, so the function returns a warning message.
#'
#' @param code a string with a maximum length of 15 characters. Code from a GENESIS-Object. Only one code per iteration. "*"-Notations are possibly to be used as a placeholder.
#' @param detailed a logical. Indicator if function should return the detailed output of the iteration including all object-related information or only a shortened output including only code and object title. This parameter only affects the details of the variables-related output. The default is FALSE.
#' @param error.ignore  a logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is an parameter of the GENESIS API call itself. The default is "code".
#' @param ... Additional parameter of the GENESIS API call. These parameters are only affecting the GENESIS API call itself for the variables, no further processing for the values-related objects.
#'
#' @return A list with all recalled elements from GENESIS. Based on the detailed-parameter it contains more or less information, but always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find the values of variables in a specific statistic with
#' # the code "21111" and a detailed description of the variables
#' object <- gen_val2var2stat(code = "21111", detailed = TRUE)
#' }
#'
gen_val2var2stat <- function(code = NULL,
                             detailed = FALSE,
                             sortcriterion = c("code", "content"),
                             error.ignore = FALSE,
                             ...) {
  check_function_input(
    code = code,
    detailed = detailed,
    error.ignore = error.ignore,
    sortcriterion = sortcriterion
  )

  sortcriterion <- match.arg(sortcriterion)

  #-----------------------------------------------------------------------------

  variables <- suppressMessages(suppressWarnings(gen_var2stat(
    code = code,
    detailed = detailed,
    sortcriterion = sortcriterion,
    error.ignore = error.ignore,
    ...
  )))

  list_values <- list()

  lapply(variables$Variables$Code, function(x) {
    zwisch <- suppressMessages(suppressWarnings(gen_val2var(
      code = x,
      sortcriterion = sortcriterion,
      error.ignore = error.ignore
    )))
    list_values <<- append(list_values, zwisch)
  })

  list_resp <- list(variables, list_values)

  return(list_resp)
}

#-------------------------------------------------------------------------------

#' Search for a Specific Variable
#'
#' @description Function to search for specific variables in GENESIS.
#'
#' @param code a string with a maximum length of 6. Code from a GENESIS-Object. Only one code per iteration. "*"-Notations are possibly to be used as a placeholder.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the GENESIS API call itself. The default is "code".
#' @param error.ignore  a logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response.
#' @param ... Additional parameter of the GENESIS API call. These parameters are only affecting the GENESIS API call itself, no further processing.
#'
#' @return A list with all recalled elements from GENESIS. Always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find a specific variable "GES" in GENESIS
#' object <- gen_search_vars("GES")
#' }
#'
gen_search_vars <- function(code = NULL,
                            sortcriterion = c("code", "content"),
                            error.ignore = FALSE,
                            ...) {
  caller <- as.character(match.call()[1])

  check_function_input(
    code = code,
    error.ignore = error.ignore,
    sortcriterion = sortcriterion,
    caller = caller
  )

  sortcriterion <- match.arg(sortcriterion)

  #-----------------------------------------------------------------------------

  results_raw <- gen_api("catalogue/variables",
    selection = code,
    sortcriterion = sortcriterion,
    ...
  )

  results_json <- test_if_json(results_raw)

  empty_object <- test_if_error(results_json, para = error.ignore)

  if (isTRUE(empty_object)) {
    list_of_variables <- "No `variables`- object found for your request."
  } else if (isFALSE(empty_object)) {
    list_of_variables <- results_json$Status$Content
  } else if (empty_object == "DONE") {
    list_of_variables <- binding_lapply(results_json$List,
      characteristics = c(
        "Code",
        "Content",
        "Type",
        "Information"
      )
    )

    list_of_variables$Object_Type <- "Variable"

    list_of_variables <- tibble::as_tibble(list_of_variables)
  }

  list_resp <- list("Variables" = list_of_variables)

  attr(list_resp, "Code") <- results_json$Parameter$selection
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyright") <- results_json$Copyright

  return(list_resp)
}
