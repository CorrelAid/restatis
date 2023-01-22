#' get_variables_from_statistic
#'
#' @description Function to generate variables from statistics.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Genesis-Object. Only one code per iteration. "*"-Notations are possibly to be used as a placeholder.
#' @param detailed a logical. Indicator if function should return the detailed output of the iteration including all object-related information or only a shortened output including only code and object title. The default is detailed = FALSE.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the Genesis call itself. The default is "code".
#' @param ... Additional parameter of the Genesis call. These parameters are only affecting the Genesis call itself, no further processing.
#'
#' @return A list with all recalled elements from Genesis. Based on the detailed-parameter it contains more or less information, but always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find the variables of the statistic with the code "12411" with a detailed output
#' object <- get_variables_from_statistic(code = "12411", detailed = T)
#' }
#'
get_variables_from_statistic <- function(code = NULL,
                                         detailed = FALSE,
                                         sortcriterion = c("code", "content"),
                                         ...) {

  check_function_input(code = code,
                       detailed = detailed,
                       sortcriterion = sortcriterion)

  sortcriterion <- match.arg(sortcriterion)

  #-----------------------------------------------------------------------------

  # Processing ####
  results_raw <- gen_api("catalogue/variables2statistic",
                          username = gen_auth_get()$username,
                          password = gen_auth_get()$password,
                          name = code,
                          sortcriterion = sortcriterion,
                          ...)

  results_json <- test_if_json(results_raw)

  test_if_error(results_json)

  if (isTRUE(detailed)) {

    list_of_variables <- binding_lapply(results_json$List,
                            characteristics = c("Code",
                                                "Content",
                                                "Type",
                                                "Values",
                                                "Information"))

  } else {

    list_of_variables <- binding_lapply(results_json$List,
                                        characteristics = c("Code",
                                                            "Content"
                                                            ))

  }

  list_of_variables$Object_Type <- "Variable"

  # Summary ####
  list_resp <- list("Variables" = tibble::as_tibble(list_of_variables))

  attr(list_resp, "Code") <- results_json$Parameter$name
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyright") <- results_json$Copyright

  return(list_resp)

}

#-------------------------------------------------------------------------------

#' get_values_from_variables
#'
#' @description Function to extract the possible values from a variable from Genesis. Values for continuous variables are not extractable, so the function returns a warning message.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Genesis-Object. Only one code per iteration.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the Genesis call itself. The default is "code".
#' @param ... Additional parameter of the Genesis call. These parameters are only affecting the Genesis call itself, no further processing.
#'
#' @return A list with all recalled elements from Genesis. Always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find the values of the variable "DLAND"
#' object <- get_values_from_variables(code = "DLAND")
#' }
#'
get_values_from_variables <- function(code = NULL,
                                      sortcriterion = c("code", "content"),
                                      ...) {

  check_function_input(code = code,
                       sortcriterion = sortcriterion)

  sortcriterion <- match.arg(sortcriterion)

  #-----------------------------------------------------------------------------

  results_raw <- gen_api("catalogue/values2variable",
                          username = gen_auth_get()$username,
                          password = gen_auth_get()$password,
                          name = code,
                          sortcriterion = sortcriterion,
                          ...)

  results_json <- test_if_json(results_raw)

  test_if_error(results_json)

  list_of_variables <- binding_lapply(results_json$List,
                                      characteristics = c("Code",
                                                          "Content",
                                                          "Variables",
                                                          "Information"))


  if (nrow(list_of_variables) > 0) {

    list_of_variables$Object_Type <- "Value"

  }

  list_resp <- list("Values" = tibble::as_tibble(list_of_variables))

  attr(list_resp, "Name") <- results_json$Parameter$name
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyright") <- results_json$Copyright

  names(list_resp) <- paste("Values of", results_json$Parameter$name)

  return(list_resp)

}

#-------------------------------------------------------------------------------

#' get_values_from_variables_from_statistic
#'
#' @description Get values from variables from statistic. Values for continuous variables cannot be extracted, so the function returns a warning message for each continuous variable whose values are attempted to be extracted.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Genesis-Object. Only one code per iteration. "*"-Notations are possibly to be used as a placeholder.
#' @param detailed a logical. Indicator if function should return the detailed output of the iteration including all object-related information or only a shortened output including only code and object title. This parameter only affects the details of the variables-related output. The default is FALSE.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is an parameter of the Genesis call itself. The default is "code".
#' @param ... Additional parameter of the Genesis call. These parameters are only affecting the Genesis call itself for the variables, no further processing or the values-related objects.
#'
#' @return A list with all recalled elements from Genesis. Based on the detailed-parameter it contains more or less information, but always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find the values of variables in a specific statistic with the code "21111" and a detailed description of the variables
#' object <- get_values_from_variables_from_statistic(code = "21111", detailed = TRUE)
#' }
#'
get_values_from_variables_from_statistic <- function(code = NULL,
                                                     detailed = FALSE,
                                                     sortcriterion = c("code", "content"),
                                                     ...) {

  check_function_input(code = code,
                       detailed = detailed,
                       sortcriterion = sortcriterion)

  sortcriterion <- match.arg(sortcriterion)

  #-----------------------------------------------------------------------------

  variables <- get_variables_from_statistic(code = code,
                                            detailed = detailed,
                                            sortcriterion = sortcriterion,
                                            ...)

  list_values <- list()

  lapply(variables$Variables$Code, function(x) {

    zwisch <- get_values_from_variables(code = x, sortcriterion = sortcriterion)
    list_values <<- append(list_values, zwisch)

  })

  list_resp <- list(variables, list_values)

  return(list_resp)

}

#-------------------------------------------------------------------------------

#' search_variables
#'
#' @description Function to search for specific variables in Genesis databank.
#'
#' @param code a string with a maximum length of 6. Code from a Genesis-Object. Only one code per iteration. "*"-Notations are possibly to be used as a placeholder.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the Genesis call itself. The default is "code".
#' @param ... Additional parameter of the Genesis call. These parameters are only affecting the Genesis call itself, no further processing.
#'
#' @return A list with all recalled elements from Genesis. Always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find a specific variable "GES" in Genesis
#' object <- search_variables("GES")
#' }
#'
search_variables <- function(code = NULL,
                             sortcriterion = c("code", "content"),
                             ...) {

  check_function_input(code = code,
                       sortcriterion = sortcriterion)

  sortcriterion <- match.arg(sortcriterion)

  #-----------------------------------------------------------------------------

  results_raw <- gen_api("catalogue/variables",
                          username = gen_auth_get()$username,
                          password = gen_auth_get()$password,
                          selection = code,
                          sortcriterion = sortcriterion,
                          ...)

  results_json <- test_if_json(results_raw)

  test_if_error(results_json)

  list_of_variables <- binding_lapply(results_json$List,
                                      characteristics = c("Code",
                                                          "Content",
                                                          "Type",
                                                          "Information"))

  list_of_variables$Object_Type <- "Variable"

  list_resp <- list("Variables" = tibble::as_tibble(list_of_variables))

  attr(list_resp, "Code") <- results_json$Parameter$selection
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyright") <- results_json$Copyright

  return(list_resp)

}
