#####
#' Get Variables From Statistics
#'
#' Function to generate variables from statistics.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Destatis-Object. Only one code per iteration. "*"-Notations are possibly to be used as a placeholder.
#' @param detailed a logical. Indicator if function should return the detailed output of the iteration including all object-related information or only a shortened output including only code and object title. The default is detailed = FALSE.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the Destatis call itself. The default is "code".
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself, no further processing.
#'
#' @return A list with all recalled elements from Destatis. Based on the detailed-parameter it contains more or less information, but always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find the variables of the statistic with the code "12411" with a detailed output
#' object <- get_variables_from_statistic(code = "12411", detailed = T)
#' }
#'
get_variables_from_statistic <- function(code = NULL,
                                         detailed = F,
                                         sortcriterion = c("code", "content"),
                                         ...) {
  # Check ####
  if (!(is.character(code)) && length(code) < 1L && is.null(code)) {
    stop("code must be a single string or NULL", call. = FALSE)
  }

  if (detailed == FALSE) {
    message("Use detailed = TRUE to obtain the complete output.")
  }

  if (!(isTRUE(detailed) | isFALSE(detailed))) {
    stop("detailed-parameter must be a TRUE or FALSE", call. = FALSE)
  }

  sortcriterion <- match.arg(sortcriterion)

  # Processing ####
  results_raw <- gen_api("catalogue/variables2statistic",
    username = gen_auth_get()$username,
    password = gen_auth_get()$password,
    name = code,
    sortcriterion = sortcriterion,
    ...
  )

  if (httr2::resp_content_type(results_raw) == "application/json") {
    results_json <- httr2::resp_body_json(results_raw)
  }

  if (results_json$Status$Code != 0) {
    message(results_json$Status$Content)
  }


  list_of_variables <- data.frame()
  if (detailed) {
    lapply(results_json$List, function(x) {
      zwisch <- rbind(c(
        "Code" = x$Code, "Content" = x$Content, "Type" = x$Type,
        "Values" = x$Values, "Information" = x$Information
      ))
      list_of_variables <<- rbind(list_of_variables, zwisch)
    })
  } else {
    lapply(results_json$List, function(x) {
      zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Type" = x$Type))
      list_of_variables <<- rbind(list_of_variables, zwisch)
    })
  }

  list_of_variables$Object_Type <- "Variable"

  # Summary ####
  list_resp <- list("Variables" = tibble::as_tibble(list_of_variables))
  attr(list_resp, "Code") <- results_json$Parameter$name
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <- results_json$Copyright

  return(list_resp)
}


#####
#' Get Values From Variables
#'
#' Function to extract the possible values from a variable from Destatis.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Destatis-Object. Only one code per iteration.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the Destatis call itself. The default is "code".
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself, no further processing.
#'
#' @return A list with all recalled elements from Destatis. Always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find the values of the variable "DLAND"
#' object <- get_values_from_variables(code = "DLAND")
#' }
get_values_from_variables <- function(code = NULL,
                                      sortcriterion = c("code", "content"),
                                      ...) {
  if (!(is.character(code)) && length(code) < 1L && is.null(code)) {
    stop("code must be a single string or NULL", call. = FALSE)
  }

  sortcriterion <- match.arg(sortcriterion)


  results_raw <- gen_api("catalogue/values2variable",
    username = gen_auth_get()$username,
    password = gen_auth_get()$password,
    name = code,
    sortcriterion = sortcriterion,
    ...
  )

  if (httr2::resp_content_type(results_raw) == "application/json") {
    results_json <- httr2::resp_body_json(results_raw)
  }

  if (results_json$Status$Code != 0) {
    message(results_json$Status$Content)
  }

  list_of_variables <- data.frame()

  lapply(results_json$List, function(x) {
    zwisch <- rbind(c(
      "Code" = x$Code, "Content" = x$Content,
      "Variables" = x$Variables, "Information" = x$Information
    ))
    list_of_variables <<- rbind(list_of_variables, zwisch)
  })

  if (nrow(list_of_variables) > 0) {
    list_of_variables$Object_Type <- "Value"
  }

  list_resp <- list("Values" = tibble::as_tibble(list_of_variables))
  attr(list_resp, "Name") <- results_json$Parameter$name
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <- results_json$Copyright
  names(list_resp) <- paste("Values of", results_json$Parameter$name)

  return(list_resp)
}

#####
#' Get Values From Variables From Statistic
#'
#' @param code a string with a maximum length of 15 characters. Code from a Destatis-Object. Only one code per iteration. "*"-Notations are possibly to be used as a placeholder.
#' @param detailed.variables a logical. Indicator if function should return the detailed output of the iteration including all object-related information or only a shortened output including only code and object title. This parameter only affects the details of the variables-related output. The default is FALSE.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is an parameter of the Destatis call itself. The default is "code".
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself for the variables, no further processing or the values-related objects.
#'
#' @return A list with all recalled elements from Destatis. Based on the detailed-parameter it contains more or less information, but always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find the values of variables in a specific statistic with the code "21111" and a detailed description of the variables
#' object <- get_values_from_variables_from_statistic(code = "21111", detailed.variables = T)
#' }
get_values_from_variables_from_statistic <- function(code = NULL,
                                                     detailed.variables = F,
                                                     sortcriterion = c("code", "content"),
                                                     ...) {
  if (!(is.character(code)) && length(code) < 1L && is.null(code)) {
    stop("code must be a single string or NULL", call. = FALSE)
  }

  if (detailed.variables == FALSE) {
    message("Use detailed = TRUE to obtain the complete output.")
  }

  if (!(isTRUE(detailed.variables) | isFALSE(detailed.variables))) {
    stop("detailed-parameter must be a TRUE or FALSE", call. = FALSE)
  }

  sortcriterion <- match.arg(sortcriterion)

  variables <- get_variables_from_statistic(
    code = code,
    detailed = detailed.variables,
    sortcriterion = sortcriterion,
    ...
  )

  list_values <- list()

  lapply(variables$Variables$Code, function(x) {
    zwisch <- get_values_from_variables(code = x, sortcriterion = sortcriterion)
    list_values <<- append(list_values, zwisch)
  })

  list_resp <- list(variables, list_values)

  return(list_resp)
}

# search variable ####
#' Search For Variable In Destatis
#'
#' Function to search for specific variables in Destatis databank.
#'
#' @param code a string with a maximum length of 6. Code from a Destatis-Object. Only one code per iteration. "*"-Notations are possibly to be used as a placeholder.
#' @param sortcriterion a string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the Destatis call itself. The default is "code".
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself, no further processing.
#'
#' @return A list with all recalled elements from Destatis. Always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find a specific variable "GES" in Destatis
#' object <- search_variables("GES")
#' }
search_variables <- function(code = NULL,
                             sortcriterion = c("code", "content"),
                             ...) {
  if (!(is.character(code)) && length(code) < 1L && is.null(code)) {
    stop("code must be a single string or NULL", call. = FALSE)
  }

  sortcriterion <- match.arg(sortcriterion)

  results_raw <- gen_api("catalogue/variables",
    username = gen_auth_get()$username,
    password = gen_auth_get()$password,
    selection = code,
    sortcriterion = sortcriterion,
    ...
  )

  if (httr2::resp_content_type(results_raw) == "application/json") {
    results_json <- httr2::resp_body_json(results_raw)
  }

  if (results_json$Status$Code != 0) {
    message(results_json$Status$Content)
  }


  list_of_variables <- data.frame()

  lapply(results_json$List, function(x) {
    zwisch <- rbind(c(
      "Code" = x$Code, "Content" = x$Content,
      "Type" = x$Type, "Information" = x$Information
    ))
    list_of_variables <<- rbind(list_of_variables, zwisch)
  })

  list_resp <- list("Variables" = tibble::as_tibble(list_of_variables))
  attr(list_resp, "Code") <- results_json$Parameter$selection
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <- results_json$Copyright

  return(list_resp)
}
