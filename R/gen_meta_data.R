# Statistic ####
#' gen_meta_data_stat: Meta-Information For Statistic
#'
#' Function to search for meta-information for a specific statistic.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Genesis-Object. Only one code per iteration.
#' @param error.ignore  a logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response.
#' @param ... Additional parameter of the Genesis API call. These parameters are only affecting the Genesis API call itself, no further processing.
#'
#' @return A list with all recalled elements from Genesis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find meta-information of the statistic with the code "12411"
#' object <- gen_meta_data_stats(code = "12411")
#' }
gen_meta_data_stats <- function(code = NULL,
                                     error.ignore = FALSE,
                                     ...) {
  # Check of parameter ####
  if (!(is.character(code)) && length(code) < 1L) {
    stop("code must be a single string", call. = T)
  }

  if (!(is.logical(error.ignore))) {
    stop("parameter has to be logical", call. = F)
  }

  # Data ####

  results_raw <- gen_api("metadata/statistic",
    username = gen_auth_get()$username,
    password = gen_auth_get()$password,
    name = code,
    ...
  )

  results_json <- test_if_json(results_raw)

  empty_object <- test_if_error_meta(results_json, para = error.ignore)

  if(isTRUE(empty_object)){
    df_stats <- "No `meta_information`- object found for your request."
  } else if(isFALSE(empty_object)){
    df_stats <- results_json$Status$Content
  } else if(empty_object == "DONE"){
    df_stats <- c(
      "Code" = results_json$Object$Code, "Content" = results_json$Object$Content, "Cubes" = results_json$Object$Cubes,
      "Variables" = results_json$Object$Variables, "Information" = results_json$Object$Information,
      "Time_from" = results_json$Object$Frequency[[1]]$From,
      "Time_to" = results_json$Object$Frequency[[1]]$To, "Time_type" = results_json$Object$Frequency[[1]]$Type
    )
  }

    attr(df_stats, "Code") <- results_json$Parameter$name
    attr(df_stats, "Method") <- results_json$Ident$Method
    attr(df_stats, "Updated") <- results_json$Object$Updated
    attr(df_stats, "Language") <- results_json$Parameter$language
    attr(df_stats, "Copyrigtht") <- results_json$Copyright

    return(df_stats)
}

# Variable ####
#' gen_meta_data_var: Meta-Information For Variable
#'
#' Function to search for meta-information for a specific variable.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Genesis-Object. Only one code per iteration. "*"-Notation is possible.
#' @param error.ignore  a logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response.
#' @param ... Additional parameter of the Genesis API call. These parameters are only affecting the Genesis API call itself, no further processing.
#'
#' @return A list with all recalled elements from Genesis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find meta-information of the variable with the code "FAMSTD"
#' object <- gen_meta_data_var(code = "FAMSTD")
#' }
gen_meta_data_var <- function(code = NULL,
                                    error.ignore = FALSE,
                                    ...) {
  # Check of parameter ####
  if (!(is.character(code)) && length(code) < 1L) {
    stop("code must be a single string", call. = T)
  }

  if (!(is.logical(error.ignore))) {
    stop("parameter has to be logical", call. = F)
  }

  # Data ####

  results_raw <- gen_api("metadata/variable", username = gen_auth_get()$username, password = gen_auth_get()$password, name = code, ...)

  results_json <- test_if_json(results_raw)

  empty_object <- test_if_error_meta(results_json, para = error.ignore)

  if(isTRUE(empty_object)){
    df_var <- "No `meta_information`- object found for your request."
  } else if(isFALSE(empty_object)){
    df_var <- results_json$Status$Content
  } else if(empty_object == "DONE"){
    df_var <- c(
      "Code" = results_json$Object$Code, "Content" = results_json$Object$Content, "Values" = results_json$Object$Values,
      "Type" = results_json$Object$Type,
      "Validity_from" = results_json$Object$Validity$From, "Validity_to" = results_json$Object$Validity$To
    )
  }

  list_resp <- list("General" = df_var, "Information" = results_json$Object$Information)

  attr(list_resp, "Code") <- results_json$Parameter$name
  attr(list_resp, "Method") <- results_json$Ident$Method
  attr(list_resp, "Updated") <- results_json$Object$Updated
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Copyrigtht") <- results_json$Copyright

  return(list_resp)
}

# Values ####
#' gen_meta_data_val: Meta-Information For Value
#'
#' Function to search for meta-information for a specific value.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Genesis-Object. Only one code per iteration.
#' @param error.ignore  a logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response.
#' @param ... Additional parameter of the Genesis API call. These parameters are only affecting the Genesis API call itself, no further processing.
#'
#' @return A list with all recalled elements from Genesis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find meta-information of the value with the code "LEDIG"
#' object <- gen_meta_data_val(code = "LEDIG")
#' }
gen_meta_data_val <- function(code = NULL,
                                 error.ignore = FALSE,
                                 ...) {
  # Check of parameter ####
  if (!(is.character(code)) && length(code) < 1L) {
    stop("code must be a single string", call. = T)
  }

  if (!(is.logical(error.ignore))) {
    stop("parameter has to be logical", call. = F)
  }

  # Data ####

  results_raw <- gen_api("metadata/value", username = gen_auth_get()$username, password = gen_auth_get()$password, name = code, ...)

  results_json <- test_if_json(results_raw)

  empty_object <- test_if_error_meta(results_json, para = error.ignore)

  if(isTRUE(empty_object)){
    df_value <- "No `meta_information`- object found for your request."
  } else if(isFALSE(empty_object)){
    df_value <- results_json$Status$Content
  } else if(empty_object == "DONE"){
    df_value <- c(
      "Code" = results_json$Object$Code, "Content" = results_json$Object$Content,
      "Variables" = results_json$Object$Variables
    )
  }

  list_resp <- list("General" = df_value, "Information" = results_json$Object$Information)
  attr(list_resp, "Code") <- results_json$Parameter$name
  attr(list_resp, "Method") <- results_json$Ident$Method
  attr(list_resp, "Updated") <- results_json$Object$Updated
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Copyrigtht") <- results_json$Copyright

  return(list_resp)
}


# Tables ####
#' gen_meta_data_tab: Meta-Information For Table
#'
#' Function to search for meta-information for a specific table.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Genesis-Object. Only one code per iteration.
#' @param error.ignore  a logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response.
#' @param ... Additional parameter of the Genesis API call. These parameters are only affecting the Genesis API call itself, no further processing.
#'
#' @return A list with all recalled elements from Genesis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find meta-information of the table with the code "11111"
#' object <- gen_meta_data_tab(code = "11111")
#' }
gen_meta_data_tab <- function(code = NULL,
                                 error.ignore = FALSE,
                                 ...) {
  # Check of parameter ####
  if (!(is.character(code)) && length(code) < 1L) {
    stop("code must be a single string", call. = T)
  }

  if (!(is.logical(error.ignore))) {
    stop("parameter has to be logical", call. = F)
  }

  # Data ####

  results_raw <- gen_api("metadata/table", username = gen_auth_get()$username, password = gen_auth_get()$password, name = code, ...)

  results_json <- test_if_json(results_raw)

  empty_object <- test_if_error_meta(results_json, para = error.ignore)

  if(isTRUE(empty_object)){
    char <- "No `meta_information`- object found for your request."
    structure <- NULL
    embedded <- NULL
  } else if(isFALSE(empty_object)){
    char <- results_json$Status$Content
    structure <- NULL
    embedded <- NULL
  } else if(empty_object == "DONE"){
    char <- c(
      "Code" = results_json$Object$Code, "Content" = results_json$Object$Content,
      "Time_From" = results_json$Object$Time$From, "Time_To" = results_json$Object$Time$To,
      "Valid" = results_json$Object$Valid
    )
    embedded <- cbind(
      "Code" = results_json$Object$Structure$Head$Code, "Content" = results_json$Object$Structure$Head$Content,
      "Type" = results_json$Object$Structure$Head$Type, "Values" = results_json$Object$Structure$Head$Values, "Selection" = results_json$Object$Structure$Head$Selected,
      "Updated" = results_json$Object$Structure$Head$Updated
    )
    structure <- list()
    structure$Head <- if (
      length(results_json$Object$Structure$Head$Structure) == 1) {
      cbind(
        "Code" = results_json$Object$Structure$Head$Structure[[1]]$Code, "Content" = results_json$Object$Structure$Head$Structure[[1]]$Content,
        "Type" = results_json$Object$Structure$Head$Structure[[1]]$Type, "Values" = results_json$Object$Structure$Head$Structure[[1]]$Values,
        "Selected" = results_json$Object$Structure$Head$Structure[[1]]$Selected,
        "Structure" = results_json$Object$Structure$Head$Structure[[1]]$Structure, "Updated" = results_json$Object$Structure$Head$Structure[[1]]$Updated
      )
    } else {
      cbind(
        "Code" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 1)), "Content" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 2)),
        "Type" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 3)), "Values" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 4)),
        "Selected" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 5)), "Structure" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 6)),
        "Updated" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 7))
      )
    }

    structure$Columns <- if (
      length(results_json$Object$Structure$Columns) == 1) {
      cbind(
        "Code" = results_json$Object$Structure$Columns[[1]]$Code, "Content" = results_json$Object$Structure$Columns[[1]]$Content,
        "Type" = results_json$Object$Structure$Columns[[1]]$Type, "Unit" = results_json$Object$Structure$Columns[[1]]$Unit,
        "Values" = results_json$Object$Structure$Columns[[1]]$Values, "Updated" = results_json$Object$Structure$Columns[[1]]$Updated
      )
    } else {
      cbind(
        "Code" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 1)), "Content" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 2)),
        "Type" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 3)), "Unit" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 4)),
        "Values" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 5)), "Updated" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 6))
      )
    }
    structure$Rows <- if (
      length(results_json$Object$Structure$Rows) == 1) {
      cbind(
        "Code" = results_json$Object$Structure$Rows[[1]]$Code, "Content" = results_json$Object$Structure$Rows[[1]]$Content,
        "Type" = results_json$Object$Structure$Rows[[1]]$Type, "Unit" = results_json$Object$Structure$Rows[[1]]$Unit,
        "Values" = results_json$Object$Structure$Rows[[1]]$Values, "Updated" = results_json$Object$Structure$Rows[[1]]$Updated
      )
    } else {
      cbind(
        "Code" = unlist(lapply(results_json$Object$Structure$Rows, `[[`, 1)), "Content" = unlist(lapply(results_json$Object$Structure$Rows, `[[`, 2)),
        "Type" = unlist(lapply(results_json$Object$Structure$Rows, `[[`, 3)), "Unit" = unlist(lapply(results_json$Object$Structure$Rows, `[[`, 4)),
        "Values" = unlist(lapply(results_json$Object$Structure$Rows, `[[`, 5)), "Updated" = unlist(lapply(results_json$Object$Structure$Rows, `[[`, 6))
      )
    }
  }

  list_resp <- list("General" = char, "Structure" = structure, "Embedded_in" = embedded)
  attr(list_resp, "Code") <- results_json$Parameter$name
  attr(list_resp, "Method") <- results_json$Ident$Method
  attr(list_resp, "Updated") <- results_json$Object$Updated
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Copyrigtht") <- results_json$Copyright

  return(list_resp)
}

# Cubes ####
#' gen_meta_data_cube: Meta-Information For Cube
#'
#' Function to search for meta-information for a specific cube.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Genesis-Object. Only one code per iteration.
#' @param error.ignore  a logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response.
#' @param ... Additional parameter of the Genesis API call. These parameters are only affecting the Genesis API call itself, no further processing.
#'
#' @return A list with all recalled elements from Genesis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find meta-information of the cube with the code "11111KE001"
#' object <- gen_meta_data_cube(code = "11111KE001")
#' }
gen_meta_data_cube <- function(code = NULL,
                                error.ignore = FALSE,
                                ...) {
  # Check of parameter ####
  if (!(is.character(code)) && length(code) < 1L) {
    stop("code must be a single string", call. = T)
  }

  if (!(is.logical(error.ignore))) {
    stop("parameter has to be logical", call. = F)
  }

  # Data ####

  results_raw <- gen_api("metadata/cube", username = gen_auth_get()$username, password = gen_auth_get()$password, name = code, ...)

  results_json <- test_if_json(results_raw)

  empty_object <- test_if_error_meta(results_json, para = error.ignore)

  if(isTRUE(empty_object)){
    char <- "No `meta_information`- object found for your request."
    time <- NULL
    stat <- NULL
    structure <- NULL
  } else if(isFALSE(empty_object)){
    char <- results_json$Status$Content
    time <- NULL
    stat <- NULL
    structure <- NULL
  } else if(empty_object == "DONE"){
    char <- c(
      "Code" = results_json$Object$Code, "Content" = results_json$Object$Content,
      "State" = results_json$Object$State, "Values" = results_json$Object$Values
    )
    time <- unlist(results_json$Object$Timeslices)
    stat <- c(
      "Code" = results_json$Object$Statistic$Code, "Content" = results_json$Object$Statistic$Content,
      "Updated" = results_json$Object$Statistic$Updated
    )


    structure <- list()
    structure$Axis <- if (
      length(results_json$Object$Structure$Axis) == 1) {
      cbind(
        "Code" = results_json$Object$Structure$Axis[[1]]$Code, "Content" = results_json$Object$Structure$Axis[[1]]$Content,
        "Type" = results_json$Object$Structure$Axis[[1]]$Type, "Updated" = results_json$Object$Structure$Axis[[1]]$Updated
      )
    } else {
      cbind(
        "Code" = unlist(lapply(results_json$Object$Structure$Axis, `[[`, 1)), "Content" = unlist(lapply(results_json$Object$Structure$Axis, `[[`, 2)),
        "Type" = unlist(lapply(results_json$Object$Structure$Axis, `[[`, 3)), "Updated" = unlist(lapply(results_json$Object$Structure$Axis, `[[`, 4))
      )
    }

    structure$Content <- if (
      length(results_json$Object$Structure$Contents) == 1) {
      cbind(
        "Code" = results_json$Object$Structure$Contents[[1]]$Code, "Content" = results_json$Object$Structure$Contents[[1]]$Content,
        "Type" = results_json$Object$Structure$Contents[[1]]$Type, "Unit" = results_json$Object$Structure$Contents[[1]]$Unit,
        "Values" = results_json$Object$Structure$Contents[[1]]$Values, "Updated" = results_json$Object$Structure$Contents[[1]]$Updated, "Timeslices" = results_json$Object$Structure$Contents[[1]]$Timeslices
      )
    } else {
      cbind(
        "Code" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 1)), "Content" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 2)),
        "Type" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 3)), "Unit" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 4)),
        "Values" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 5)), "Updated" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 7)),
        "Updated" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 6))
      )
    }
  }

  list_resp <- list("General" = char, "Timespan" = time, "Statistic_used" = stat, "Structure" = structure)
  attr(list_resp, "Code") <- results_json$Parameter$name
  attr(list_resp, "Method") <- results_json$Ident$Method
  attr(list_resp, "Updated") <- results_json$Object$Updated
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Copyrigtht") <- results_json$Copyright

  return(list_resp)
}

# Overall ####
#' gen_meta_data: Search For Meta-Information For All Types Of Objects
#'
#' @param codea string with a maximum length of 15 characters. Code from a Genesis-Object. Only one code per iteration.
#' @param category a string. Specific object-types: 'Cube', 'Statistic', "Table", "Variable" and 'Value'. The function needs a specified object type.
#' @param error.ignore  a logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response.
#' @param ... Additional parameter of the Genesis API call. These parameters are only affecting the Genesis API call itself, no further processing.
#'
#' @return A list with all recalled elements from Genesis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find meta-information of the table with the code "11111"
#' object <- gen_meta_data(code = "11111", category = "Table")
#' }
gen_meta_data <- function(code = NULL,
                          category = c("Cube", "Statistic", "Table", "Variable", "Value"),
                          error.ignore = FALSE,
                          ...) {
  if (!(is.character(code)) && length(code) < 1L) {
    stop("code must be a single string", call. = T)
  }

  if (!(length(category) == 1)) {
    stop("Available categories are Cube, Statistic, Table, Variable, or Value. Please choose one of them.")
  }

  if (!(is.logical(error.ignore))) {
    stop("parameter has to be logical", call. = F)
  }

  if (category == "Cube") {
    gen_meta_data_cube(code = code, error.ignore = error.ignore, ...)
  } else if (category == "Value") {
    gen_meta_data_val(code = code, error.ignore = error.ignore, ...)
  } else if (category == "Variable") {
    gen_meta_data_var(code = code, error.ignore = error.ignore, ...)
  } else if (category == "Table") {
    gen_meta_data_tab(code = code, error.ignore = error.ignore, ...)
  } else if (category == "Statistic") {
    gen_meta_data_stats(code = code, error.ignore = error.ignore, ...)
  } else {
    stop("Category is not found, please select a correct category.
         Available categories are Cube, Statistic, Table, Variable, or Value.
         Please choose one of them.", call. = T)
  }
}
