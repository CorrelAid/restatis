# Statistic ####
#' Meta-Information For Statistics
#'
#' Function to search for meta-information for a specific statistic.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Destatis-Object. Only one code per iteration.
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself, no further processing.
#'
#' @return A list with all recalled elements from Destatis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find meta-information of the statistic with the code "12411"
#' object <- meta_data_for_statistics(code = "12411")
#' }
meta_data_for_statistics <- function(code = NULL,
                                     ...) {
  # Check of parameter ####
  if (!(is.character(code)) && length(code) < 1L) {
    stop("code must be a single string", call. = T)
  }

  # Data ####

  results_raw <- gen_api("metadata/statistic",
    username = gen_auth_get()$username, password = gen_auth_get()$password, name = code, ...
  )

  if (httr2::resp_content_type(results_raw) == "application/json") {
    results_json <- httr2::resp_body_json(results_raw)
  }

  if (results_json$Status$Code != 0) {
    message(results_json$Status$Content)
  } else {
    df_stats <- c(
      "Code" = results_json$Object$Code, "Content" = results_json$Object$Content, "Cubes" = results_json$Object$Cubes,
      "Variables" = results_json$Object$Variables, "Information" = results_json$Object$Information,
      "Time_from" = results_json$Object$Frequency[[1]]$From,
      "Time_to" = results_json$Object$Frequency[[1]]$To, "Time_type" = results_json$Object$Frequency[[1]]$Type
    )

    attr(df_stats, "Code") <- results_json$Parameter$name
    attr(df_stats, "Method") <- results_json$Ident$Method
    attr(df_stats, "Updated") <- results_json$Object$Updated
    attr(df_stats, "Language") <- results_json$Parameter$language
    attr(df_stats, "Copyrigtht") <- results_json$Copyright

    return(df_stats)
  }
}

# Variable ####
#' Meta-Information For Variables
#'
#' Function to search for meta-information for a specific variable.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Destatis-Object. Only one code per iteration. "*"-Notation is possible.
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself, no further processing.
#'
#' @return A list with all recalled elements from Destatis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find meta-information of the variable with the code "FAMSTD"
#' object <- meta_data_for_variables(code = "FAMSTD")
#' }
meta_data_for_variables <- function(code = NULL,
                                    ...) {
  # Check of parameter ####
  if (!(is.character(code)) && length(code) < 1L) {
    stop("code must be a single string", call. = T)
  }

  # Data ####

  results_raw <- gen_api("metadata/variable", username = gen_auth_get()$username, password = gen_auth_get()$password, name = code, ...)

  if (httr2::resp_content_type(results_raw) == "application/json") {
    results_json <- httr2::resp_body_json(results_raw)
  }

  if (results_json$Status$Code != 0) {
    message(results_json$Status$Content)
  } else {
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
#' Meta-Information For Values
#'
#' Function to search for meta-information for a specific value.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Destatis-Object. Only one code per iteration.
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself, no further processing.
#'
#' @return A list with all recalled elements from Destatis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find meta-information of the value with the code "LEDIG"
#' object <- meta_data_for_values(code = "LEDIG")
#' }
meta_data_for_values <- function(code = NULL,
                                 ...) {
  # Check of parameter ####
  if (!(is.character(code)) && length(code) < 1L) {
    stop("code must be a single string", call. = T)
  }

  # Data ####

  results_raw <- gen_api("metadata/value", username = gen_auth_get()$username, password = gen_auth_get()$password, name = code, ...)

  if (httr2::resp_content_type(results_raw) == "application/json") {
    results_json <- httr2::resp_body_json(results_raw)
  }

  if (results_json$Status$Code != 0) {
    message(results_json$Status$Content)
  } else {
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
#' Meta-Information For Tables
#'
#' Function to search for meta-information for a specific table.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Destatis-Object. Only one code per iteration.
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself, no further processing.
#'
#' @return A list with all recalled elements from Destatis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find meta-information of the table with the code "11111"
#' object <- meta_data_for_tables(code = "11111")
#' }
meta_data_for_tables <- function(code = NULL,
                                 ...) {
  # Check of parameter ####
  if (!(is.character(code)) && length(code) < 1L) {
    stop("code must be a single string", call. = T)
  }

  # Data ####

  results_raw <- gen_api("metadata/table", username = gen_auth_get()$username, password = gen_auth_get()$password, name = code, ...)

  if (httr2::resp_content_type(results_raw) == "application/json") {
    results_json <- httr2::resp_body_json(results_raw)
  }

  if (results_json$Status$Code != 0) {
    message(results_json$Status$Content)
  } else {
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
#' Meta-Information For Cubes
#'
#' Function to search for meta-information for a specific cube.
#'
#' @param code a string with a maximum length of 15 characters. Code from a Destatis-Object. Only one code per iteration.
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself, no further processing.
#'
#' @return A list with all recalled elements from Destatis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find meta-information of the cube with the code "11111KE001"
#' object <- meta_data_for_tables(code = "11111KE001")
#' }
meta_data_for_cubes <- function(code = NULL,
                                ...) {
  # Check of parameter ####
  if (!(is.character(code)) && length(code) < 1L) {
    stop("code must be a single string", call. = T)
  }

  # Data ####

  results_raw <- gen_api("metadata/cube", username = gen_auth_get()$username, password = gen_auth_get()$password, name = code, ...)

  if (httr2::resp_content_type(results_raw) == "application/json") {
    results_json <- httr2::resp_body_json(results_raw)
  }

  if (results_json$Status$Code != 0) {
    message(results_json$Status$Content)
  } else {
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
#' Search For Meta-Information For All Types Of Objects
#'
#' @param codea string with a maximum length of 15 characters. Code from a Destatis-Object. Only one code per iteration.
#' @param category a string. Specific Destatis-Object-types: 'Cube', 'Statistic', "Table", "Variable" and 'Value'. The function needs a specified object type.
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself, no further processing.
#'
#' @return A list with all recalled elements from Destatis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find meta-information of the table with the code "11111"
#' object <- meta_data_for_tables(code = "11111", category = "Table")
#' }
meta_data_for <- function(code = NULL,
                          category = c("Cube", "Statistic", "Table", "Variable", "Value"),
                          ...) {
  if (!(is.character(code)) && length(code) < 1L) {
    stop("code must be a single string", call. = T)
  }

  if (!(length(category) == 1)) {
    stop("Available categories are Cube, Statistic, Table, Variable, or Value. Please choose one of them.")
  }

  if (category == "Cube") {
    meta_data_for_cubes(code = code, ...)
  } else if (category == "Value") {
    meta_data_for_values(code = code, ...)
  } else if (category == "Variable") {
    meta_data_for_variables(code = code, ...)
  } else if (category == "Table") {
    meta_data_for_tables(code = code, ...)
  } else if (category == "Statistic") {
    meta_data_for_statistics(code = code, ...)
  } else {
    stop("Category is not found, please select a correct category.
         Available categories are Cube, Statistic, Table, Variable, or Value.
         Please choose one of them.", call. = T)
  }
}
