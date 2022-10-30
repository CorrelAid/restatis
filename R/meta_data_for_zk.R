# Statistics ####
meta_data_for_statistics <- function(code = NULL,
                       ...) {

  # Check of parameter ####
  if(!(is.character(code)) && length(code) < 1L){
    stop("code must be a single string", call. = T)
  }


  # Data ####

  results_raw <- gen_api("metadata/statistic",  username = gen_auth_get()$username,password = gen_auth_get()$password, name = code, ...)

  if(resp_content_type(results_raw) == "application/json"){
    results_json <<- resp_body_json(results_raw)
  }

  if(results_json$Status$Code == 104){
    stop("No object with this code found", call. = T)
  } else if(results_json$Status$Code == 114){
    stop("No rights to read this", call. = T)
  } else {
  df_stats <- c("Code" = results_json$Object$Code, "Content" = substr(results_json$Object$Content, 1,40), "Title" = results_json$Object$Content, "Cubes" = results_json$Object$Cubes,
                        "Variables" = results_json$Object$Variables, "Information" = results_json$Object$Information,
                        "Time_from" = results_json$Object$Frequency[[1]]$From, "Time_to" = results_json$Object$Frequency[[1]]$To, "Time_type" = results_json$Object$Frequency[[1]]$Type)

  attr(df_stats, "Code") <-  results_json$Parameter$name
  attr(df_stats, "Method") <-  results_json$Ident$Method
  attr(df_stats, "Updated") <-  results_json$Object$Updated
  attr(df_stats, "Language") <-  results_json$Parameter$language
  attr(df_stats, "Copyrigtht") <-  results_json$Copyright

  return(df_stats)
  }


}

# Variable ####
meta_data_for_variables <- function(code = NULL,
                                     ...) {

  # Check of parameter ####
  if(!(is.character(code)) && length(code) < 1L){
    stop("code must be a single string", call. = T)
  }

  # Data ####

  results_raw <- gen_api("metadata/variable",  username = gen_auth_get()$username,password = gen_auth_get()$password, name = code, ...)

  if(resp_content_type(results_raw) == "application/json"){
    results_json <<- resp_body_json(results_raw)
  }

  df_var <- c("Code" = results_json$Object$Code, "Content" = substr(results_json$Object$Content, 1,40), "Title" = results_json$Object$Content, "Values" = results_json$Object$Values,
                      "Type" = results_json$Object$Type,
                      "Validity_from" = results_json$Object$Validity$From, "Validity_to" = results_json$Object$Validity$To)

  list_resp <- list("General" = df_var, "Information" = results_json$Object$Information)
  attr(list_resp, "Code") <-  results_json$Parameter$name
  attr(list_resp, "Method") <-  results_json$Ident$Method
  attr(list_resp, "Updated") <-  results_json$Object$Updated
  attr(list_resp, "Language") <-  results_json$Parameter$language
  attr(list_resp, "Copyrigtht") <-  results_json$Copyright

  return(list_resp)

}

# Values ####
meta_data_for_values <- function(code = NULL,
                                    ...) {

  # Check of parameter ####
  if(!(is.character(code)) && length(code) < 1L){
    stop("code must be a single string", call. = T)
  }

  # Data ####

  results_raw <- gen_api("metadata/value",  username = gen_auth_get()$username,password = gen_auth_get()$password, name = code, ...)

  if(resp_content_type(results_raw) == "application/json"){
    results_json <<- resp_body_json(results_raw)
  }

  df_value <- c("Code" = results_json$Object$Code, "Content" = substr(results_json$Object$Content, 1,40), "Title" = results_json$Object$Content, "Variables" = results_json$Object$Variables)

  list_resp <- list("General" = df_value, "Information" = results_json$Object$Information)
  attr(list_resp, "Code") <-  results_json$Parameter$name
  attr(list_resp, "Method") <-  results_json$Ident$Method
  attr(list_resp, "Updated") <-  results_json$Object$Updated
  attr(list_resp, "Language") <-  results_json$Parameter$language
  attr(list_resp, "Copyrigtht") <-  results_json$Copyright

  return(list_resp)

}


# Time series ####
meta_data_for_timeseries <- function(code = NULL,
                                 ...) {

  # Check of parameter ####
  if(!(is.character(code)) && length(code) < 1L){
    stop("code must be a single string", call. = T)
  }

  # Data ####

  results_raw <- gen_api("metadata/timeseries",  username = gen_auth_get()$username,password = gen_auth_get()$password, name = code, ...)

  if(resp_content_type(results_raw) == "application/json"){
    results_json <<- resp_body_json(results_raw)
  }

  char <- c("Code" = results_json$Object$Code, "Content" = substr(results_json$Object$Content, 1,40), "Title" = results_json$Object$Content,
            "State" = results_json$Object$State, "Values" = results_json$Object$Values, "Time" = results_json$Object$Time, "Valid" = results_json$Object$Valid)
  time <- unlist(results_json$Object$Timeslices)
  stat <- c("Code" = results_json$Object$Statistic$Code, "Content" = substr(results_json$Object$Statistic$Content, 1,40), "Title" = results_json$Object$Statistic$Content,
            "Updated" = results_json$Object$Statistic$Updated)
  structure <- list()
  structure$Axis <- cbind("Code" = unlist(lapply(results_json$Object$Structure$Axis, `[[`, 1)), "Content" = unlist(lapply(results_json$Object$Structure$Axis, `[[`, 2)),
                          "Type" = unlist(lapply(results_json$Object$Structure$Axis, `[[`, 3)), "Updated" = unlist(lapply(results_json$Object$Structure$Axis, `[[`, 4)))
  structure$Content <- if(
    length(results_json$Object$Structure$Contents) == 1){
                                cbind("Code" = results_json$Object$Structure$Contents[[1]]$Code, "Content" = results_json$Object$Structure$Contents[[1]]$Content,
          "Type" = results_json$Object$Structure$Contents[[1]]$Type, "Unit" = results_json$Object$Structure$Contents[[1]]$Unit,
          "Values" =results_json$Object$Structure$Contents[[1]]$Values, "Updated" = results_json$Object$Structure$Contents[[1]]$Updated)


                      } else { cbind("Code" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 1)), "Content" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 2)),
                          "Type" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 3)), "Unit" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 4)),
                          "Values" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 5)), "Updated" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 7)),
                          "Timeslices" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 6)))
                      }

  list_resp <- list("General" = char, "Timespan" = time, "Statistic_used" = stat, "Structure" = structure)
  attr(list_resp, "Code") <-  results_json$Parameter$name
  attr(list_resp, "Method") <-  results_json$Ident$Method
  attr(list_resp, "Updated") <-  results_json$Object$Updated
  attr(list_resp, "Language") <-  results_json$Parameter$language
  attr(list_resp, "Copyrigtht") <-  results_json$Copyright

  return(list_resp)

}

# Tables ####
meta_data_for_tables <- function(code = NULL,
                                     ...) {

  # Check of parameter ####
  if(!(is.character(code)) && length(code) < 1L){
    stop("code must be a single string", call. = T)
  }

  # Data ####

  results_raw <- gen_api("metadata/table",  username = gen_auth_get()$username,password = gen_auth_get()$password, name = code, ...)

  if(resp_content_type(results_raw) == "application/json"){
    results_json <<- resp_body_json(results_raw)
  }

  char <- c("Code" = results_json$Object$Code, "Content" = substr(results_json$Object$Content, 1,40), "Title" = results_json$Object$Content,
            "Time_From" = results_json$Object$Time$From, "Time_To" = results_json$Object$Time$To,
            "Valid" = results_json$Object$Valid)
  embedded <- cbind("Code" = results_json$Object$Structure$Head$Code,  "Content" = results_json$Object$Structure$Head$Content,
                    "Type" = results_json$Object$Structure$Head$Type, "Values" = results_json$Object$Structure$Head$Values, "Selection" = results_json$Object$Structure$Head$Selected,
                    "Updated" = results_json$Object$Structure$Head$Updated)
  structure <- list()
  structure$Head <- if(
    length(results_json$Object$Structure$Head$Structure) == 1){
    cbind("Code" = results_json$Object$Structure$Head$Structure[[1]]$Code, "Content" = results_json$Object$Structure$Head$Structure[[1]]$Content,
          "Type" = results_json$Object$Structure$Head$Structure[[1]]$Type, "Values" =results_json$Object$Structure$Head$Structure[[1]]$Values,
          "Selected" = results_json$Object$Structure$Head$Structure[[1]]$Selected,
          "Structure" =results_json$Object$Structure$Head$Structure[[1]]$Structure, "Updated" = results_json$Object$Structure$Head$Structure[[1]]$Updated)


  } else { cbind("Code" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 1)), "Content" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 2)),
                 "Type" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 3)), "Values" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 4)),
                 "Selected" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 5)), "Structure" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 6)),
                 "Updated" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 7)))
  }

  structure$Columns <- if(
    length(results_json$Object$Structure$Columns) == 1){
    cbind("Code" = results_json$Object$Structure$Columns[[1]]$Code, "Content" = results_json$Object$Structure$Columns[[1]]$Content,
          "Type" = results_json$Object$Structure$Columns[[1]]$Type, "Unit" = results_json$Object$Structure$Columns[[1]]$Unit,
          "Values" =results_json$Object$Structure$Columns[[1]]$Values, "Updated" = results_json$Object$Structure$Columns[[1]]$Updated)


  } else { cbind("Code" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 1)), "Content" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 2)),
                 "Type" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 3)), "Unit" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 4)),
                 "Values" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 5)), "Updated" = unlist(lapply(results_json$Object$Structure$Columns, `[[`, 6)))
  }
  structure$Rows <- if(
    length(results_json$Object$Structure$Rows) == 1){
    cbind("Code" = results_json$Object$Structure$Rows[[1]]$Code, "Content" = results_json$Object$Structure$Rows[[1]]$Content,
          "Type" = results_json$Object$Structure$Rows[[1]]$Type, "Unit" = results_json$Object$Structure$Rows[[1]]$Unit,
          "Values" =results_json$Object$Structure$Rows[[1]]$Values, "Updated" = results_json$Object$Structure$Rows[[1]]$Updated)


  } else { cbind("Code" = unlist(lapply(results_json$Object$Structure$Rows, `[[`, 1)), "Content" = unlist(lapply(results_json$Object$Structure$Rows, `[[`, 2)),
                 "Type" = unlist(lapply(results_json$Object$Structure$Rows, `[[`, 3)), "Unit" = unlist(lapply(results_json$Object$Structure$Rows, `[[`, 4)),
                 "Values" = unlist(lapply(results_json$Object$Structure$Rows, `[[`, 5)), "Updated" = unlist(lapply(results_json$Object$Structure$Rows, `[[`, 6)))
  }

  list_resp <- list("General" = char, "Structure" = structure, "Embedded_in" = embedded)
  attr(list_resp, "Code") <-  results_json$Parameter$name
  attr(list_resp, "Method") <-  results_json$Ident$Method
  attr(list_resp, "Updated") <-  results_json$Object$Updated
  attr(list_resp, "Language") <-  results_json$Parameter$language
  attr(list_resp, "Copyrigtht") <-  results_json$Copyright

  return(list_resp)

}

# Cubes ####
meta_data_for_cubes <- function(code = NULL,
                                 ...) {

  # Check of parameter ####
  if(!(is.character(code)) && length(code) < 1L){
    stop("code must be a single string", call. = T)
  }

  # Data ####

  results_raw <- gen_api("metadata/cube",  username = gen_auth_get()$username,password = gen_auth_get()$password, name = code, ...)

  if(resp_content_type(results_raw) == "application/json"){
    results_json <<- resp_body_json(results_raw)
  }

  char <- c("Code" = results_json$Object$Code, "Content" = substr(results_json$Object$Content, 1,40), "Title" = results_json$Object$Content,
            "State" = results_json$Object$State, "Values" = results_json$Object$Values)
  time <- unlist(results_json$Object$Timeslices)
  stat <- c("Code" = results_json$Object$Statistic$Code, "Content" = substr(results_json$Object$Statistic$Content, 1,40), "Title" = results_json$Object$Statistic$Content,
            "Updated" = results_json$Object$Statistic$Updated)


  structure <- list()
  structure$Axis <- if(
    length(results_json$Object$Structure$Axis) == 1){
    cbind("Code" = results_json$Object$Structure$Axis[[1]]$Code, "Content" = results_json$Object$Structure$Axis[[1]]$Content,
          "Type" = results_json$Object$Structure$Axis[[1]]$Type, "Updated" = results_json$Object$Structure$Axis[[1]]$Updated)


  } else { cbind("Code" = unlist(lapply(results_json$Object$Structure$Axis, `[[`, 1)), "Content" = unlist(lapply(results_json$Object$Structure$Axis, `[[`, 2)),
                 "Type" = unlist(lapply(results_json$Object$Structure$Axis, `[[`, 3)), "Updated" = unlist(lapply(results_json$Object$Structure$Axis, `[[`, 4)))
  }

  structure$Content <- if(
    length(results_json$Object$Structure$Contents) == 1){
    cbind("Code" = results_json$Object$Structure$Contents[[1]]$Code, "Content" = results_json$Object$Structure$Contents[[1]]$Content,
          "Type" = results_json$Object$Structure$Contents[[1]]$Type, "Unit" = results_json$Object$Structure$Contents[[1]]$Unit,
          "Values" =results_json$Object$Structure$Contents[[1]]$Values, "Updated" = results_json$Object$Structure$Contents[[1]]$Updated, "Timeslices" = results_json$Object$Structure$Contents[[1]]$Timeslices)


  } else { cbind("Code" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 1)), "Content" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 2)),
                 "Type" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 3)), "Unit" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 4)),
                 "Values" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 5)), "Updated" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 7)),
                 "Updated" = unlist(lapply(results_json$Object$Structure$Contents, `[[`, 6)))
  }



  list_resp <- list("General" = char, "Timespan" = time, "Statistic_used" = stat, "Structure" = structure)
  attr(list_resp, "Code") <-  results_json$Parameter$name
  attr(list_resp, "Method") <-  results_json$Ident$Method
  attr(list_resp, "Updated") <-  results_json$Object$Updated
  attr(list_resp, "Language") <-  results_json$Parameter$language
  attr(list_resp, "Copyrigtht") <-  results_json$Copyright

  return(list_resp)

}

# Overall ####
meta_data_for <- function(code = NULL,
                          type = c("cube", "timeserie", "statistic", "table", "variable", "value"),
                                  ...) {

  if(!(is.character(code)) && length(code) < 1L){
    stop("code must be a single string", call. = T)
  }

  type <- match.arg(type)

  if(type == "cube"){
    meta_data_for_cubes(code = code, ...)
  }

  else if(type == "value"){
    meta_data_for_values(code = code, ...)
  }

  else if(type == "variable"){
    meta_data_for_variables(code = code, ...)
  }

  else if(type == "table"){
    meta_data_for_tables(code = code, ...)
  }

  else if(type == "statistic"){
    meta_data_for_statistics(code = code, ...)
  }

  else if(type == "timeserie"){
    meta_data_for_timeseries(code = code, ...)
  }

  else {
    stop("type is not found, please select a correct type", call. = T)
  }
}

