xy_to_statistic <- function(name = NULL,
                            category =  c("tables", "variables", "cubes", "timeseries"),
                            language = "de",
                            details = FALSE,
                             ...) {


  if(!(language %in% c("de","eng"))){
    stop("Available languages are German (de) or English (eng).")
  }

  # if(is.null(type)){
  #   type = "alle"
  #   message("No type was specified so all types will be included.")
  # }

  if(details == FALSE){
    message("Use details = TRUE to obtain the complete output.")
  }

  # if(!(type %in% c("klassifizierend", "insgesamt", "räumlich", "sachlich", "wert", "zeitlich", "zeitidentifizierend", "alle"))){
  #   stop("One of the following types must be specified: klassifizierend, insgesamt, räumlich, sachlich, wert, zeitlich, zeitidentifizierend, alle.")
  # }

  if("tables" %in% category){
    results_raw <- gen_api("catalogue/tables2statistic",
                           username = gen_auth_get()$username,
                           password = gen_auth_get()$password,
                           name = name,
                           language = language,
                           ...)

    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    }
    df_tables <- data.frame()
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time))
      df_tables <<- rbind(df_tables, zwisch)})
  } else {
    df_tables <- data.frame()
  }

  df_tables$Object_Type <- "Table"

  if("variables" %in% category){
    results_raw <- gen_api("catalogue/variables2statistic",
                           username = gen_auth_get()$username,
                           password = gen_auth_get()$password,
                           name = name,
                           language = language,
                           ...)

    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    }

    df_variables <- data.frame()

    if(details == TRUE){
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Type" =x$Type, "Values" = x$Values, "Information" = x$Information))
      df_variables <<- rbind(df_variables, zwisch)})}
    else {
      lapply(results_json$List, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Type" =x$Type))
        df_variables <<- rbind(df_variables, zwisch)})
    }
  } else {
    df_variables <- data.frame()
  }

  df_variables$Object_Type <- "Variable"

  if("cubes" %in% category){
    results_raw <- gen_api("catalogue/cubes2statistic",
                           username = gen_auth_get()$username,
                           password = gen_auth_get()$password,
                           name = name,
                           language = language,
                           ...)

    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    }

    df_cubes <- data.frame()

    if(details==TRUE){
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time, "State" = x$State, "LatestUpdate" = x$LatestUpdate, "Information" = x$Information))
      df_cubes <<- rbind(df_cubes, zwisch)})}
    else {
      lapply(results_json$List, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time))
        df_cubes <<- rbind(df_cubes, zwisch)})
    }

  } else {
    df_cubes <- data.frame()
  }

  df_cubes$Object_Type <- "Cube"


  if("timeseries" %in% category){
    results_raw <- gen_api("catalogue/timeseries2statistic",
                           username = gen_auth_get()$username,
                           password = gen_auth_get()$password,
                           name = name,
                           language = language,
                           ...)

    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    }

    df_timeseries <- data.frame()

    if(details==TRUE){
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time, "State" = x$State, "LatestUpdate" = x$LatestUpdate, "Information" = x$Information))
      df_timeseries <<- rbind(df_timeseries, zwisch)})}
    else{
      lapply(results_json$List, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time))
        df_timeseries <<- rbind(df_timeseries, zwisch)})
    }

  } else {
    df_timeseries <- data.frame()
  }

  df_timeseries$Object_Type <- "Time-serie"

list_resp <- list("Tables" = df_tables, "Variables" = df_variables, "Cubes" = df_cubes, "Timeseries" = df_timeseries)
attr(list_resp, "Term") <-  results_json$Parameter$term
attr(list_resp, "Language") <-  results_json$Parameter$language
attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
attr(list_resp, "Copyrigtht") <-  results_json$Copyright
return(list_resp)
}



