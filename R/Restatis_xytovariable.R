# Restatis - xytovariable function
#install.packages("httr2")
library(httr2)
library(rlang)
source("C:/Users/Dorian/Documents/restatis/restatis/R/auth.R")
source("C:/Users/Dorian/Documents/restatis/restatis/R/api.R")
Sys.setenv(LANG = "de")

xy_to_variable <- function(name = NULL,
                            selection = NULL,
                            category =  c("tables", "statistics", "cubes", "timeseries"),
                            area = NULL, # Gibt es eine Auflistung der gültigen Parameter hier?
                            searchcriterion = NULL,
                            sortcriterion = NULL,
                            type = NULL,
                            pagelength = NULL,
                            language = NULL,
                            ...) {

  if(is.null(language)){
    language = Sys.getenv("LANG") # Hilfsfunktion "languagecheck"
  }

  if(!(language %in% c("de","eng"))){
    stop("Available languages are German (de) or English (eng).")
  }

  if(is.null(type)){
    type = "alle"
    message("No type was specified so all types will be included.")
  }

  if(!(type %in% c("klassifizierend", "insgesamt", "räumlich", "sachlich", "wert", "zeitlich", "zeitidentifizierend", "alle"))){
    stop("One of the following types must be specified: klassifizierend, insgesamt, räumlich, sachlich, wert, zeitlich, zeitidentifizierend, alle.")
  }


  if("tables" %in% category){
    results_raw <- gen_api("catalogue/tables2variable",
                           username = gen_auth_get()$username,
                           password = gen_auth_get()$password,
                           name = name,
                           selection = selection,
                           area = area,
                           pagelength = pagelength,
                           language = language,
                           ...) # Sollen weiter Parameter erlaubt sein?

    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    }
    df_tables <- data.frame()
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time))
      df_tables <<- rbind(df_tables, zwisch)})
    # df_tables <- df_tables[with(df_tables), c("Code", "Content", "Time")]
    # return(df_tables)
  } else {
    df_tables <- data.frame()
  }

  if("statistics" %in% category){
    results_raw <- gen_api("catalogue/statistics2variable",
                           username = gen_auth_get()$username,
                           password = gen_auth_get()$password,
                           name = name,
                           selection = selection,
                           area = area,
                           searchcriterion = searchcriterion,
                           sortcriterion = sortcriterion,
                           type = type,
                           pagelength = pagelength,
                           language = language,
                           ...) # Sollen weiter Parameter erlaubt sein?

    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    }

    df_statistics <- data.frame()
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time, "Type" =x$Type, "Values" = x$Values, "Information" = x$Information))
      df_statistics <<- rbind(df_statistics, zwisch)})
    # df_statistics <- df_statistics[with(df_statistics), c("Code", "Content", "Time")]
    # return(df_statistics)
  } else {
    df_statistics <- data.frame()
  }

  if("cubes" %in% category){
    results_raw <- gen_api("catalogue/cubes2variable",
                           username = gen_auth_get()$username,
                           password = gen_auth_get()$password,
                           name = name,
                           selection = selection,
                           area = area,
                           pagelength = pagelength,
                           language = language,
                           ...) # Sollen weiter Parameter erlaubt sein?

    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    }

    df_cubes <- data.frame()
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time, "State" = x$State, "LatestUpdate" = x$LatestUpdate))
      df_cubes <<- rbind(df_cubes, zwisch)})
    # df_cubes <- df_cubes[with(df_cubes), c("Code", "Content", "Time")]
    # return(df_cubes)
  } else {
    df_cubes <- data.frame()
  }

  if("timeseries" %in% category){
    results_raw <- gen_api("catalogue/timeseries2variable",
                           username = gen_auth_get()$username,
                           password = gen_auth_get()$password,
                           name = name,
                           selection = selection,
                           area = area,
                           pagelength = pagelength,
                           language = language,
                           ...) # Sollen weiter Parameter erlaubt sein?

    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    }

    df_timeseries <- data.frame()
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time, "State" = x$State, "LatestUpdate" = x$LatestUpdate))
      df_timeseries <<- rbind(df_timeseries, zwisch)})
    # df_timeseries <- df_timeseries[with(df_timeseries), c("Code", "Content", "Time")]
    # return(df_timeseries)
  } else {
    df_timeseries <- data.frame()
  }

  list_resp <- list("Tables" = df_tables, "Statistics" = df_statistics, "Cubes" = df_cubes, "Timeseries" = df_timeseries)
  return(list_resp)
}

xy_to_variable("ADSAW2")

