# Restatis - find function
search_for <- function(term = NULL,
                       category = c("tables", "statistics", "variables", "cubes", "time-series"),
                       details = F,
                       ordering = T,
                       ...) {

  # Check of parameter ####
  if(!(is.character(term)) && length(term) < 1L && is.null(term)){
    stop("term must be a single string or NULL", call. = T)
  }

  if(identical(category, c("tables", "statistics", "variables", "cubes", "time-series"))){
    category <- "all"
  }
  if(!(category %in% c("all", "tables", "statistics", "variables", "cubes", "time-series"))){
    stop("category must be one of the offered options", call. = T)
  }

  if(!(is.logical(details))){
    stop("parameter has to be logical", call. = T)
  }

  if(!(is.logical(ordering))){
    stop("parameter has to be logical", call. = T)
  }
  # Data ####

  results_raw <- gen_api("find/find",  username = gen_auth_get()$username,password = gen_auth_get()$password, term = term, category = category, ...)

  if(resp_content_type(results_raw) == "application/json"){
    results_json <<- resp_body_json(results_raw)
  }

  if(details == T){
  if( category == "all"){

    df_table <- data.frame()
    lapply(results_json$Tables, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,40), "Title" = x$Content, "Time" = x$Time, "Spezifisch" = gsub(".*:", "", x$Content)))
      df_table <<- rbind(df_table, zwisch)
    })
    if(nrow(df_table) != 0){
      df_table$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_table$Title, ignore.case = T)
      df_table$Variablen <- unlist(lapply(strsplit(df_table$Spezifisch, ","), length))

      if(ordering){
      df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c("Code", "Content", "Title",
                                                                        "Titel", "Time",
                                                                        "Variablen", "Spezifisch")]
      } else {
        df_table <- df_table[, c("Code", "Content", "Title",
                                                                          "Titel", "Time",
                                                                          "Variablen", "Spezifisch")]
      }

      df_table$Object_Type <- "Table"
    }

    df_stats <- data.frame()
    lapply(results_json$Statistics, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,40), "Information" = x$Information, "Cubes" = x$Cubes, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
      df_stats <<- rbind(df_stats, zwisch)
    })
    if(nrow(df_stats) != 0){
      df_stats$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_stats$Title, ignore.case = T)
      df_stats$Variablen <- unlist(lapply(strsplit(df_stats$Spezifisch, ","), length))

      if(ordering){
      df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c("Code", "Content", "Title", "Titel", "Information", "Cubes", "Variablen", "Spezifisch")]
      } else {
        df_stats <- df_stats[, c("Code", "Content", "Title", "Titel", "Information", "Cubes", "Variablen", "Spezifisch")]
      }

      df_stats$Object_Type <- "Statistic"
    }

    df_variables <- data.frame()
    lapply(results_json$Variables, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,40), "Type" = x$Type, "Values" = x$Values, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
      df_variables <<- rbind(df_variables, zwisch)
    })
    if(nrow(df_variables) != 0){
      df_variables$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_variables$Title, ignore.case = T)
      df_variables$Variablen <- unlist(lapply(strsplit(df_variables$Spezifisch, ","), length))

      if(ordering){
      df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c("Code", "Content", "Title", "Titel", "Values", "Information", "Variablen", "Spezifisch")]
      } else {
        df_variables <- df_variables[, c("Code", "Content", "Title", "Titel", "Values", "Information", "Variablen", "Spezifisch")]
      }

      df_variables$Object_Type <- "Variable"
    }

    df_cubes <- data.frame()
    lapply(results_json$Cubes, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,40), "Time" = x$Time, "LatestUpdate" = x$LatestUpdate, "State" = x$State, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
      df_cubes <<- rbind(df_cubes, zwisch)
    })
    if(nrow(df_cubes) != 0){
      df_cubes$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_cubes$Title, ignore.case = T)
      df_cubes$Variablen <- unlist(lapply(strsplit(df_cubes$Spezifisch, ","), length))

      if(ordering){
      df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c("Code", "Content", "Title" , "Titel", "Time", "LatestUpdate", "State", "Information", "Variablen", "Spezifisch")]
      } else {
        df_cubes <- df_cubes[, c("Code", "Content", "Title" , "Titel", "Time", "LatestUpdate", "State", "Information", "Variablen", "Spezifisch")]
      }

      df_cubes$Object_Type <- "Cube"
    }

    df_time.series <- data.frame()
    lapply(results_json$Timeseries, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,40), "Information" = x$Information, "Cubes" = x$Cubes, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
      df_time.series <<- rbind(df_time.series, zwisch)
    })
    if(nrow(df_time.series) != 0){
      df_time.series$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_time.series$Title, ignore.case = T)
      df_time.series$Variablen <- unlist(lapply(strsplit(df_time.series$Spezifisch, ","), length))

      if(ordering){
      df_time.series <- df_time.series[with(df_time.series, order(-Titel, -Variablen)), c("Code", "Content", "Title", "Titel", "Cubes", "Information", "Variablen", "Spezifisch")]
      } else {
        df_time.series <- df_time.series[, c("Code", "Content", "Title", "Titel", "Cubes", "Information", "Variablen", "Spezifisch")]
      }

      df_time.series$Object_Type <- "Time-serie"
    }

    list_resp <- list("Tables" = df_table, "Statistics" = df_stats, "Variables" = df_variables, "Cubes" = df_cubes, "Time-series" = df_time.series)
    attr(list_resp, "Term") <-  results_json$Parameter$term
    attr(list_resp, "Language") <-  results_json$Parameter$language
    attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
    attr(list_resp, "Copyrigtht") <-  results_json$Copyright

    return(list_resp)

  }

  else if(category == "tables"){
    df_table <- data.frame()
    lapply(results_json$Tables, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,40), "Title" = x$Content, "Time" = x$Time, "Spezifisch" = gsub(".*:", "", x$Content)))
      df_table <<- rbind(df_table, zwisch)
    })
    if(nrow(df_table) != 0){
      df_table$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_table$Title, ignore.case = T)
      df_table$Variablen <- unlist(lapply(strsplit(df_table$Spezifisch, ","), length))

      if(ordering){
      df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c("Code", "Content", "Title", "Titel", "Time", "Variablen", "Spezifisch")]
      } else {
        df_table <- df_table[, c("Code", "Content", "Title", "Titel", "Time", "Variablen", "Spezifisch")]
      }

      df_table$Object_Type <- "Table"
    }


    list_resp <- list("Tables" = df_table)
    attr(list_resp, "Term") <-  results_json$Parameter$term
    attr(list_resp, "Language") <-  results_json$Parameter$language
    attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
    attr(list_resp, "Copyrigtht") <-  results_json$Copyright
    return(list_resp)

  }
  else if(category == "statistics"){

    df_stats <- data.frame()
    lapply(results_json$Statistics, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,40), "Information" = x$Information, "Cubes" = x$Cubes, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
      df_stats <<- rbind(df_stats, zwisch)
    })
    if(nrow(df_stats) != 0){
      df_stats$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_stats$Title, ignore.case = T)
      df_stats$Variablen <- unlist(lapply(strsplit(df_stats$Spezifisch, ","), length))

      if(ordering){
      df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c("Code", "Content", "Title",
                                                                        "Titel", "Information",
                                                                        "Cubes", "Variablen", "Spezifisch")]
      } else {
        df_stats <- df_stats[, c("Code", "Content", "Title",
                                                                          "Titel", "Information",
                                                                          "Cubes", "Variablen", "Spezifisch")]
      }

      df_stats$Object_Type <- "Statistic"
    }

    list_resp <- list("Statistics" = df_stats)
    attr(list_resp, "Term") <-  results_json$Parameter$term
    attr(list_resp, "Language") <-  results_json$Parameter$language
    attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
    attr(list_resp, "Copyrigtht") <-  results_json$Copyright
    return(list_resp)

  }
  else if( category == "variables"){

    df_variables <- data.frame()
    lapply(results_json$Variables, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,40), "Type" = x$Type, "Values" = x$Values, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
      df_variables <<- rbind(df_variables, zwisch)
    })
    if(nrow(df_variables) != 0){
      df_variables$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_variables$Title, ignore.case = T)
      df_variables$Variablen <- unlist(lapply(strsplit(df_variables$Spezifisch, ","), length))

      if(ordering){
        df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c("Code", "Content", "Title",
                                                                                    "Titel",
                                                                                    "Values", "Type",
                                                                                    "Information",
                                                                                    "Variablen", "Spezifisch")]
      } else {
        df_variables <- df_variables[, c("Code", "Content", "Title",
                                                                                      "Titel",
                                                                                      "Values", "Type",
                                                                                      "Information",
                                                                                      "Variablen", "Spezifisch")]
      }

      df_variables$Object_Type <- "Variable"
    }

    list_resp <- list("Variables" = df_variables)
    attr(list_resp, "Term") <-  results_json$Parameter$term
    attr(list_resp, "Language") <-  results_json$Parameter$language
    attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
    attr(list_resp, "Copyrigtht") <-  results_json$Copyright
    return(list_resp)

  }
  else if(category == "cubes"){
    df_cubes <- data.frame()
    lapply(results_json$Cubes, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,40), "Time" = x$Time, "LatestUpdate" = x$LatestUpdate, "State" = x$State, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
      df_cubes <<- rbind(df_cubes, zwisch)
    })
    if(nrow(df_cubes) != 0){
      df_cubes$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_cubes$Title, ignore.case = T)
      df_cubes$Variablen <- unlist(lapply(strsplit(df_cubes$Spezifisch, ","), length))

      if(ordering){
      df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c("Code", "Content", "Title",
                                                                        "Titel",
                                                                        "Time", "LatestUpdate", "State",
                                                                        "Information",
                                                                        "Variablen", "Spezifisch")]
      } else {
        df_cubes <- df_cubes[, c("Code", "Content", "Title",
                                                                          "Titel",
                                                                          "Time", "LatestUpdate", "State",
                                                                          "Information",
                                                                          "Variablen", "Spezifisch")]
      }

      df_cubes$Object_Type <- "Cube"
    }

    list_resp <- list("Cubes" = df_cubes)
    attr(list_resp, "Term") <-  results_json$Parameter$term
    attr(list_resp, "Language") <-  results_json$Parameter$language
    attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
    attr(list_resp, "Copyrigtht") <-  results_json$Copyright
    return(list_resp)

  }
  else if( category == "time-series"){

    df_time.series <- data.frame()
    lapply(results_json$Timeseries, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,40), "Time" = x$Time, "LatestUpdate" = x$LatestUpdate, "State" = x$State, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
      df_time.series <<- rbind(df_time.series, zwisch)
    })
    if(nrow(df_time.series) != 0){
      df_time.series$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_time.series$Title, ignore.case = T)
      df_time.series$Variablen <- unlist(lapply(strsplit(df_time.series$Spezifisch, ","), length))

      if(ordering){
      df_time.series <- df_time.series[with(df_time.series, order(-Titel, -Variablen)), c("Code", "Content", "Title", "Titel", "Time", "Information", "State", "LatestUpdate", "Variablen", "Spezifisch")]
      } else {
        df_time.series <- df_time.series[, c("Code", "Content", "Title", "Titel", "Time", "Information", "State", "LatestUpdate", "Variablen", "Spezifisch")]
      }

      df_time.series$Object_Type <- "Time-serie"
    }


    list_resp <- list("Time-series" = df_time.series)
    attr(list_resp, "Term") <-  results_json$Parameter$term
    attr(list_resp, "Language") <-  results_json$Parameter$language
    attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
    attr(list_resp, "Copyright") <-  results_json$Copyright
    return(list_resp)

  }
}


  if(details == F){
    if( category == "all"){

      df_table <- data.frame()
      lapply(results_json$Tables, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,60), "Title" = x$Content, "Time" = x$Time, "Spezifisch" = gsub(".*:", "", x$Content)))
        df_table <<- rbind(df_table, zwisch)
      })
      if(nrow(df_table) != 0){
        df_table$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_table$Title, ignore.case = T)
        df_table$Variablen <- unlist(lapply(strsplit(df_table$Spezifisch, ","), length))

        if(ordering){
          df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c("Code", "Content"
                                                                            #"Title","Titel", "Time","Variablen", "Spezifisch"
                                                                            )]
        } else {
          df_table <- df_table[, c("Code", "Content",
                                   #"Title","Titel", "Time","Variablen", "Spezifisch"
          )]
        }

        df_table$Object_Type <- "Table"
      }

      df_stats <- data.frame()
      lapply(results_json$Statistics, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,60), "Information" = x$Information, "Cubes" = x$Cubes, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
        df_stats <<- rbind(df_stats, zwisch)
      })
      if(nrow(df_stats) != 0){
        df_stats$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_stats$Title, ignore.case = T)
        df_stats$Variablen <- unlist(lapply(strsplit(df_stats$Spezifisch, ","), length))

        if(ordering){
          df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c("Code", "Content"
                                                                            #"Title", "Titel", "Information", "Cubes", "Variablen", "Spezifisch"
          )]
        } else {
          df_stats <- df_stats[, c("Code", "Content"
                                   #"Title", "Titel", "Information", "Cubes", "Variablen", "Spezifisch"
                                   )]
        }

        df_stats$Object_Type <- "Statistic"
      }

      df_variables <- data.frame()
      lapply(results_json$Variables, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,60), "Type" = x$Type, "Values" = x$Values, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
        df_variables <<- rbind(df_variables, zwisch)
      })
      if(nrow(df_variables) != 0){
        df_variables$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_variables$Title, ignore.case = T)
        df_variables$Variablen <- unlist(lapply(strsplit(df_variables$Spezifisch, ","), length))

        if(ordering){
          df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c("Code", "Content"
                                                                                        #"Title", "Titel", "Values", "Information", "Variablen", "Spezifisch"
          )]
        } else {
          df_variables <- df_variables[, c("Code", "Content"
                                           #"Title", "Titel", "Values", "Information", "Variablen", "Spezifisch"
                                           )]
        }

        df_variables$Object_Type <- "Variable"
      }

      df_cubes <- data.frame()
      lapply(results_json$Cubes, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,60), "Time" = x$Time, "LatestUpdate" = x$LatestUpdate, "State" = x$State, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
        df_cubes <<- rbind(df_cubes, zwisch)
      })
      if(nrow(df_cubes) != 0){
        df_cubes$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_cubes$Title, ignore.case = T)
        df_cubes$Variablen <- unlist(lapply(strsplit(df_cubes$Spezifisch, ","), length))

        if(ordering){
          df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c("Code", "Content"
                                                                            #"Title" , "Titel", "Time", "LatestUpdate", "State", "Information", "Variablen", "Spezifisch"
          )]
        } else {
          df_cubes <- df_cubes[, c("Code", "Content"
                                   #"Title" , "Titel", "Time", "LatestUpdate", "State", "Information", "Variablen", "Spezifisch"
                                   )]
        }

        df_cubes$Object_Type <- "Cube"
      }

      df_time.series <- data.frame()
      lapply(results_json$Timeseries, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,60), "Time" = x$Time, "LatestUpdate" = x$LatestUpdate, "State" = x$State, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
        df_time.series <<- rbind(df_time.series, zwisch)
      })
      if(nrow(df_time.series) != 0){
        df_time.series$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_time.series$Title, ignore.case = T)
        df_time.series$Variablen <- unlist(lapply(strsplit(df_time.series$Spezifisch, ","), length))

        if(ordering){
          df_time.series <- df_time.series[with(df_time.series, order(-Titel, -Variablen)), c("Code", "Content"
                                                                                              #"Title", "Titel", "Cubes", "Information", "Variablen", "Spezifisch"
                                                                                              )]
        } else {
          df_time.series <- df_time.series[, c("Code", "Content"
                                               #"Title", "Titel", "Cubes", "Information", "Variablen", "Spezifisch"
          )]
        }

        df_time.series$Object_Type <- "Time-serie"
      }

      list_resp <- list("Tables" = df_table, "Statistics" = df_stats, "Variables" = df_variables, "Cubes" = df_cubes, "Time-series" = df_time.series)
      attr(list_resp, "Term") <-  results_json$Parameter$term
      attr(list_resp, "Language") <-  results_json$Parameter$language
      attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <-  results_json$Copyright

      return(list_resp)

    }

    else if(category == "tables"){
      df_table <- data.frame()
      lapply(results_json$Tables, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,60), "Title" = x$Content, "Time" = x$Time, "Spezifisch" = gsub(".*:", "", x$Content)))
        df_table <<- rbind(df_table, zwisch)
      })
      if(nrow(df_table) != 0){
        df_table$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_table$Title, ignore.case = T)
        df_table$Variablen <- unlist(lapply(strsplit(df_table$Spezifisch, ","), length))

        if(ordering){
          df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c("Code", "Content"
                                                                            #"Title", "Titel", "Time", "Variablen", "Spezifisch"
          )]
        } else {
          df_table <- df_table[, c("Code", "Content"
                                   #"Title", "Titel", "Time", "Variablen", "Spezifisch"
                                   )]
        }

        df_table$Object_Type <- "Table"
      }


      list_resp <- list("Tables" = df_table)
      attr(list_resp, "Term") <-  results_json$Parameter$term
      attr(list_resp, "Language") <-  results_json$Parameter$language
      attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <-  results_json$Copyright
      return(list_resp)

    }
    else if(category == "statistics"){

      df_stats <- data.frame()
      lapply(results_json$Statistics, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,60), "Information" = x$Information, "Cubes" = x$Cubes, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
        df_stats <<- rbind(df_stats, zwisch)
      })
      if(nrow(df_stats) != 0){
        df_stats$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_stats$Title, ignore.case = T)
        df_stats$Variablen <- unlist(lapply(strsplit(df_stats$Spezifisch, ","), length))

        if(ordering){
          df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c("Code", "Content"
                                                                            #"Title","Titel", "Information", "Cubes", "Variablen", "Spezifisch"
          )]
        } else {
          df_stats <- df_stats[, c("Code", "Content"
                                   #"Title","Titel", "Information", "Cubes", "Variablen", "Spezifisch"
                                   )]
        }

        df_stats$Object_Type <- "Statistic"
      }

      list_resp <- list("Statistics" = df_stats)
      attr(list_resp, "Term") <-  results_json$Parameter$term
      attr(list_resp, "Language") <-  results_json$Parameter$language
      attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <-  results_json$Copyright
      return(list_resp)

    }
    else if( category == "variables"){

      df_variables <- data.frame()
      lapply(results_json$Variables, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,60), "Type" = x$Type, "Values" = x$Values, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
        df_variables <<- rbind(df_variables, zwisch)
      })
      if(nrow(df_variables) != 0){
        df_variables$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_variables$Title, ignore.case = T)
        df_variables$Variablen <- unlist(lapply(strsplit(df_variables$Spezifisch, ","), length))

        if(ordering){
          df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c("Code", "Content"
                                                                                        #"Title","Titel","Values","Information","Variablen", "Spezifisch"
          )]
        } else {
          df_variables <- df_variables[, c("Code", "Content"
                                           #"Title","Titel","Values","Information","Variablen", "Spezifisch"
                                           )]
        }

        df_variables$Object_Type <- "Variable"
      }

      list_resp <- list("Variables" = df_variables)
      attr(list_resp, "Term") <-  results_json$Parameter$term
      attr(list_resp, "Language") <-  results_json$Parameter$language
      attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <-  results_json$Copyright
      return(list_resp)

    }
    else if(category == "cubes"){
      df_cubes <- data.frame()
      lapply(results_json$Cubes, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,60), "Time" = x$Time, "LatestUpdate" = x$LatestUpdate, "State" = x$State, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
        df_cubes <<- rbind(df_cubes, zwisch)
      })
      if(nrow(df_cubes) != 0){
        df_cubes$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_cubes$Title, ignore.case = T)
        df_cubes$Variablen <- unlist(lapply(strsplit(df_cubes$Spezifisch, ","), length))

        if(ordering){
          df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c("Code", "Content"
                                                                            #"Title",    "Titel",    "Time", "LatestUpdate", "State",      "Information",            "Variablen", "Spezifisch"
                                                                            )]
        } else {
          df_cubes <- df_cubes[, c("Code", "Content"
                                   #"Title",    "Titel",    "Time", "LatestUpdate", "State",      "Information",            "Variablen", "Spezifisch"
          )]
        }

        df_cubes$Object_Type <- "Cube"
      }

      list_resp <- list("Cubes" = df_cubes)
      attr(list_resp, "Term") <-  results_json$Parameter$term
      attr(list_resp, "Language") <-  results_json$Parameter$language
      attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <-  results_json$Copyright
      return(list_resp)

    }
    else if( category == "time-series"){

      df_time.series <- data.frame()
      lapply(results_json$Timeseries, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,60), "Information" = x$Information, "State" = x$State, "Time" = x$Time, "Latestpdate" = x$LatestUpdate, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
        df_time.series <<- rbind(df_time.series, zwisch)
      })
      if(nrow(df_time.series) != 0){
        df_time.series$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_time.series$Title, ignore.case = T)
        df_time.series$Variablen <- unlist(lapply(strsplit(df_time.series$Spezifisch, ","), length))

        if(ordering){
          df_time.series <- df_time.series[with(df_time.series, order(-Titel, -Variablen)), c("Code", "Content"
                                                                                              #"Title", "Titel", "Cubes", "Information", "Variablen", "Spezifisch"
                                                                                              )]
        } else {
          df_time.series <- df_time.series[, c("Code", "Content"
                                               #"Title", "Titel", "Cubes", "Information", "Variablen", "Spezifisch"
                                               )]
        }

        df_time.series$Object_Type <- "Time-serie"
      }


      list_resp <- list("Time-series" = df_time.series)
      attr(list_resp, "Term") <-  results_json$Parameter$term
      attr(list_resp, "Language") <-  results_json$Parameter$language
      attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
      attr(list_resp, "Copyright") <-  results_json$Copyright
      return(list_resp)

    }
  }
}
