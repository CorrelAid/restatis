#' Search for Object in Restatis
#'
#' Additional information that time-series are treated as cubes.
#'
#' @param term
#' @param category
#' @param detailed
#' @param ordering
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
search_for <- function(term = NULL,
                       category = c("all", "tables", "statistics", "variables", "cubes"),
                       detailed = F,
                       ordering = T,
                       ...) {
  # Check of parameter ####
  if (!(is.character(term)) && length(term) < 1L && is.null(term)) {
    stop("term must be a single string or NULL", call. = F)
  }

  if (detailed == FALSE) {
    message("Use detailed = TRUE to obtain the complete output.")
  }

  if (!all(category %in% c("all", "tables", "statistics", "variables", "cubes"))) {
    stop("category must be one of the offered options", call. = F)
  }

  category <- match.arg(category)

  if (!(is.logical(detailed))) {
    stop("parameter has to be logical", call. = F)
  }

  if (!(is.logical(ordering))) {
    stop("parameter has to be logical", call. = F)
  }

  # Data ####
  results_raw <- gen_api("find/find", username = gen_auth_get()$username, password = gen_auth_get()$password, term = term, category = category, ...)

  if (httr2::resp_content_type(results_raw) == "application/json") {
    results_json <<- httr2::resp_body_json(results_raw)
  }

  if (results_json$Status$Code != 0) {
    message(results_json$Status$Content)
  }

  if (sum(unlist(lapply(results_json[4:8], function(x) {
    is.null(x)
  }))) == 5) {
    message("No related terms found for your code.")
  }

  if (detailed == T) {
    if (category == "all") {
      df_table <- data.frame()
      lapply(results_json$Tables, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time, "Spezifisch" = gsub(".*:", "", x$Content)))
        df_table <<- rbind(df_table, zwisch)
      })
      if (nrow(df_table) != 0) {
        df_table$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_table$Content, ignore.case = T)
        df_table$Variablen <- unlist(lapply(strsplit(df_table$Spezifisch, ","), length))

        if (ordering) {
          df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c(
            "Code", "Content",
            "Titel", "Time",
            "Variablen", "Spezifisch"
          )]
        } else {
          df_table <- df_table[, c(
            "Code", "Content",
            "Titel", "Time",
            "Variablen", "Spezifisch"
          )]
        }

        df_table$Object_Type <- "Table"
      }

      df_stats <- data.frame()
      lapply(results_json$Statistics, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Information" = x$Information, "Cubes" = x$Cubes, "Spezifisch" = gsub(".*:", "", x$Content)))
        df_stats <<- rbind(df_stats, zwisch)
      })
      if (nrow(df_stats) != 0) {
        df_stats$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_stats$Content, ignore.case = T)
        df_stats$Variablen <- unlist(lapply(strsplit(df_stats$Spezifisch, ","), length))

        if (ordering) {
          df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c("Code", "Content", "Titel", "Information", "Cubes", "Variablen", "Spezifisch")]
        } else {
          df_stats <- df_stats[, c("Code", "Content", "Titel", "Information", "Cubes", "Variablen", "Spezifisch")]
        }

        df_stats$Object_Type <- "Statistic"
      }

      df_variables <- data.frame()
      lapply(results_json$Variables, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Type" = x$Type, "Values" = x$Values, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content)))
        df_variables <<- rbind(df_variables, zwisch)
      })
      if (nrow(df_variables) != 0) {
        df_variables$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_variables$Content, ignore.case = T)
        df_variables$Variablen <- unlist(lapply(strsplit(df_variables$Spezifisch, ","), length))

        if (ordering) {
          df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c("Code", "Content", "Titel", "Values", "Information", "Variablen", "Spezifisch")]
        } else {
          df_variables <- df_variables[, c("Code", "Content", "Titel", "Values", "Information", "Variablen", "Spezifisch")]
        }

        df_variables$Object_Type <- "Variable"
      }

      df_cubes <- data.frame()
      lapply(results_json$Cubes, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time, "LatestUpdate" = x$LatestUpdate, "State" = x$State, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content)))
        df_cubes <<- rbind(df_cubes, zwisch)
      })
      if (nrow(df_cubes) != 0) {
        df_cubes$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_cubes$Content, ignore.case = T)
        df_cubes$Variablen <- unlist(lapply(strsplit(df_cubes$Spezifisch, ","), length))

        if (ordering) {
          df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c("Code", "Content", "Titel", "Time", "LatestUpdate", "State", "Information", "Variablen", "Spezifisch")]
        } else {
          df_cubes <- df_cubes[, c("Code", "Content", "Titel", "Time", "LatestUpdate", "State", "Information", "Variablen", "Spezifisch")]
        }

        df_cubes$Object_Type <- "Cube"
      }

      list_resp <- list(
        "Tables" = tibble::as_tibble(df_table), "Statistics" = tibble::as_tibble(df_stats),
        "Variables" = tibble::as_tibble(df_variables), "Cubes" = tibble::as_tibble(df_cubes)
      )
      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright

      return(list_resp)
    } else if (category == "tables") {
      df_table <- data.frame()
      lapply(results_json$Tables, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time, "Spezifisch" = gsub(".*:", "", x$Content)))
        df_table <<- rbind(df_table, zwisch)
      })
      if (nrow(df_table) != 0) {
        df_table$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_table$Content, ignore.case = T)
        df_table$Variablen <- unlist(lapply(strsplit(df_table$Spezifisch, ","), length))

        if (ordering) {
          df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c("Code", "Content", "Titel", "Time", "Variablen", "Spezifisch")]
        } else {
          df_table <- df_table[, c("Code", "Content", "Titel", "Time", "Variablen", "Spezifisch")]
        }

        df_table$Object_Type <- "Table"
      }


      list_resp <- list("Tables" = tibble::as_tibble(df_table))
      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright
      return(list_resp)
    } else if (category == "statistics") {
      df_stats <- data.frame()
      lapply(results_json$Statistics, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Information" = x$Information, "Cubes" = x$Cubes, "Spezifisch" = gsub(".*:", "", x$Content)))
        df_stats <<- rbind(df_stats, zwisch)
      })
      if (nrow(df_stats) != 0) {
        df_stats$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_stats$Content, ignore.case = T)
        df_stats$Variablen <- unlist(lapply(strsplit(df_stats$Spezifisch, ","), length))

        if (ordering) {
          df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c(
            "Code", "Content",
            "Titel", "Information",
            "Cubes", "Variablen", "Spezifisch"
          )]
        } else {
          df_stats <- df_stats[, c(
            "Code", "Content",
            "Titel", "Information",
            "Cubes", "Variablen", "Spezifisch"
          )]
        }

        df_stats$Object_Type <- "Statistic"
      }

      list_resp <- list("Statistics" = tibble::as_tibble(df_stats))
      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright
      return(list_resp)
    } else if (category == "variables") {
      df_variables <- data.frame()
      lapply(results_json$Variables, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Type" = x$Type, "Values" = x$Values, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content)))
        df_variables <<- rbind(df_variables, zwisch)
      })
      if (nrow(df_variables) != 0) {
        df_variables$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_variables$Content, ignore.case = T)
        df_variables$Variablen <- unlist(lapply(strsplit(df_variables$Spezifisch, ","), length))

        if (ordering) {
          df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c(
            "Code", "Content",
            "Titel",
            "Values", "Type",
            "Information",
            "Variablen", "Spezifisch"
          )]
        } else {
          df_variables <- df_variables[, c(
            "Code", "Content",
            "Titel",
            "Values", "Type",
            "Information",
            "Variablen", "Spezifisch"
          )]
        }

        df_variables$Object_Type <- "Variable"
      }

      list_resp <- list("Variables" = tibble::as_tibble(df_variables))
      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright
      return(list_resp)
    } else if (category == "cubes") {
      df_cubes <- data.frame()
      lapply(results_json$Cubes, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time, "LatestUpdate" = x$LatestUpdate, "State" = x$State, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content)))
        df_cubes <<- rbind(df_cubes, zwisch)
      })
      if (nrow(df_cubes) != 0) {
        df_cubes$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_cubes$Content, ignore.case = T)
        df_cubes$Variablen <- unlist(lapply(strsplit(df_cubes$Spezifisch, ","), length))

        if (ordering) {
          df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c(
            "Code", "Content",
            "Titel",
            "Time", "LatestUpdate", "State",
            "Information",
            "Variablen", "Spezifisch"
          )]
        } else {
          df_cubes <- df_cubes[, c(
            "Code", "Content",
            "Titel",
            "Time", "LatestUpdate", "State",
            "Information",
            "Variablen", "Spezifisch"
          )]
        }

        df_cubes$Object_Type <- "Cube"
      }

      list_resp <- list("Cubes" = tibble::as_tibble(df_cubes))
      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright
      return(list_resp)
    }
  }


  if (detailed == F) {
    if (category == "all") {
      df_table <- data.frame()
      lapply(results_json$Tables, function(x) {
        zwisch <- rbind(c(
          "Code" = x$Code, "Content" = x$Content,
          "Time" = x$Time, "Spezifisch" = gsub(".*:", "", x$Content)
        ))
        df_table <<- rbind(df_table, zwisch)
      })
      if (nrow(df_table) != 0) {
        df_table$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_table$Content, ignore.case = T)
        df_table$Variablen <- unlist(lapply(strsplit(df_table$Spezifisch, ","), length))

        if (ordering) {
          df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c("Code", "Content")]
        } else {
          df_table <- df_table[, c("Code", "Content")]
        }

        df_table$Object_Type <- "Table"
      }

      df_stats <- data.frame()
      lapply(results_json$Statistics, function(x) {
        zwisch <- rbind(c(
          "Code" = x$Code, "Content" = x$Content,
          "Information" = x$Information, "Cubes" = x$Cubes,
          "Spezifisch" = gsub(".*:", "", x$Content)
        ))
        df_stats <<- rbind(df_stats, zwisch)
      })
      if (nrow(df_stats) != 0) {
        df_stats$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_stats$Content, ignore.case = T)
        df_stats$Variablen <- unlist(lapply(strsplit(df_stats$Spezifisch, ","), length))

        if (ordering) {
          df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c("Code", "Content")]
        } else {
          df_stats <- df_stats[, c("Code", "Content")]
        }

        df_stats$Object_Type <- "Statistic"
      }

      df_variables <- data.frame()
      lapply(results_json$Variables, function(x) {
        zwisch <- rbind(c(
          "Code" = x$Code, "Content" = x$Content,
          "Type" = x$Type, "Values" = x$Values,
          "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content)
        ))
        df_variables <<- rbind(df_variables, zwisch)
      })
      if (nrow(df_variables) != 0) {
        df_variables$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_variables$Content, ignore.case = T)
        df_variables$Variablen <- unlist(lapply(strsplit(df_variables$Spezifisch, ","), length))

        if (ordering) {
          df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c("Code", "Content")]
        } else {
          df_variables <- df_variables[, c("Code", "Content")]
        }

        df_variables$Object_Type <- "Variable"
      }

      df_cubes <- data.frame()
      lapply(results_json$Cubes, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time, "LatestUpdate" = x$LatestUpdate, "State" = x$State, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content)))
        df_cubes <<- rbind(df_cubes, zwisch)
      })
      if (nrow(df_cubes) != 0) {
        df_cubes$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_cubes$Content, ignore.case = T)
        df_cubes$Variablen <- unlist(lapply(strsplit(df_cubes$Spezifisch, ","), length))

        if (ordering) {
          df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c("Code", "Content")]
        } else {
          df_cubes <- df_cubes[, c("Code", "Content")]
        }

        df_cubes$Object_Type <- "Cube"
      }

      list_resp <- list(
        "Tables" = tibble::as_tibble(df_table), "Statistics" = tibble::as_tibble(df_stats),
        "Variables" = tibble::as_tibble(df_variables), "Cubes" = tibble::as_tibble(df_cubes)
      )
      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright

      return(list_resp)
    } else if (category == "tables") {
      df_table <- data.frame()
      lapply(results_json$Tables, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time, "Spezifisch" = gsub(".*:", "", x$Content)))
        df_table <<- rbind(df_table, zwisch)
      })
      if (nrow(df_table) != 0) {
        df_table$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_table$Content, ignore.case = T)
        df_table$Variablen <- unlist(lapply(strsplit(df_table$Spezifisch, ","), length))

        if (ordering) {
          df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c("Code", "Content")]
        } else {
          df_table <- df_table[, c("Code", "Content")]
        }

        df_table$Object_Type <- "Table"
      }


      list_resp <- list("Tables" = tibble::as_tibble(df_table))
      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright
      return(list_resp)
    } else if (category == "statistics") {
      df_stats <- data.frame()
      lapply(results_json$Statistics, function(x) {
        zwisch <- rbind(c(
          "Code" = x$Code, "Content" = x$Content, "Information" = x$Information,
          "Cubes" = x$Cubes, "Spezifisch" = gsub(".*:", "", x$Content)
        ))
        df_stats <<- rbind(df_stats, zwisch)
      })
      if (nrow(df_stats) != 0) {
        df_stats$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_stats$Content, ignore.case = T)
        df_stats$Variablen <- unlist(lapply(strsplit(df_stats$Spezifisch, ","), length))

        if (ordering) {
          df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c("Code", "Content")]
        } else {
          df_stats <- df_stats[, c("Code", "Content")]
        }

        df_stats$Object_Type <- "Statistic"
      }

      list_resp <- list("Statistics" = tibble::as_tibble(df_stats))
      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright
      return(list_resp)
    } else if (category == "variables") {
      df_variables <- data.frame()
      lapply(results_json$Variables, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Type" = x$Type, "Values" = x$Values, "Information" = x$Information, "Spezifisch" = gsub(".*:", "", x$Content), "Title" = x$Content))
        df_variables <<- rbind(df_variables, zwisch)
      })
      if (nrow(df_variables) != 0) {
        df_variables$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_variables$Content, ignore.case = T)
        df_variables$Variablen <- unlist(lapply(strsplit(df_variables$Spezifisch, ","), length))

        if (ordering) {
          df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c("Code", "Content")]
        } else {
          df_variables <- df_variables[, c("Code", "Content")]
        }

        df_variables$Object_Type <- "Variable"
      }

      list_resp <- list("Variables" = tibble::as_tibble(df_variables))
      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright
      return(list_resp)
    } else if (category == "cubes") {
      df_cubes <- data.frame()
      lapply(results_json$Cubes, function(x) {
        zwisch <- rbind(c(
          "Code" = x$Code, "Content" = x$Content, "Time" = x$Time, "LatestUpdate" = x$LatestUpdate,
          "State" = x$State, "Information" = x$Information,
          "Spezifisch" = gsub(".*:", "", x$Content)
        ))
        df_cubes <<- rbind(df_cubes, zwisch)
      })
      if (nrow(df_cubes) != 0) {
        df_cubes$Titel <- grepl(paste(unlist(strsplit(term, c(" & | und "))), collapse = "|"), df_cubes$Content, ignore.case = T)
        df_cubes$Variablen <- unlist(lapply(strsplit(df_cubes$Spezifisch, ","), length))

        if (ordering) {
          df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c("Code", "Content")]
        } else {
          df_cubes <- df_cubes[, c("Code", "Content")]
        }

        df_cubes$Object_Type <- "Cube"
      }

      list_resp <- list("Cubes" = tibble::as_tibble(df_cubes))
      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright
      return(list_resp)
    }
  }
}
