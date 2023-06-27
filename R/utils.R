resp_check_data_csv <- function(resp) {
  if (httr2::resp_content_type(resp) != "text/csv") {
    stop("No data found that meets the specified parameters", call. = FALSE)
  }
}

param_check_year <- function(year) {
  if (as.integer(year) < 1900 || as.integer(year) > 2100) {
    stop("The parameter 'year' has been misspecified (>= 1900 or <= 2100).",
      call. = FALSE
    )
  }
}

param_collapse_vec <- function(vec) {
  paste0(vec, collapse = ",")
}

#-------------------------------------------------------------------------------

# Forming_evas ----

forming_evas <- function(list_of) {
  evas_list_long_20220724 <- restatis::evas_list_long_20220724

  # Progress them
  list_of$Main <- apply(list_of, 1, function(x) {
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 1)]
  })

  list_of$Main2 <- apply(list_of, 1, function(x) {
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 2)]
  })

  list_of$Main3 <- apply(list_of, 1, function(x) {
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 3)]
  })

  list_of$Main5 <- apply(list_of, 1, function(x) {
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 5)]
  })

  nestedlist <- split(list_of, list_of$Main, drop = TRUE)

  nestedlist <- lapply(nestedlist, function(x) {
    split(x, x["Main2"], drop = TRUE)
  })

  nestedlist <- lapply(nestedlist, function(x) {
    lapply(x, function(y) {
      split(y, y["Main3"])
    })
  })

  nestedlist <- lapply(nestedlist, function(x) {
    lapply(x, function(y) {
      lapply(y, function(z) {
        split(z, z["Main5"])
      })
    })
  })

  aba <- lapply(
    nestedlist, function(d) {
      lapply(d, function(z) {
        lapply(z, function(y) {
          lapply(y, function(x) {
            x[!(
              names(x)
              %in% c("Main", "Main2", "Main3", "Main5"))]
          })
        })
      })
    }
  )

  return(aba)
}

#-------------------------------------------------------------------------------

# check_function_input ----

check_function_input <- function(code = NULL,
                                 term = NULL,
                                 sortcriterion = NULL,
                                 category = NULL,
                                 detailed = NULL,
                                 type = NULL,
                                 date = NULL,
                                 similarity = NULL,
                                 error.ignore = NULL,
                                 ordering = NULL,
                                 caller = NULL) {
  # Code & Term ----

  if (is.null(code) && is.null(term)) {
    if (!(caller %in% c(
      "gen_search_vars",
      "restatis::gen_search_vars"
    ))) {
      stop("Parameter 'code' or 'term' must NOT be NULL.",
        call. = FALSE
      )
    }
  }

  # Code ----

  if (!is.null(code)) {
    if (length(code) != 1L) {
      stop("Parameter 'code' must be a single string.",
        call. = FALSE
      )
    }

    if (!is.character(code)) {
      stop("Parameter 'code' has to be of type 'character'.",
        call. = FALSE
      )
    }
  }

  # Term ----

  if (!is.null(term)) {
    if (length(term) != 1L) {
      stop("Parameter 'term' must be a single string.",
        call. = FALSE
      )
    }

    if (!is.character(term)) {
      stop("Parameter 'term' has to be of type 'character'.",
        call. = FALSE
      )
    }

    if (nchar(term) > 15 && !(caller %in% c("gen_find", "restatis::gen_find"))) {
      stop("Parameter 'term' cannot consist of more than 15 characters.",
        call. = FALSE
      )
    }
  }

  # sortcriterion ----

  if (!is.null(sortcriterion)) {
    if (!is.character(sortcriterion)) {
      stop("Parameter 'sortcriterion' has to be of type 'character'.",
        call. = FALSE
      )
    }

    if (length(sortcriterion) == 1) {
      if (!(sortcriterion %in% c("code", "content"))) {
        stop("Parameter 'sortcriterion' has to be 'code' or 'content'.",
          call. = FALSE
        )
      }
    }
  }

  # category ----

  if (!is.null(category)) {
    if (!(length(category) %in% c(1:3)) && caller %in% c(
      "restatis::gen_catalogue",
      "restatis::gen_objects2var",
      "restatis::gen_objects2stat",
      "gen_catalogue",
      "gen_objects2var",
      "gen_objects2stat"
    )) {
      stop("Parameter 'category' has to have a length of 1 to 3.")
    }

    #----------------------------------------

    if (!(length(category) %in% c(1, 5)) && caller %in% c(
      "restatis::gen_find",
      "gen_find"
    )) {
      stop("Parameter 'category' must have a length of 1.")
    }

    #----------------------------------------

    if (length(category) != 1 && caller %in% c(
      "restatis::gen_metadata",
      "gen_metadata"
    )) {
      stop("Parameter 'category' must have a length of 1. Please specify the category.")
    }

    #---------------------------------------------------------------------------

    if (caller %in% c(
      "restatis::gen_catalogue", "restatis::gen_objects2var",
      "gen_catalogue", "gen_objects2var"
    )) {
      if (!all(category %in% c("tables", "cubes", "statistics"))) {
        stop("Available categories are tables, statistics, and cubes.",
          call. = FALSE
        )
      }
    }

    #----------------------------------------

    if (caller %in% c("restatis::gen_objects2stat", "gen_objects2stat")) {
      if (!all(category %in% c("tables", "cubes", "variables"))) {
        stop("Available categories are tables, variables, and cubes.",
          call. = FALSE
        )
      }
    }

    #----------------------------------------

    if (caller %in% c("restatis::gen_find", "gen_find")) {
      if (!all(category %in% c("all", "tables", "statistics", "variables", "cubes"))) {
        stop("Available categories are all, tables, statistics, variables, and cubes.",
          call. = FALSE
        )
      }
    }

    #----------------------------------------

    if (caller %in% c("restatis::gen_metadata", "gen_metadata")) {
      if (!all(category %in% c("Cube", "Statistic", "Table", "Variable", "Value"))) {
        stop("Available categories are Cube, Table, Statistic, Variable, and Value.",
          call. = FALSE
        )
      }
    }
  }


  # detailed ----

  if (!is.null(detailed)) {
    if (!is.logical(detailed) || length(detailed) != 1) {
      stop("Parameter 'detailed' has to be of type 'logical' and of length 1.",
        call. = FALSE
      )
    }

    if (isFALSE(detailed)) {
      message("Use 'detailed = TRUE' to obtain the complete output.")
    }
  }

  # type ----

  if (!is.null(type)) {
    if (!all(type %in% c("all", "tables", "statistics", "statisticsUpdates"))) {
      stop("Available categories for parameter 'type' are 'tables', 'statistics', 'statistic updates', and 'all'.",
        call. = FALSE
      )
    }
  }

  # date ----

  if (!is.null(date)) {
    if (identical(date, c("now", "week_before", "month_before", "year_before"))) {
      message("Please note that this date is calculated automatically and may differ
              from manually entered data. Manually entered data must have
              the format DD.MM.YYYY.")

      return("now")
    }

    if (!(length(date) %in% c(1, 4))) {
      stop("Parameter 'date' has to be of length 4 (c('now', 'week_before', 'month_before', 'year_before') for the default option of 'now' or of length 1.))",
        call. = FALSE
      )
    }

    if (length(date) == 1) {
      if (date %in% c("now", "week_before", "month_before", "year_before")) {
        message("Please note that this date is calculated automatically and may differ
                from manually entered data. Manually entered data must have
                the format DD.MM.YYYY.")

        return(date)
      }

      if (!(date %in% c("now", "week_before", "month_before", "year_before"))) {
        if (!is.character(date)) {
          stop("If using a specific date for parameter 'date', it has to be of type 'character' (format: DD.MM.YYYY).",
            call. = FALSE
          )
        }

        if (length(date) != 1 || nchar(date) != 10) {
          stop("If specifying a specific date for parameter 'date', it has to be of length 1 and format DD.MM.YYYY.",
            call. = FALSE
          )
        }

        return(date)
      }
    }
  }

  # similarity ----

  if (!is.null(similarity)) {
    if (!is.logical(similarity)) {
      stop("Parameter 'similarity' has to be of type 'logical'.",
        call. = FALSE
      )
    }
  }

  # error.ignore ----

  if (!is.null(error.ignore)) {
    if (length(error.ignore == 1)) {
      if (!is.logical(error.ignore) || length(error.ignore) != 1) {
        stop("Parameter 'error.ignore' has to be of type 'logical' and of length 1.",
          call. = FALSE
        )
      }
    }

    if (isTRUE(error.ignore)) {
      message("Use 'error.ignore = FALSE' to stop the function at the point where no object could be found.")
    }
  }

  # ordering ----

  if (!is.null(ordering)) {
    if (!is.logical(ordering) || length(ordering) != 1) {
      stop("Parameter 'ordering' has to be of type 'logical' and of length 1.",
        call. = FALSE
      )
    }

    if (isFALSE(ordering)) {
      message("Use 'ordering = TRUE' to obtain the output ordered based on the search term presence.")
    }
  }
}

#-------------------------------------------------------------------------------

# test_if_json ----

test_if_json <- function(input) {
  if ((httr2::resp_content_type(input) == "application/json") && !is.na(httr2::resp_content_type(input))) {
    results_json <- httr2::resp_body_json(input)
  } else {
    stop("No json-csv file detected.", call. = FALSE)
  }

  return(results_json)
}

#-------------------------------------------------------------------------------

# test_if_error_find ----

test_if_error_find <- function(input, para) {
  if (input$Status$Code != 0 && isTRUE(para)) {
    stop(input$Status$Content)
  } else if (input$Status$Code != 0 && isFALSE(para)) {
    message(input$Status$Content)

    message("Artificial token is used.")

    empty_object <- FALSE
  } else {
    empty_object <- "DONE"
  }

  return(empty_object)
}

#-------------------------------------------------------------------------------

# test_if_error ----

test_if_error <- function(input, para) {
  if (input$Status$Code == 104 && isFALSE(para)) {
    stop("No object found for your request. Check your parameters if you expected an object for this request.",
      call. = FALSE
    )
  } else if (input$Status$Code != 0 && isFALSE(para)) {
    stop(input$Status$Content, call. = FALSE)
  } else if (input$Status$Code == 104 && isTRUE(para)) {
    message("No object found for your request. Check your parameters if you expected an object for this request. Artificial token is used.")

    empty_object <- TRUE
  } else if (input$Status$Code != 0 && isTRUE(para)) {
    message(input$Status$Content)

    message("Artificial token is used.")

    empty_object <- FALSE
  } else {
    empty_object <- "DONE"
  }

  return(empty_object)
}

#-------------------------------------------------------------------------------

# test_if_process_further ----

test_if_process_further <- function(input, para) {
  if (sum(unlist(lapply(input[4:8], function(x) {
    is.null(x)
  }))) == 5 && isFALSE(para)) {
    stop("No object found for your request. Check your parameters if you expected an object for this request.")
  } else if (sum(unlist(lapply(input[4:8], function(x) {
    is.null(x)
  }))) == 5 && isTRUE(para)) {
    message("No object found for your request. Check your parameters if you expected an object for this request. Artificial token is used.")

    empty_object <- TRUE
  } else {
    empty_object <- "DONE"
  }

  return(empty_object)
}

#-------------------------------------------------------------------------------

# binding_lapply ----

binding_lapply <- function(x,
                           characteristics) {
  list_of <- stats::setNames(data.frame(matrix(ncol = length(characteristics), nrow = 0)), characteristics)

  lapply(x, function(x) {
    zwisch <- unlist(x[characteristics])

    list_of <<- rbind(list_of, zwisch[characteristics])
  })

  colnames(list_of) <- characteristics

  return(list_of)
}

#-------------------------------------------------------------------------------

# gsub ----

ggsub <- function(x) {
  a <- gsub(".*:", "", x$Content)

  return(a)
}

#-------------------------------------------------------------------------------

# spezifisch_create ----

spezifisch_create <- function(x) {
  a <- unlist(lapply(strsplit(x$Spezifisch, ","), length))

  return(a)
}

#-------------------------------------------------------------------------------

# titel_search ----

titel_search <- function(x, term) {
  split <- unlist(strsplit(gsub(" ", "und", term), c("und|UND|Und|\\&|ODER|oder|Oder|\\|")))

  split <- split[sapply(split, function(y) {
    nchar(y) > 0
  })]

  if (length(split) == 1) {
    a <- grepl(split, x$Content, ignore.case = TRUE)
  } else if (grep("ODER|oder|Oder|\\|", term, ignore.case = TRUE) && grep("UND|und|Und|\\|", term, ignore.case = TRUE)) {
    a <- rep(FALSE, length(x$Content))
    message("Combination of words too complex for ordering. Data is processed without ordering.")
  } else if (grep("ODER|oder|Oder|\\|", term, ignore.case = TRUE)) {
    a <- grepl(paste(split, collapse = "|"), x$Content, ignore.case = TRUE)
  } else if (grep("UND|und|Und|\\|", term, ignore.case = TRUE)) {
    a <- sapply(x$Content, function(con) {
      all(sapply(split, function(z) {
        grepl(z, con, ignore.case = TRUE)
      }))
    })
  } else {
    a <- rep(FALSE, length(x$Content))

    message("Combination of words not valid for ordering. Data is processed without ordering.")
  }

  return(a)
}


#-------------------------------------------------------------------------------

# test_if_error_light ----

test_if_error_light <- function(input) {
  if (input$Status$Code != 0) {
    warning(input$Status$Content, call. = FALSE)
  }
}
