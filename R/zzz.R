# .onLoad <- function(libname, pkgname) {
#   gen_api <<- memoise::memoise(gen_api)
# }

forming_evas <- function(list_of){
  list_of$Main <- apply(list_of, 1, function(x){
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 1)]
  })

  list_of$Main2 <- apply(list_of, 1, function(x){
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 2)]
  })

  list_of$Main3 <- apply(list_of, 1, function(x){
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 3)]
  })

  list_of$Main5 <- apply(list_of, 1, function(x){
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 5)]
  })

  nestedlist <- split(list_of, list_of$Main, drop = TRUE)
  nestedlist <- lapply(nestedlist, function(x){
    split(x, x["Main2"], drop = TRUE)
  })
  nestedlist <- lapply(nestedlist, function(x){
    lapply(x, function(y){
      split(y, y["Main3"])
    })})
  nestedlist <- lapply(nestedlist, function(x){
    lapply(x, function(y){
      lapply(y, function(z){
        split(z, z["Main5"])
      })
    })
  })

  aba <- lapply(
    nestedlist, function(d){
      lapply(d,  function(z){
        lapply(z,  function(y){
          lapply(y,  function(x){
            x[!(
              names(x)
              %in% c("Main", "Main2", "Main3", "Main5"))]})
        })
      })
    })

  return(aba)
}

#-------------------------------------------------------------------------------

check_function_input <- function(code = NULL,
                                 term = NULL,
                                 sortcriterion = NULL,
                                 category = NULL,
                                 detailed = NULL,
                                 type = NULL,
                                 date = NULL,
                                 similarity = NULL,
                                 error.ignore = NULL,
                                 caller = NULL) {

  #-----------------------------------------------------------------------------

  if (!is.null(code)) {

    if (length(code) != 1L) {

      stop("Parameter 'code' must be a single string.",
           call. = FALSE)

    }

    if (!is.null(code) & !is.character(code)) {

      stop("Parameter 'code' has to be of type 'character' or NULL.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------

  if(!is.null(term)) {

    if (length(term) != 1L) {

      stop("Parameter 'term' must be a single string.",
           call. = FALSE)

    }

    if (!is.null(term) & !is.character(term)) {

      stop("Parameter 'term' has to be of type 'character' or NULL.",
           call. = FALSE)

    }

    if (nchar(term) > 15) {

      stop("Parameter 'term' cannot consist of more than 15 characters.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------

  if(!is.null(sortcriterion)) {

    if(!is.character(sortcriterion)) {

      stop("Parameter 'sortcriterion' has to be of type 'character'.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------

  if(!is.null(similarity)) {

    if(!is.logical(similarity)) {

      stop("Parameter 'similarity' has to be of type 'logical'.",
           call. = FALSE)

    }

  }

  #---------------------------------------------------------------------------

  if(!is.null(detailed)) {

    if(!is.logical(detailed) | length(detailed) != 1) {

      stop("Paramter 'detailed' has to be of type 'logical' and of length 1.",
           call. = FALSE)

    }

    if(isFALSE(detailed)) {

      message("Use 'detailed = TRUE' to obtain the complete output.")

    }

  }

  #---------------------------------------------------------------------------

  if(!is.null(error.ignore)) {

    if(!is.logical(error.ignore) | length(error.ignore) != 1) {

      stop("Parameter 'error.ignore' has to be of type 'logical' and of length 1.",
           call. = FALSE)

    }

    if(isTRUE(error.ignore)) {

      message("Use 'error.ignore = FALSE' to stop the function at the point where no object could be found.")

    }

  }

  #-----------------------------------------------------------------------------

  if (!is.null(category)) {

    if (!(length(category) %in% c(1:3))) {

      stop("Parameter 'category' has to have a length of 1 to 3.")

    }

    #---------------------------------------------------------------------------

      if(caller %in% c("restatis::catalogue", "restatis::xy_to_variable")) {

        if(!all(category %in% c("tables", "cubes", "statistics"))) {

          stop("Available categories are tables, statistics, and cubes.",
               call. = FALSE)

        }

      }

      #----------------------------------------

      if(caller == "restatis::xy_to_statistics") {

        if(!all(category %in% c("tables", "cubes", "variables"))) {

          stop("Available categories are tables, variables, and cubes.",
               call. = FALSE)

      }

    }

  }

  #-----------------------------------------------------------------------------

  if(!is.null(type)) {

    if(!all(type %in% c("all", "tables", "statistics", "statisticsUpdates"))) {

      stop("Available categories for parameter 'type' are 'tables', 'statistics', 'statistic updates', and 'all'.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------

  if(!is.null(date)) {

    if (identical(date, c("now", "week_before", "month_before", "year_before"))) {

      message("Please note that this date is calculated automatically and may differ
              from manually entered data. Manually entered data must have
              the format DD.MM.YYYY.")

      return("now")

    }

    if (!(length(date) %in% c(1, 4))) {

      stop("Parameter 'date' has to be of length 4 (c('now', 'week_before', 'month_before', 'year_before') for the default option of 'now' or of length 1.))",
           call. = FALSE)

    }

    if (length(date) == 1) {

      if (date %in% c("now", "week_before", "month_before", "year_before")) {

        message("Please note that this date is calculated automatically and may differ
                from manually entered data. Manually entered data must have
                the format DD.MM.YYYY.")

        return(date)

      }

      if(!(date %in% c("now", "week_before", "month_before", "year_before"))) {

        if(!is.character(date)) {

          stop("If using a specific date for parameter 'date', it has to be of type 'character' (format: DD.MM.YYYY).",
               call. =  FALSE)

        }

        if(length(date) != 1 | nchar(date) != 10) {

          stop("If specifying a specific date for parameter 'date', it has to be of length 1 and format DD.MM.YYYY.",
               call. = FALSE)

        }

        return(date)

      }

    }

  }

}


# (ZK): Ideas for refactoring functions ####
# json response ####
test_if_json <- function(input){

  if (httr2::resp_content_type(input) == "application/json") {

    results_json <- httr2::resp_body_json(input)

  } else {

    stop("No json-csv file detected.", call. = FALSE)

  }

  return(results_json)

}

# error response ####
test_if_error <- function(input, para){

  if (input$Status$Code == 104 & isFALSE(para)){

    stop("No object found for your request. Check your parameters if you expected an object for this request.")

  } else if (input$Status$Code != 0 & isFALSE(para)){

    stop(input$Status$Content)

  } else if (input$Status$Code == 104 & isTRUE(para)){

    message("No object found for your request. Check your parameters if you expected an object for this request. Artificial token is used.")

    empty_object <- TRUE

  } else if (input$Status$Code != 0 & isTRUE(para)){

    message(input$Status$Content)

    message("Artificial token is used.")

    empty_object <- FALSE

  } else {

    empty_object <- "DONE"

  }

  return(empty_object)

}

# error response light ####
test_if_error_light <- function(input){

  if (input$Status$Code != 0) {

    warning(input$Status$Content, call. = FALSE)

  }

}

# binding_function ####
binding_lapply <- function(x,
                           characteristics){
  list_of <- setNames(data.frame(matrix(ncol = length(characteristics), nrow = 0)), characteristics)

  lapply(x, function(x){

    zwisch <- unlist(x[characteristics])

    list_of <<- rbind(list_of, zwisch[characteristics])

  })

  colnames(list_of) <- characteristics

  return(list_of)
}

# bind_rows ####
# map_dfr ####
# It seems that the current version and the older version are not significantly different in execution time (seconds focused)
