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

  #-----------------------------------------------------------------------------

  if (is.null(code)) {

    stop("Parameter `code` must NOT be NULL.",
         call. = FALSE)

  }

  if (!is.null(code)) {

    if (length(code) != 1L) {

      stop("Parameter 'code' must be a single string.",
           call. = FALSE)

    }

    if (!is.character(code)) {

      stop("Parameter 'code' has to be of type 'character'.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------

  if (is.null(term)) {

    stop("Parameter `term` must NOT be NULL.",
         call. = FALSE)

  }

  if(!is.null(term)) {

    if (length(term) != 1L) {

      stop("Parameter 'term' must be a single string.",
           call. = FALSE)

    }

    if (!is.character(term)) {

      stop("Parameter 'term' has to be of type 'character'.",
           call. = FALSE)

    }

    if (nchar(term) > 15 & caller != "restatis::gen_find") {

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

    if(length(sortcriterion) == 1) {

      if(!(sortcriterion %in% c("code", "content"))) {

        stop("Parameter 'sortcriterion' has to be 'code' or 'content'.",
             call. = FALSE)

      }

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

      stop("Parameter 'detailed' has to be of type 'logical' and of length 1.",
           call. = FALSE)

    }

    if(isFALSE(detailed)) {

      message("Use 'detailed = TRUE' to obtain the complete output.")

    }

  }

  #---------------------------------------------------------------------------

  if(!is.null(error.ignore)) {

    if(length(error.ignore == 1)) {

      if(!is.logical(error.ignore) | length(error.ignore) != 1) {

        stop("Parameter 'error.ignore' has to be of type 'logical' and of length 1.",
             call. = FALSE)

      }

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

    if(caller %in% c("restatis::gen_catalogue", "restatis::gen_objects2var")) {

      if(!all(category %in% c("tables", "cubes", "statistics"))) {

        stop("Available categories are tables, statistics, and cubes.",
             call. = FALSE)

      }

    }

    #----------------------------------------

    if(caller == "restatis::gen_objects2stat") {

      if(!all(category %in% c("tables", "cubes", "variables"))) {

        stop("Available categories are tables, variables, and cubes.",
             call. = FALSE)

      }

    }

    #----------------------------------------

    if(caller == "restatis::gen_find") {

      if(!all(category %in% c("all", "tables", "statistics", "variables", "cubes"))) {

        stop("Available categories are all, tables, statistics, variables, and cubes.",
             call. = FALSE)

      }

    }

    #----------------------------------------

    if(caller == "restatis::gen_meta_data") {

      if(!all(category %in% c("Cube", "Statistic", "Table", "Variable", "Value"))) {

        stop("Available categories are Cube, Table, Statistic, Variable, and Value.",
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

  if(!is.null(ordering)) {

    if(!is.logical(ordering) | length(ordering) != 1) {

      stop("Parameter 'ordering' has to be of type 'logical' and of length 1.",
           call. = FALSE)

    }

    if(isFALSE(ordering)) {

      message("Use 'ordering = TRUE' to obtain the output in a ordered way based on the search term presence.")

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

# --------------------------------------- #

test_if_json <- function(input){

  if (httr2::resp_content_type(input) == "application/json") {

    results_json <- httr2::resp_body_json(input)

  } else {

    stop("No json-csv file detected.", call. = FALSE)

  }

  return(results_json)

}

# --------------------------------------- #

test_if_error_find <- function(input, para){
  if (input$Status$Code != 0 & isTRUE(para)) {
    stop(input$Status$Content)
  } else if (input$Status$Code != 0 & isFALSE(para)){
    message(input$Status$Content)
    message("Artificial token is used.")
    empty_object <- FALSE
  } else {
    empty_object <- "DONE"
  }
  return(empty_object)
}

# --------------------------------------- #

test_if_process_further <- function(input, para){
  if (sum(unlist(lapply(input[4:8], function(x) {
    is.null(x)
  }))) == 5 & isFALSE(para)) {
    stop("No object found for your request. Check your parameters if you expected an object for this request.")
  } else if (sum(unlist(lapply(input[4:8], function(x) {
    is.null(x)
  }))) == 5 & isTRUE(para)){
    message("No object found for your request. Check your parameters if you expected an object for this request. Artificial token is used.")
    empty_object <- TRUE
  }
  return(empty_object)
}

# --------------------------------------- #

binding_lapply <- function(x,
                           characteristics){
  list_of <- stats::setNames(data.frame(matrix(ncol = length(characteristics), nrow = 0)), characteristics)

  lapply(x, function(x){

    zwisch <- unlist(x[characteristics])

    list_of <<- rbind(list_of, zwisch[characteristics])

  })

  colnames(list_of) <- characteristics

  return(list_of)
}
# --------------------------------------- #

# gsub function ####
ggsub <- function(x){
  a <- gsub(".*:", "", x$Content)
  return(a)
}

# --------------------------------------- #

# spezifisch function ####
spezifisch_create <- function(x){
  a <- unlist(lapply(strsplit(x$Spezifisch, ","), length))
  return(a)
}

# --------------------------------------- #

# Titel function ####
titel_search <- function(x, term){
  a <- grepl(
    paste(
      unlist(
        strsplit(term,
                 c(" & | und "))),
      collapse = "|"),
    x$Content,
    ignore.case = T
  )

  return(a)
}

# --------------------------------------- #

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
}


# -------------------------------------------------------------------------- #
test_if_json <- function(input){
  if (httr2::resp_content_type(input) == "application/json") {

    results_json <- httr2::resp_body_json(input)

  } else {
    stop("No json-csv file detected")
  }

  return(results_json)
}

# error response ####
test_if_error <- function(input, para){
  if (input$Status$Code != 0 & isTRUE(para)) {
    stop(input$Status$Content)
  } else if (input$Status$Code != 0 & isFALSE(para)){
    message(input$Status$Content)
    message("Artificial token is used.")
    empty_object <- FALSE
  } else {
    empty_object <- "DONE"
  }
  return(empty_object)
}


# process further ####
test_if_process_further <- function(input, para){
  if (sum(unlist(lapply(input[4:8], function(x) {
    is.null(x)
  }))) == 5 & isFALSE(para)) {
    stop("No object found for your request. Check your parameters if you expected an object for this request.")
  } else if (sum(unlist(lapply(input[4:8], function(x) {
    is.null(x)
  }))) == 5 & isTRUE(para)){
    message("No object found for your request. Check your parameters if you expected an object for this request. Artificial token is used.")
    empty_object <- TRUE
  }
  return(empty_object)
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

# gsub function ####
ggsub <- function(x){
  a <- gsub(".*:", "", x$Content)
  return(a)
}

# spezifisch function ####
spezifisch_create <- function(x){
  a <- unlist(lapply(strsplit(x$Spezifisch, ","), length))
  return(a)
}

# Titel function ####
titel_search <- function(x, term){
  a <- grepl(
    paste(
      unlist(
        strsplit(term,
                 c(" & | und "))),
      collapse = "|"),
    x$Content,
    ignore.case = T
  )

  return(a)
}


# test if error meta ___ identical to test_if_error from cataloge
test_if_error_meta <- function(input, para){

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


