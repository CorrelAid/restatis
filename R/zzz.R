.onLoad <- function(libname, pkgname) {
  gen_api <<- memoise::memoise(gen_api)
}

# (YAB): The list extraction that happens in the following function
# might be eligible for refactoring into a smaller function to avoid
# repetition
# Also, why is "Main4" missing?

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

  #-----------------------------------------------------------------------------

  if(!is.null(category) && (caller %in% c("restatis::catalogue", "restatis::xy_to_variable"))) {

    if(!all(category %in% c("tables", "cubes", "statistics"))) {

      stop("Available categories are tables, statistics, and cubes.",
           call. = FALSE)

    }

  }

  #----------------------------------------

  if(!is.null(category) && (caller == "restatis::xy_to_statistics")) {

    if(!all(category %in% c("tables", "cubes", "statistics"))) {

      stop("Available categories are tables, statistics, and cubes.",
           call. = FALSE)

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

    if (date %in% c("now", "week_before", "month_before", "year_before")) {

      message("Please note that this date is calculated automatically and may differ
              from manually entered data. Manually entered data must have
              the format DD.MM.YYYY.")

    }

    if(!(date %in% c("now", "week_before", "month_before", "year_before"))) {

      if(!is.character(date)) {

        stop("If using a specific date for parameter 'date', it has to be of type 'character'.",
             call. =  FALSE)

      }

      if(length(date) != 1 | nchar(date) != 10) {

        stop("If specifying a specific date for parameter 'date', it has to be of length 1 and format DD.MM.YYYY.",
             call. = FALSE)

      }


    }

  }

}











