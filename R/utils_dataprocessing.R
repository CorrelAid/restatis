#-------------------------------------------------------------------------------
# Util functions related to data processing
#-------------------------------------------------------------------------------

#' param_check_year
#'
#' @param year Year as parameter value
#'
param_check_year <- function(year) {

  if (as.integer(year) < 1900 || as.integer(year) > 2100)  {

    stop("The parameter 'year' has been misspecified (>= 1900 or <= 2100).",
         call. = FALSE)

  }

}

#-------------------------------------------------------------------------------

#' param_check_regionalkey
#'
#' @param regionalkey Regional key
#'
param_check_regionalkey <- function(regionalkey) {

  if (!is.null(regionalkey) & !is.character(regionalkey)) {

    stop("The parameter 'regionalkey' needs to be of type 'character'.",
         call. = FALSE)

  }

}

#-------------------------------------------------------------------------------

#' param_collapse_vec
#'
#' @param vec Vector to be collapsed
#'
param_collapse_vec <- function(vec) {

  paste0(vec, collapse = ",")

}

#-------------------------------------------------------------------------------

#' forming_evas
#'
#' @param list_of List of EVAS to iterate over
#'
forming_evas <- function(list_of) {

  # Path selection
  data_path <- system.file("data", "EVAS_numbers.RData", package = "restatis")

  # Load data
  load(data_path)

  #-----------------------------------------------------------------------------
  # Process them

  list_of$Main <- apply(list_of, 1, function(x) {

    obj <- evas_list$Titel[evas_list$EVAS == substr(x["Code"], 1, 1)]

    if(length(obj) == 0){

      obj <- "No assignment"

    }

    return(obj)

  })

  #-----------------------------------------------------------------------------

  list_of$Main2 <- apply(list_of, 1, function(x) {

    obj <- evas_list$Titel[evas_list$EVAS == substr(x["Code"], 1, 2)]

    if(length(obj) == 0){

      obj <- "No assignment"

    }

    return(obj)

  })

  #-----------------------------------------------------------------------------

  list_of$Main3 <- apply(list_of, 1, function(x) {

    obj <- evas_list$Titel[evas_list$EVAS == substr(x["Code"], 1, 3)]

    if(length(obj) == 0){

      obj <- "No assignment"

    }

    return(obj)

  })

  #-----------------------------------------------------------------------------

  list_of$Main5 <- apply(list_of, 1, function(x) {

    obj <- evas_list$Titel[evas_list$EVAS == substr(x["Code"], 1, 5)]

    if(length(obj) == 0){

      obj <- "No assignment"

    }

    return(obj)

  })

  #-----------------------------------------------------------------------------

  keep <- colnames(list_of[,1:(ncol(list_of) - 4)])

  #-----------------------------------------------------------------------------
  if(sum(list_of$Main == "No assignment") != nrow(list_of)){

    nestedlist <- split(list_of, list_of$Main, drop = TRUE)

    #---------------------------------------------------------------------------
    if(sum(list_of$Main2 == "No assignment") != nrow(list_of)){

      nestedlist <- lapply(nestedlist, function(x) {

        obj <- split(x, x["Main2"], drop = TRUE)

      })
      #-------------------------------------------------------------------------
      if(sum(list_of$Main3 == "No assignment") != nrow(list_of)){

        nestedlist <- lapply(nestedlist, function(x) {

          lapply(x, function(y) {

            obj <- split(y, y["Main3"])

          })

        })

        #-----------------------------------------------------------------------
        if(sum(list_of$Main5 == "No assignment") != nrow(list_of)){

          nestedlist <- lapply(nestedlist, function(x) {

            lapply(x, function(y) {

              lapply(y, function(z) {

                obj <- split(z, z["Main5"])

                return(obj)

              })

            })

          })

          #---------------------------------------------------------------------

          nestedlist <- lapply(nestedlist, function(d){

            lapply(d, function(y){

              lapply(y, function(x){

                lapply(x, function(r, remain){

                  obj <- r[keep]
                  obj <- tibble::as_tibble(obj)

                },
              remain = keep)})

            })

          })

        #-----------------------------------------------------------------------
        } else {

          nestedlist <- lapply(nestedlist, function(d){

            lapply(d, function(y){

              lapply(y, function(r, remain){

                obj <- r[keep]
                obj <- tibble::as_tibble(obj)

                },
              remain = keep

            )}
          )}
        )}

      #-------------------------------------------------------------------------
      } else {

        nestedlist <- lapply(nestedlist, function(d){

          lapply(d, function(r, remain){

            obj <- r[keep]
            obj <- tibble::as_tibble(obj)
            },
          remain = keep

        )}

      )}

    #---------------------------------------------------------------------------
    } else {

      nestedlist <- lapply(nestedlist, function(r, remain){

        obj <- r[keep]
        obj <- tibble::as_tibble(obj)

        },
      remain = keep)

    }

  } else {

    nestedlist <- tibble::as_tibble(list_of[keep])

  }

  return(nestedlist)

}

#-------------------------------------------------------------------------------

#' check_function_input
#'
#' @param code Parameter to be checked
#' @param term Parameter to be checked
#' @param sortcriterion Parameter to be checked
#' @param category Parameter to be checked
#' @param detailed Parameter to be checked
#' @param type Parameter to be checked
#' @param date Parameter to be checked
#' @param similarity Parameter to be checked
#' @param error.ignore Parameter to be checked
#' @param ordering Parameter to be checked
#' @param database Parameter to be checked
#' @param caller Parameter to be checked
#'
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
                                 database = NULL,
                                 area = NULL,
                                 caller = NULL) {

  #-----------------------------------------------------------------------------
  # Code & Term ----

  if (is.null(code) && is.null(term)) {

    if (!(caller %in% c("gen_search_vars", "restatis::gen_search_vars"))) {

      stop("Parameter 'code' or 'term' must NOT be NULL.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------
  # Code ----

  if (!is.null(code)) {

    if (length(code) != 1L) {

      stop("Parameter 'code' must be a single string.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (!is.character(code)) {

      stop("Parameter 'code' has to be of type 'character'.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------
  # Term ----

  if (!is.null(term)) {

    if (length(term) != 1L) {

      stop("Parameter 'term' must be a single string.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (!is.character(term)) {

      stop("Parameter 'term' has to be of type 'character'.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (nchar(term) > 15 && !(caller %in% c("gen_find", "restatis::gen_find"))) {

      stop("Parameter 'term' cannot consist of more than 15 characters.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------
  # sortcriterion ----

  if (!is.null(sortcriterion)) {

    if (!is.character(sortcriterion)) {

      stop("Parameter 'sortcriterion' has to be of type 'character'.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (length(sortcriterion) == 1) {

      if (!(sortcriterion %in% c("code", "content"))) {

        stop("Parameter 'sortcriterion' has to be 'code' or 'content'.",
             call. = FALSE)

      }

    }

  }

  #-----------------------------------------------------------------------------
  # category ----

  if (!is.null(category)) {

    if (!(length(category) %in% c(1:3)) &&
        caller %in% c("restatis::gen_catalogue",
                      "restatis::gen_objects2var",
                      "restatis::gen_objects2stat",
                      "gen_catalogue",
                      "gen_objects2var",
                      "gen_objects2stat")) {

      stop("Parameter 'category' has to have a length of 1 to 3.",
           call. = FALSE)

      }

    #---------------------------------------------------------------------------

    if (!(length(category) %in% c(1, 5)) &&
        caller %in% c("restatis::gen_find", "gen_find")) {

      stop("Parameter 'category' must have a length of 1.",
           call. = FALSE)

    }

    #----------------------------------------

    if (length(category) != 1 &&
        caller %in% c("restatis::gen_metadata", "gen_metadata")) {

      stop("Parameter 'category' must have a length of 1. Please specify the category.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (caller %in% c("restatis::gen_catalogue",
                      "restatis::gen_objects2var",
                      "gen_catalogue",
                      "gen_objects2var")) {

      #-------------------------------------------------------------------------

      if("gen_zensus_api" %in% database){

        #-----------------------------------------------------------------------

        if (!all(category %in% c("tables", "cubes", "statistics"))) {

          stop("Available categories are 'tables' and 'statistics'.",
               call. = FALSE)

        }

        #-----------------------------------------------------------------------

        if (length(category) == 1 &&
            "cubes" %in% category &&
            isFALSE(error.ignore)) {

          stop("Available categories for 'zensus'-database are: 'tables' and 'statistics'.",
               call. = FALSE)

        }

        #-----------------------------------------------------------------------

        else if (length(category) == 1 &&
                 "cubes" %in% category &&
                 isTRUE(error.ignore)) {

          warning("Available categories for 'zensus'-database are: 'tables' and 'statistics'.
                  Function is continued with a placeholder for the 'cubes' output.",
                  call. = FALSE)

        }

        else if ("cubes" %in% category &&
                 isFALSE(error.ignore)) {

          warning("Available categories for 'zensus'-database are: 'tables' and 'statistics'.",
                  call. = FALSE)

        }

        else if ("cubes" %in% category &&
                 isTRUE(error.ignore)) {

          warning("Available categories for 'zensus'-database are: 'tables' and 'statistics'.
                  Function is continued with specified 'category'-parameter excluding 'cubes'.",
                  call. = FALSE)

        }

      }

    #-------------------------------------------------------------------------------

      if("gen_api" %in% database){

        if (!all(category %in% c("tables", "cubes", "statistics"))) {

          stop("Available categories are 'tables', 'statistics', and 'cubes'.",
               call. = FALSE)

        }

      }

    }

    #----------------------------------------

    if (caller %in% c("restatis::gen_objects2stat",
                      "gen_objects2stat")) {

      #-------------------------------------------------------------------------

      if("gen_zensus_api" %in% database){

        #-----------------------------------------------------------------------

        if (!all(category %in% c("tables", "cubes", "variables"))) {

          stop("Available categories are 'tables' and 'variables'.",
               call. = FALSE)

        }

        #-----------------------------------------------------------------------

        if (length(category) == 1 &&
            "cubes" %in% category &&
            isFALSE(error.ignore)) {

          stop("Available categories for 'zensus'-database are: 'tables' and 'variables'.",
               call. = FALSE)

        }

        #-----------------------------------------------------------------------

        else if (length(category) == 1 &&
                 "cubes" %in% category && isTRUE(error.ignore)) {

          warning("Available categories for 'zensus'-database are: 'tables' and 'variables'.
                  Function is continued with a placeholder for the 'cubes' output.",
                  call. = FALSE)

        }

        #-----------------------------------------------------------------------

        else if ("cubes" %in% category &&
                 isFALSE(error.ignore)) {

          warning("Available categories for 'zensus'-database are: 'tables' and 'variables'.",
                  call. = FALSE)

        }

        #-----------------------------------------------------------------------

        else if ("cubes" %in% category &&
                 isTRUE(error.ignore)) {

          warning("Available categories for 'zensus'-database are: 'tables' and 'variables'.
                  Function is continued with specified 'category'-parameter excluding 'cubes'.",
                  call. = FALSE)

        }

      }

      #-------------------------------------------------------------------------

      if("gen_api" %in% database){

        if (!all(category %in% c("tables", "cubes", "variables"))) {

          stop("Available categories are 'tables', 'variables', and 'cubes'.",
               call. = FALSE)

        }

      }

    }

    #----------------------------------------

    if (caller %in% c("restatis::gen_find", "gen_find")) {

      #-------------------------------------------------------------------------

        if (!all(category %in% c("all", "tables", "statistics", "variables", "cubes"))) {

          #---------------------------------------------------------------------

          if("gen_api" %in% database){

            stop("Available categories for parameter 'category' for 'genesis'-database are 'all', 'tables', 'statistics', 'variables', and 'cubes'.",
                 call. = FALSE)

          }

          #---------------------------------------------------------------------

          if("gen_zensus_api" %in% database){

            stop("Available categories for parameter 'category' for 'zensus'-database are 'all', 'tables', 'statistics', and 'variables'.",
                 call. = FALSE)

        }

      }

    }

    #---------------------------------------------------------------------------

    if (caller %in% c("restatis::gen_find", "gen_find")) {

      #-------------------------------------------------------------------------

      if("gen_zensus_api" %in% database){

        #-----------------------------------------------------------------------

        if ("cubes" %in% category &&
            isTRUE(error.ignore)) {

          warning("Available categories for 'zensus'-database are: 'all', 'tables', 'statistics', and 'variables'.
                  Function is continued with a placeholder for the 'cubes' output.",
                  call. = FALSE)

        }

        #-----------------------------------------------------------------------

        else if ("all" %in% category) {

          warning("Available categories for 'zensus'-database are: 'all', 'tables', 'statistics', and 'variables'.
                  Function is continued with a placeholder for the 'cubes' output.",
                  call. = FALSE)

        }

        #-----------------------------------------------------------------------

        else if ("cubes" %in% category &&
                 isFALSE(error.ignore)) {

          stop("Available categories are 'all', 'tables', 'statistics', and 'variables'.",
               call. = FALSE)

        }

      }

    }

    #---------------------------------------------------------------------------

    if (caller %in% c("restatis::gen_metadata", "gen_metadata")) {

      #-------------------------------------------------------------------------

      if("gen_api" %in% database){

        #-----------------------------------------------------------------------

        if (!all(category %in% c("cube", "statistic", "table", "variable", "value"))) {

            stop("Available categories for parameter 'category' for 'genesis'-database are 'cube', 'table', 'statistic', 'variable', and 'value'.",
                  call. = FALSE)

          }

        }

      #-------------------------------------------------------------------------

      else if("gen_zensus_api" %in% database) {

        if (!all(category %in% c("statistic", "table", "variable", "value"))) {

          stop("Available categories for parameter 'category' for 'zensus'-database are 'table', 'statistic', 'variable', and 'value'.",
               call. = FALSE)

        }

      }

    }

  }


  #-----------------------------------------------------------------------------
  # detailed ----

  if (!is.null(detailed)) {

    if (!is.logical(detailed) ||
        length(detailed) != 1) {

      stop("Parameter 'detailed' has to be of type 'logical' and of length 1.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (isFALSE(detailed)) {

      message("Use 'detailed = TRUE' to obtain the complete output.")

    }

  }

  #-------------------------------------------------------------------------------
  # type ----

  if (!is.null(type)) {

    #---------------------------------------------------------------------------

    if ("gen_api" %in% database){

      #-------------------------------------------------------------------------

      if (!all(type %in% c("all", "tables", "statistics", "statisticsUpdates"))) {

        stop("Available categories for parameter 'type' for 'genesis'-database are 'tables', 'statistics', 'statistic updates', and 'all'.",
             call. = FALSE)

      }

    }

    #---------------------------------------------------------------------------

    if ("gen_zensus_api" %in% database){

      if (!all(type %in% c("all", "tables", "statistics"))) {

        stop("Available categories for parameter 'type' for 'zensus'-database are 'tables', 'statistics', and 'all'.",
             call. = FALSE)

      }

    }

  }

  #-----------------------------------------------------------------------------
  # date ----

  if (!is.null(date)) {

    #---------------------------------------------------------------------------

    if (identical(date, c("now", "week_before", "month_before", "year_before"))) {

      message("Please note that this date is calculated automatically and may differ
              from manually entered data. Manually entered data must have
              the format DD.MM.YYYY.")

      return("now")

    }

    #---------------------------------------------------------------------------

    if (!(length(date) %in% c(1, 4))) {

      stop("Parameter 'date' has to be of length 4 (c('now', 'week_before', 'month_before', 'year_before') for the default option of 'now' or of length 1.))",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (length(date) == 1) {

      #-------------------------------------------------------------------------

      if (date %in% c("now", "week_before", "month_before", "year_before")) {

        message("Please note that this date is calculated automatically and may differ
                from manually entered data. Manually entered data must have
                the format DD.MM.YYYY.")

        return(date)

      }

      #-------------------------------------------------------------------------

      if (!(date %in% c("now", "week_before", "month_before", "year_before"))) {

        #-----------------------------------------------------------------------

        if (!is.character(date)) {

          stop("If using a specific date for parameter 'date', it has to be of type 'character' (format: DD.MM.YYYY).",
               call. = FALSE)

        }

        #-----------------------------------------------------------------------

        if (length(date) != 1 ||
            nchar(date) != 10) {

          stop("If specifying a specific date for parameter 'date', it has to be of length 1 and format DD.MM.YYYY.",
               call. = FALSE)

        }

        return(date)

      }

    }

  }

  #-----------------------------------------------------------------------------
  # similarity ----

  if (!is.null(similarity)) {

    if (!is.logical(similarity)) {

      stop("Parameter 'similarity' has to be of type 'logical'.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------
  # error.ignore ----

  if (!is.null(error.ignore)) {

    #---------------------------------------------------------------------------

    if (length(error.ignore == 1)) {

      #-------------------------------------------------------------------------

      if (!is.logical(error.ignore) ||
          length(error.ignore) != 1) {

        stop("Parameter 'error.ignore' has to be of type 'logical' and of length 1.",
             call. = FALSE)

      }

    }

    #---------------------------------------------------------------------------

    if (isTRUE(error.ignore)) {

      message("Use 'error.ignore = FALSE' to stop the function at the point where no object could be found.")

    }

  }

  #-------------------------------------------------------------------------------
  # ordering ----

  if (!is.null(ordering)) {

    if (!is.logical(ordering) ||
        length(ordering) != 1) {

      stop("Parameter 'ordering' has to be of type 'logical' and of length 1.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (isFALSE(ordering)) {

      message("Use 'ordering = TRUE' to obtain the output ordered based on the search term presence.")

    }

  }

  #-----------------------------------------------------------------------------
  # area ----
  if (!is.null(area)) {

    if (!is.character(area) ||
        length(area) != 1) {

      stop("Parameter 'area' has to be of type 'character' and of length 1.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (!area %in% c("all", "public", "user")) {

      stop("Available categories for parameter 'area' are 'all', 'public', and 'user'.")

    }

  }

}

#-------------------------------------------------------------------------------

#' binding_lapply
#'
#' @param x Element to bind
#' @param characteristics Characteristics to filter for
#'
binding_lapply <- function(x,
                           characteristics) {

  list_of <- stats::setNames(data.frame(matrix(ncol = length(characteristics),
                                               nrow = 0)),
                             characteristics)

  lapply(x, function(x) {

    zwisch <- unlist(x[characteristics])

    list_of <<- rbind(list_of, zwisch[characteristics])

  })

  colnames(list_of) <- characteristics

  return(list_of)

}

#-------------------------------------------------------------------------------

#' ggsub
#'
#' @param x Element to subset with $Content
#'
ggsub <- function(x) {

  a <- gsub(".*:", "", x$Content)

  return(a)

}

#-------------------------------------------------------------------------------

#' spezifisch_create
#'
#' @param x Element to extract $Spezifisch from
#'
spezifisch_create <- function(x) {

  a <- unlist(lapply(strsplit(x$Spezifisch, ","), length))

  return(a)

}

#-------------------------------------------------------------------------------

#' titel_search
#'
#' @param x Element to extract $Content from
#' @param term Search term
#'
titel_search <- function(x, term) {

  split <- unlist(strsplit(gsub(" ", "und", term), c("und|UND|Und|\\&|ODER|oder|Oder|\\|")))

  split <- split[sapply(split, function(y) {

    nchar(y) > 0

  })]

  #-----------------------------------------------------------------------------

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
#' test_database_function
#'
#' @param input Input to test for database name
#'
test_database_function <- function(input){

  if(sum(is.na(input)) == length(input)){

    stop("Database parameter must be either 'genesis', 'zensus', 'regio', or 'all'.",
         call. = FALSE)

  }

  res <- c()

  if("genesis" %in% input){

    res <- c(res, "genesis" = "gen_api")

  }

  if("zensus" %in% input){

    res <- c(res, "zensus" = "gen_zensus_api")

  }

  if("regio" %in% input){

    res <- c(res, "regio" = "gen_regio_api")

  }

  if("all" %in% input){

    res <- c("genesis" = "gen_api", "zensus" = "gen_zensus_api", "regio" = "gen_regio_api")

  }

  if (identical(res, c())){

    stop("Database parameter must be either 'genesis', 'zensus', 'regio', or 'all'. No other values allowed.",
         call. = FALSE)

  } else {

    return(res)

  }

}
#-------------------------------------------------------------------------------
#' rev_database_function
#'
#' @param input Input to test for database name
#'
rev_database_function <- function(input){
  input[which(input == "gen_api")] <- "genesis"
  input[which(input == "gen_zensus_api")] <- "zensus"
  input[which(input == "gen_regio_api")] <- "regio"

  return(input)
}
#-------------------------------------------------------------------------------
#' check_results
#'
#' @param input Input to test result structure
#'
check_results <- function(input){

  if(length(input) > 1){
    return(input)
  } else {
    input <- input[[1]]
    return(input)
  }

}

