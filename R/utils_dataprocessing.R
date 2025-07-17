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

  # Load data
  evas_list <- restatis::evas_list

  #-----------------------------------------------------------------------------
  # Process them

  list_of$Main <- apply(list_of, 1, function(x) {

    obj <- evas_list$Titel[evas_list$EVAS == substr(x["Code"], 1, 1)]

    if (length(obj) == 0) {

      obj <- "No assignment"

    }

    return(obj)

  })

  #-----------------------------------------------------------------------------

  list_of$Main2 <- apply(list_of, 1, function(x) {

    obj <- evas_list$Titel[evas_list$EVAS == substr(x["Code"], 1, 2)]

    if (length(obj) == 0) {

      obj <- "No assignment"

    }

    return(obj)

  })

  #-----------------------------------------------------------------------------

  list_of$Main3 <- apply(list_of, 1, function(x) {

    obj <- evas_list$Titel[evas_list$EVAS == substr(x["Code"], 1, 3)]

    if (length(obj) == 0) {

      obj <- "No assignment"

    }

    return(obj)

  })

  #-----------------------------------------------------------------------------

  list_of$Main5 <- apply(list_of, 1, function(x) {

    obj <- evas_list$Titel[evas_list$EVAS == substr(x["Code"], 1, 5)]

    if (length(obj) == 0) {

      obj <- "No assignment"

    }

    return(obj)

  })

  #-----------------------------------------------------------------------------

  keep <- colnames(list_of[,1:(ncol(list_of) - 4)])

  #-----------------------------------------------------------------------------
  if (sum(list_of$Main == "No assignment") != nrow(list_of)) {

    nestedlist <- split(list_of, list_of$Main, drop = TRUE)

    #---------------------------------------------------------------------------
    if (sum(list_of$Main2 == "No assignment") != nrow(list_of)) {

      nestedlist <- lapply(nestedlist, function(x) {

        obj <- split(x, x["Main2"], drop = TRUE)

      })

      #-------------------------------------------------------------------------
      if (sum(list_of$Main3 == "No assignment") != nrow(list_of)) {

        nestedlist <- lapply(nestedlist, function(x) {

          lapply(x, function(y) {

            obj <- split(y, y["Main3"])

          })

        })

        #-----------------------------------------------------------------------
        if (sum(list_of$Main5 == "No assignment") != nrow(list_of)) {

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
#' @param area Parameter to be checked
#' @param verbose Parameter to be checked
#' @param raw Parameter to be checked
#' @param pagelength Parameter to be checked
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
                                 pagelength = NULL,
                                 database = NULL,
                                 area = NULL,
                                 caller = NULL,
                                 verbose = NULL,
                                 raw = NULL) {

  #-----------------------------------------------------------------------------

  if ((length(database) == 1 && !is.null(database) && database == "all") |
      (length(database) > 1 && "all" %in% database)) {

    database <- c("regio", "zensus", "genesis")

  }

  #-----------------------------------------------------------------------------
  # verbose ----
  if (!is.null(verbose)) {

    #---------------------------------------------------------------------------

    if (length(verbose == 1)) {

      #-------------------------------------------------------------------------

      if (!is.logical(verbose) || length(verbose) != 1) {

        stop("Parameter 'verbose' has to be of type 'logical' and of length 1.",
             call. = FALSE)

      }

    }

  }

  #-------------------------------------------------------------------------------
  # Pagelength

  if (!is.null(pagelength) && (!(pagelength %in% c(1:25000)) || length(pagelength) != 1)) {

    stop("Parameter 'pagelength' must have a value between 1 and 25,000 and a length of 1.",
         call. = FALSE)

  }

  #-----------------------------------------------------------------------------
  # Code & Term ----

  if (is.null(code) && is.null(term) && !is.null(caller)) {

    if (!(caller %in% c("gen_search_vars", "restatis::gen_search_vars",
                        "gen_signs", "restatis::gen_signs"))) {

      stop("Parameter 'code' or 'term' must not be NULL, missing or unspecified.",
           call. = FALSE)

    }

  }

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

    if (length(category) != 1 && caller %in% c("restatis::gen_metadata", "gen_metadata")) {

      stop("Parameter 'category' must have a length of 1. Please specify the category.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (caller %in% c("restatis::gen_catalogue",
                      "restatis::gen_objects2var",
                      "gen_catalogue",
                      "gen_objects2var")) {

      #-------------------------------------------------------------------------

      if ("zensus" %in% database) {

        #-----------------------------------------------------------------------

        if (length(category) == 1 &&
            "cubes" %in% category &&
            isFALSE(error.ignore) &&
            isTRUE(verbose)) {

          stop("Available categories for 'zensus' database are: 'tables' and 'statistics'.",
               call. = FALSE)

        #-----------------------------------------------------------------------

        } else if (length(category) == 1 &&
                   "cubes" %in% category &&
                   isTRUE(error.ignore)  &&
                   isTRUE(verbose)) {

          warning("Available categories for the 'zensus' database are: 'tables' and 'statistics'.\nFunction is continued with a placeholder for the 'cubes' output.",
                  call. = FALSE)

        } else if ("cubes" %in% category &&
                   isFALSE(error.ignore) &&
                   isTRUE(verbose)) {

          stop("Available categories for 'zensus' database are: 'tables' and 'statistics'.",
               call. = FALSE)

        } else if ("cubes" %in% category &&
                   isTRUE(error.ignore) &&
                   isTRUE(verbose)) {

          warning("Available categories for 'zensus' database are: 'tables' and 'statistics'.\nFunction is continued with specified 'category'-parameter excluding 'cubes'.",
                  call. = FALSE)

        }

      }

    #-------------------------------------------------------------------------------

      if("genesis" %in% database){

        if (!all(category %in% c("tables", "cubes", "statistics"))) {

          stop("Available categories are 'tables', 'statistics', and 'cubes'.",
               call. = FALSE)

        }

      }

    }

    #---------------------------------------------------------------------------

    if (caller %in% c("restatis::gen_objects2stat",
                      "gen_objects2stat")) {

      #-------------------------------------------------------------------------

      if("zensus" %in% database){

        #-----------------------------------------------------------------------

        if (!all(category %in% c("tables", "cubes", "variables"))) {

          stop("Available categories are 'tables' and 'variables'.",
               call. = FALSE)

        }

        #-----------------------------------------------------------------------

        if (length(category) == 1 &&
            "cubes" %in% category &&
            isFALSE(error.ignore)) {

          stop("Available categories for 'zensus' database are: 'tables' and 'variables'.",
               call. = FALSE)

        }

        #-----------------------------------------------------------------------

        else if (length(category) == 1 &&
                 "cubes" %in% category &&
                 isTRUE(error.ignore)  &&
                 isTRUE(verbose)) {

          warning("Available categories for 'zensus' database are: 'tables' and 'variables'.\nFunction is continued with a placeholder for the 'cubes' output.",
                  call. = FALSE)

        }

        #-----------------------------------------------------------------------

        else if ("cubes" %in% category &&
                 isFALSE(error.ignore) &&
                 isTRUE(verbose)) {

          warning("Available categories for 'zensus'-database are: 'tables' and 'variables'.",
                  call. = FALSE)

        }

        #-----------------------------------------------------------------------

        else if ("cubes" %in% category &&
                 isTRUE(error.ignore) &&
                 isTRUE(verbose)) {

          warning("Available categories for 'zensus' database are: 'tables' and 'variables'.\nFunction is continued with specified 'category'-parameter excluding 'cubes'.",
                  call. = FALSE)

        }

      }

      #-------------------------------------------------------------------------

      if("genesis" %in% database){

        if (!all(category %in% c("tables", "cubes", "variables"))) {

          stop("Available categories are 'tables', 'variables', and 'cubes'.",
               call. = FALSE)

        }

      }

    }

    #---------------------------------------------------------------------------

    if (caller %in% c("restatis::gen_find", "gen_find")) {

      #-------------------------------------------------------------------------

        if (!all(category %in% c("all", "tables", "statistics", "variables", "cubes"))) {

          #---------------------------------------------------------------------

          if ("genesis" %in% database){

            stop("Available categories for parameter 'category' for 'genesis' database are 'all', 'tables', 'statistics', 'variables', and 'cubes'.",
                 call. = FALSE)

          }

          #---------------------------------------------------------------------

          if("zensus" %in% database){

            stop("Available categories for parameter 'category' for 'zensus' database are 'all', 'tables', 'statistics', and 'variables'.",
                 call. = FALSE)

        }

      }

    }

    #---------------------------------------------------------------------------

    if (caller %in% c("restatis::gen_find", "gen_find")) {

      #-------------------------------------------------------------------------

      if("zensus" %in% database){

        #-----------------------------------------------------------------------

        if ("cubes" %in% category &&
            isTRUE(error.ignore) &&
            isTRUE(verbose)) {

          warning("Available categories for 'zensus' database are: 'all', 'tables', 'statistics', and 'variables'.\nFunction is continued with a placeholder for the 'cubes' output.",
                  call. = FALSE)

        }

        #-----------------------------------------------------------------------

        else if ("all" %in% category && isTRUE(verbose)) {

          warning("There are generally no 'cubes' objects available for the 'zensus' database. Token is automatically used.",
                  call. = FALSE)

        }

        #-----------------------------------------------------------------------

        else if ("cubes" %in% category &&
                 isFALSE(error.ignore)) {

          stop("Available categories for 'zensus' database are 'all', 'tables', 'statistics', and 'variables'.",
               call. = FALSE)

        }

      }

    }

    #---------------------------------------------------------------------------

    if (caller %in% c("restatis::gen_metadata", "gen_metadata")) {

      #-------------------------------------------------------------------------

      if("genesis" %in% database){

        #-----------------------------------------------------------------------

        if (!all(category %in% c("cube", "statistic", "table", "variable", "value"))) {

            stop("Available categories for parameter 'category' for 'genesis' database are 'cube', 'table', 'statistic', 'variable', and 'value'.",
                  call. = FALSE)

          }

        }

      #-------------------------------------------------------------------------

      else if("zensus" %in% database) {

        if (!all(category %in% c("statistic", "table", "variable", "value"))) {

          stop("Available categories for parameter 'category' for 'zensus' database are 'table', 'statistic', 'variable', and 'value'.",
               call. = FALSE)

        }

      }

    }

  }


  #-----------------------------------------------------------------------------
  # detailed ----

  if (!is.null(detailed)) {

    if (!is.logical(detailed) || length(detailed) != 1) {

      stop("Parameter 'detailed' has to be of type 'logical' and of length 1.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (isFALSE(detailed) && isTRUE(verbose)) {

      message("Use 'detailed = TRUE' to obtain the complete output.")

    }

  }

  #-------------------------------------------------------------------------------
  # type ----

  if (!is.null(type)) {

    #---------------------------------------------------------------------------

    if ("genesis" %in% database){

      #-------------------------------------------------------------------------

      if (!all(type %in% c("all", "tables", "statistics", "statisticsUpdates"))) {

        stop("Available categories for parameter 'type' for 'genesis' database are 'tables', 'statistics', 'statistic updates', and 'all'.",
             call. = FALSE)

      }

    }

    #---------------------------------------------------------------------------

    if ("zensus" %in% database){

      if (!all(type %in% c("all", "tables", "statistics"))) {

        stop("Available categories for parameter 'type' for 'zensus' database are 'tables', 'statistics', and 'all'.",
             call. = FALSE)

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

    if (!is.logical(error.ignore) ||
          length(error.ignore) != 1) {

        stop("Parameter 'error.ignore' has to be of type 'logical' and of length 1.",
             call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (isTRUE(error.ignore) && isTRUE(verbose) && !(caller %in% c("gen_metadata", "restatis::gen_metadata"))) {

      message("Use 'error.ignore = FALSE' to stop the function at the point where no object could be found.")

    }

  }

  #-------------------------------------------------------------------------------
  # ordering ----

  if (!is.null(ordering)) {

    if (!is.logical(ordering) || length(ordering) != 1) {

      stop("Parameter 'ordering' has to be of type 'logical' and of length 1.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (isFALSE(ordering) && isTRUE(verbose)) {

      message("Use 'ordering = TRUE' to obtain the output ordered based on the search term presence.")

    }

  }

  #-----------------------------------------------------------------------------
  # area ----
  if (!is.null(area)) {

    if (!is.character(area) || length(area) != 1) {

      stop("Parameter 'area' has to be of type 'character' and of length 1.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (!area %in% c("all", "public", "user")) {

      stop("Available categories for parameter 'area' are 'all', 'public', and 'user'.")

    }

  }

  #-----------------------------------------------------------------------------
  # Recommendation ----
  if(!is.null(verbose) && !is.null(error.ignore) && !is.null(database)){

    if(isTRUE(verbose) && isFALSE(error.ignore) && length(database) > 1){

      message("If you want to search through all databases it is often useful to set the 'error.ignore' parameter to TRUE.\nThis will prevent the function from stopping if no object is found in one of the databases.")

    }

  }

  #-----------------------------------------------------------------------------
  # raw ----
  if (!is.null(raw)){

    if (!is.logical(raw) || length(raw) != 1) {

      stop("Parameter 'raw' has to be of type 'logical' and of length 1.",
           call. = FALSE)

    }

    if (isTRUE(raw) && isTRUE(verbose)) {

      message("Use 'raw = FALSE' to obtain the output in a more readable format.")

    }

    if (isFALSE(raw) && isTRUE(verbose)) {

      message("The default 'raw = FALSE' can simplify some information to a significant extent.")

    }

  }

  #-----------------------------------------------------------------------------
  # date ----

  if (!is.null(date)) {

    #---------------------------------------------------------------------------

    if (identical(date, c("now", "week_before", "month_before", "year_before"))) {

      if (isTRUE(verbose)){

      message("Please note that per default the current system date is used.\nThis date is calculated automatically and may differ from manually entered data.\nManually entered data must have the format DD.MM.YYYY.")

      }

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

        if (isTRUE(verbose)) {

        message("Please note that this date is calculated automatically and may differ from manually entered data.\nManually entered data must have the format DD.MM.YYYY.")

        }

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

  } # The aforementioned part must be at the end of the function

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
#' @param text Indicator verbose
#'
titel_search <- function(x, term, text) {

  split <- unlist(strsplit(gsub(" ", "\\bund\\b", term), c("\\bund\\b|\\bUND\\b|\\bUnd\\b|\\&|\\bODER\\b|\\boder\\b|\\bOder\\b|\\|")))

  split <- split[sapply(split, function(y) {

    nchar(y) > 0

  })]

  #-----------------------------------------------------------------------------

  if (length(split) == 1) {

    a <- grepl(split, x$Content, ignore.case = TRUE)

  } else if (grep("\\bODER\\b|\\boder\\b|\\bOder\\b|\\|", term, ignore.case = TRUE) &&
             grep("\\bUND\\b|\\bund\\b|\\bUnd\\b|\\|", term, ignore.case = TRUE)) {

    a <- rep(FALSE, length(x$Content))

    if (isTRUE(verbose)) {

      message("Combination of words too complex for ordering. Data is processed without ordering.")

    }

  } else if (grep("\\bODER\\b|\\boder\\b|\\bOder\\b|\\|", term, ignore.case = TRUE)) {

    a <- grepl(paste(split, collapse = "|"), x$Content, ignore.case = TRUE)

  } else if (grep("\\bUND\\b|\\bund\\b|\\bUnd\\b|\\|", term, ignore.case = TRUE)) {

    a <- sapply(x$Content, function(con) {

      all(sapply(split, function(z) {

        grepl(z, con, ignore.case = TRUE)

      }))

    })

  } else {

    a <- rep(FALSE, length(x$Content))

    if (isTRUE(verbose)) {

      message("Combination of words not valid for ordering. Data is processed without ordering.")

    }

  }

  return(a)

}

#-------------------------------------------------------------------------------
#' test_database_function
#'
#' @param input Input to test for database name
#' @param error.input Indicator error.ignore
#' @param text Indicator verbose
#'
test_database_function <- function(input, error.input, text){

  #-----------------------------------------------------------------------------

  if (!is.logical(text) || length(text) != 1) {

     stop("Parameter 'verbose' has to be of type 'logical' and of length 1.",
           call. = FALSE)

  }

  #-----------------------------------------------------------------------------

  if (!is.logical(error.input) || length(error.input) != 1) {

    stop("Parameter 'error.ignore' has to be of type 'logical' and of length 1.",
         call. = FALSE)

  }

  #-----------------------------------------------------------------------------

  if (sum(is.na(input)) == length(input)) {

    stop("You have to correctly specifiy a 'database' parameter. Please refer to the documentation for further information.",
         call. = FALSE)

  }

  #-----------------------------------------------------------------------------

  res <- c()

  if ("genesis" %in% input) {

    res <- c(res, "genesis")

  }

  if ("zensus" %in% input) {

    res <- c(res, "zensus")

  }

  if ("regio" %in% input) {

    res <- c(res, "regio")

  }

  #-----------------------------------------------------------------------------

  if ("all" %in% input) {

    if (isTRUE(text)) {

      message("All databases accessible to you are preselected. Additional databases specified in the 'database' parameter are ignored.")

    }

    res <- c("genesis",
             "zensus",
             "regio")

  } else if (length(res) == 0 || is.null(res)) {

    stop("All the databases you have specified are not part of this package.\nPlease enter valid database names ('regio', 'zensus', 'genesis' or 'all').",
         call. = FALSE)

  } else if (length(res) != length(input)) {

    if (isFALSE(error.input)) {

      stop("One or more of the specified databases are not part of this package. Currently only 'genesis', 'zensus', and 'regio' are implemented.",
           call. = FALSE)

    } else {

      if (isTRUE(text)) {

        message("One or more of the specified databases are not part of this package. The function is continued with the available databases that you specified.")

      }

    }

  }

  #-----------------------------------------------------------------------------

  # Check if credentials are available for the selected databases

  check <- sapply(res, function(y) {

    result <- tryCatch({

        user <- gen_auth_get(y)$username

      }, error = function(e) {

        return(FALSE)

      })

    if (isFALSE(result)) {

      return(FALSE)

    } else {

      return(TRUE)

    }

  })

  #-----------------------------------------------------------------------------

  if (sum(check) == 0) {

    stop("None of the specified databases are accessible to you. Please check your credentials.",
         call. = FALSE)

  } else if (any(check == FALSE)) {

    if (isTRUE(error.input)) {

      if (isTRUE(text)) {

        mess <- paste("The following databases are not accessible to you:", paste(res[!check], collapse = ", "))

        message(mess)

        message("The function is continued with the available databases that you specified.")

      }

      res <- res[check]

    } else {

      mess <- paste("The following databases are not accessible to you:", paste(res[!check], collapse = ", "), "\nPlease check your credentials.")

      stop(mess, call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------

  if (identical(res, c())) {

    stop("You have to correctly specify a 'database' parameter. Please refer to the documentation for further information.",
         call. = FALSE)

  } else {

    return(res)

  }

}

#-------------------------------------------------------------------------------
#' check_results
#'
#' @param input Input to test result structure
#'
check_results <- function(input){

  if (length(input) > 1) {

    return(input)

  } else {

    input <- input[[1]]
    return(input)

  }

}

#-------------------------------------------------------------------------------
#' find_token
#'
#' @param input Input to test result structure
#' @param error.input error.ignore TRUE or FALSE
#' @param text verbose TRUE or FALSE
#' @param sub_category sub_category character string
#'
find_token <- function(input, error.input, text, sub_category) {

  mes <- paste("No", sub_category, "found for the search term.")

  if (isTRUE(error.input)) {

    if (isTRUE(text)) {

      message(mes)

    }

    return(mes)

  } else {

    stop(mes)

  }

}
