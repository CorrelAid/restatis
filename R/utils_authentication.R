#' Function to check if credential list provided is valid
#'
#' @param credential_list Credential list to be checked
#' @param database Database that is used for authentication
#'
check_credential_list_standalone <- function(credential_list, database) {

  if (!is.null(credential_list)) {

    if (!is.list(credential_list)) {

      stop("Parameter 'credential_list' has to be of type 'list' if 'credential_type' is set.",
           call. = FALSE)

    }

    if (!all(sapply(credential_list, function(x) { all(c("username", "password") %in% names(x)) }))) {

      stop("The database that is requested in the parameter 'database' needs its own list entry including the entries 'username' and 'password' (e.g., list('genesis' = c('username' = 'X', 'password' = 'Y'))).",
           call. = FALSE)

    }

    if (!(database %in% names(credential_list))) {

      stop("The database that is requested in the parameter 'database' has no value in 'credential_list'. Please check the parameters.",
           call. = FALSE)

    }

  }

}

#-------------------------------------------------------------------------------
#' test_database_function
#'
#' @param input Input to test for database name
#' @param credential_list A list containing the credentials for the databases to be accessed. If 'NULL' (default), the function will use the stored credentials from \code{gen_auth_get()}.
#' @param error.input Indicator error.ignore
#' @param text Indicator verbose
#'
test_database_function <- function(input,
                                   credential_list,
                                   error.input,
                                   text) {

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

  if ("bayern" %in% input) {

    res <- c(res, "bayern")

  }

  if ("nrw" %in% input) {

    res <- c(res, "nrw")

  }

  if ("bildung" %in% input) {

    res <- c(res, "bildung")

  }

  if ("st" %in% input) {

    res <- c(res, "st")

  }

  #-----------------------------------------------------------------------------

  if ("all" %in% input) {

    if (isTRUE(text)) {

      message("All databases accessible to you are preselected. Additional databases specified in the 'database' parameter are ignored.")

    }

    res <- c("genesis",
             "zensus",
             "regio",
             "bayern",
             "nrw",
             "bildung",
             "st")

  } else if (length(res) == 0 || is.null(res)) {

    stop("All the databases you have specified are not part of this package.\nPlease enter valid database names ('regio', 'zensus', 'genesis','bayern', 'nrw', 'bildung', 'st' or 'all').",
         call. = FALSE)

  } else if (length(res) != length(input)) {

    if (isFALSE(error.input)) {

      stop("One or more of the specified databases are not part of this package. Currently only 'genesis', 'zensus', 'regio', 'bayern', 'nrw', 'bildung', and 'st' are implemented.",
           call. = FALSE)

    } else {

      if (isTRUE(text)) {

        message("One or more of the specified databases are not part of this package. The function is continued with the available databases that you specified.")

      }

    }

  }

  #-----------------------------------------------------------------------------

  # Check if custom credential list has been provided and if the specification is valid

  if (!is.null(credential_list)) {

    if (isTRUE(text)) {

      message("Please be aware that using custom credentials as in 'credential_list' can be a potential security threat.\nUsing restatis::gen_auth_save() stores the credentials safely in a locked file.\nSee package README for further information on this issue.")

    }

    if (!is.list(credential_list)) {

      stop("Parameter 'credential_list' has to be of type 'list' if 'credential_type' is set.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (length(res) <= length(credential_list)) {

      if (!all(res %in% names(credential_list))) {

        missing <- setdiff(res, names(credential_list))

        stop(paste0("Not all databases you defined in 'database' are contained in your 'credential_list' (missing: '",
                    paste(missing, collapse = "', '"),
                    "'). Please check your parameters."),
             call. = FALSE)

      }

    }

    #---------------------------------------------------------------------------

    if (length(res) > length(credential_list) & isTRUE(error.input)) {

      common <- intersect(res, names(credential_list))

      message(paste0("You have defined more databases in 'database' than you have in your 'credential_list'. The function continues with those available (these are: '"),
              paste(common, collapse = ", "),
              "').")

      res <- common

    } else if (length(res) > length(credential_list) & isFALSE(error.input)) {

      stop("You have defined more databases in 'database' than you have in your 'credential_list'. Please check your parameters.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (!all(sapply(credential_list, function(x) { all(c("username", "password") %in% names(x)) }))) {

      stop("Every database that is requested in the parameter 'database' needs its own list entry including the entries 'username' and 'password' (e.g., list(genesis = c(username = 'X', password = 'Y'))).",
           call. = FALSE)

    }

    return(res)

  } else {

    #---------------------------------------------------------------------------

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

}

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#' logincheck_http_error
#'
#' @param database The user input to 'gen_logincheck'
#' @param verbose Boolean. Should the function message in case of success?
#' @param ... Additional parameters (unchecked)
#'
#' @return Informative error/warning messages + invisibly TRUE/FALSE
#'
logincheck_http_error <- function(database,
                                  verbose,
                                  ...) {

  #-----------------------------------------------------------------------------

  if (length(database) == 1 && database != "all") {

    if (!(database %in% c("genesis", "zensus", "regio", "bayern", "nrw", "bildung", "st"))) {

      stop("Misspecified parameter 'database' (can only be 'all', 'genesis', 'zensus', 'regio', 'bayern', 'nrw', 'bildung' or 'st').",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    response <- .gen_api_core(endpoint = "helloworld/logincheck",
                              database = database,
                              ...)

    logincheck_stop_or_warn(response = response,
                            error = TRUE,
                            verbose = verbose,
                            database = database)

    #-----------------------------------------------------------------------------

  } else if (length(database) == 1 && database == "all") {

    databases <- list("genesis", "zensus", "regio", "bayern", "nrw", "bildung", "st")

    response_list <- list(
      response_genesis = .gen_api_core(endpoint = "helloworld/logincheck", database = "genesis", ...),
      response_zensus = .gen_api_core(endpoint = "helloworld/logincheck", database = "zensus", ...),
      response_regio = .gen_api_core(endpoint = "helloworld/logincheck", database = "regio", ...),
      response_bayern = .gen_api_core(endpoint = "helloworld/logincheck", database = "bayern", ...),
      response_nrw = .gen_api_core(endpoint = "helloworld/logincheck", database = "nrw", ...),
      response_bildung = .gen_api_core(endpoint = "helloworld/logincheck", database = "bildung", ...),
      response_sa = .gen_api_core(endpoint = "helloworld/logincheck", database = "st", ...))

    purrr::walk2(.x = response_list,
                 .y = databases,
                 .f = ~ logincheck_stop_or_warn(response = .x,
                                                database = .y,
                                                error = FALSE,
                                                verbose = verbose))

    #-----------------------------------------------------------------------------

  } else if (length(database) > 1 & !("all" %in% database)) {

    if (!(all(database %in% c("genesis", "zensus", "regio", "bayern", "nrw", "bildung", "st")))) {

      stop("You can only specify 'all', 'genesis', 'zensus', 'regio', 'bayern', 'nrw', 'bildung', 'st' inside of the parameter 'database'.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if ("genesis" %in% database) {

      logincheck_stop_or_warn(response = .gen_api_core(endpoint = "helloworld/logincheck", database = "genesis", ...),
                              error = FALSE,
                              verbose = verbose,
                              database = "genesis")

    }

    #---------------------------------------------------------------------------

    if ("zensus" %in% database) {

      logincheck_stop_or_warn(response = .gen_api_core(endpoint = "helloworld/logincheck", database = "zensus", ...),
                              error = FALSE,
                              verbose = verbose,
                              database = "zensus")

    }

    #---------------------------------------------------------------------------

    if ("regio" %in% database) {

      logincheck_stop_or_warn(response = .gen_api_core(endpoint = "helloworld/logincheck", database = "regio", ...),
                              error = FALSE,
                              verbose = verbose,
                              database = "regio")

    }

    #-----------------------------------------------------------------------------

    if ("bayern" %in% database) {

      logincheck_stop_or_warn(response = .gen_api_core(endpoint = "helloworld/logincheck", database = "bayern", ...),
                              error = FALSE,
                              verbose = verbose,
                              database = "bayern")

    }

    #-----------------------------------------------------------------------------

    if ("nrw" %in% database) {

      logincheck_stop_or_warn(response = .gen_api_core(endpoint = "helloworld/logincheck", database = "nrw", ...),
                              error = FALSE,
                              verbose = verbose,
                              database = "nrw")

    }

    #-----------------------------------------------------------------------------

    if ("bildung" %in% database) {

      logincheck_stop_or_warn(response = .gen_api_core(endpoint = "helloworld/logincheck", database = "bildung", ...),
                              error = FALSE,
                              verbose = verbose,
                              database = "bildung")

    }

    #-----------------------------------------------------------------------------

    if ("st" %in% database) {

      logincheck_stop_or_warn(response = .gen_api_core(endpoint = "helloworld/logincheck", database = "st", ...),
                              error = FALSE,
                              verbose = verbose,
                              database = "st")

    }

  } else {

    stop("If you want to specify 'all', do not specify further databases (i.e., just set database to 'all').",
         call. = FALSE)

  }

}

#-------------------------------------------------------------------------------

#' logincheck_stop_or_warn
#'
#' @param response A HTTP response object
#' @param error Boolean. Should the function warn or throw an error?
#' @param verbose Boolean. Should the function message in case of success?
#' @param database The database that the check should be run for
#'
#' @return In case of failure warns or errors. Invisibly returns TRUE (success) or FALSE (failure)
#'
logincheck_stop_or_warn <- function(response,
                                    error,
                                    verbose,
                                    database) {

  #-----------------------------------------------------------------------------

  request_failed <- grepl("Ein Fehler ist aufgetreten", httr2::resp_body_json(response)$Status)

  if (isTRUE(request_failed) & isTRUE(error)) {

    stop(paste0("Database: '",
                database,
                "': There seems to be an issue with the authentication process (logincheck upon credential specification failed). \n",
                "Please retry specifying your credentials."),
         call. = FALSE)

    invisible(FALSE)

    #-----------------------------------------------------------------------------

  } else if (isTRUE(request_failed) & isFALSE(error)) {

    warning(paste0("Database: '",
                   database,
                   "': There seems to be an issue with the authentication process (logincheck upon credential specification failed). \n",
                   "Please retry specifying your credentials."),
            call. = FALSE)

    invisible(FALSE)

    #-----------------------------------------------------------------------------

  } else if (isFALSE(request_failed)) {

    if (isTRUE(verbose)) {

      message(paste0("Login check for database '", database, "' succeeded."))

    }

    invisible(TRUE)

    #-----------------------------------------------------------------------------

  } else {

    stop("Checking the HTTP response failed.",
         call. = FALSE)

  }

}

#-------------------------------------------------------------------------------

#' insert_and_save_credentials
#'
#' @param database The database to specify credentials for
#' @param use_token Boolean. Do you want to (if possible) set an API token instead of password + username? Defaults to FALSE.
#'
insert_and_save_credentials <- function(database,
                                        use_token) {

  if (database == "regio") {

    if (isTRUE(use_token)) use_token <- FALSE

    set_credentials_auth(path = "auth_regio.rds",
                         sys_env = "REGIO_KEY",
                         ui_menu_database = "regionalstatistik.de",
                         use_token = use_token)

    #-----------------------------------------------------------------------------

  } else if (database == "bayern") {

    if (isTRUE(use_token)) use_token <- FALSE

    set_credentials_auth(path = "auth_bayern.rds",
                         sys_env = "BAYERN_KEY",
                         ui_menu_database = "statistikdaten.bayern.de",
                         use_token = use_token)

    #-----------------------------------------------------------------------------

  } else if (database == "nrw") {

    if (isTRUE(use_token)) use_token <- FALSE

    set_credentials_auth(path = "auth_nrw.rds",
                         sys_env = "NRW_KEY",
                         ui_menu_database = "landesdatenbank.nrw.de",
                         use_token = use_token)

    #-----------------------------------------------------------------------------

  } else if (database == "bildung") {

    if (isTRUE(use_token)) use_token <- FALSE

    set_credentials_auth(path = "auth_bildung.rds",
                         sys_env = "BILDUNG_KEY",
                         ui_menu_database = "bildungsmonitoring.de",
                         use_token = use_token)

    #-----------------------------------------------------------------------------

  } else if (database == "st") {

    if (isTRUE(use_token)) use_token <- FALSE

    set_credentials_auth(path = "auth_st.rds",
                         sys_env = "ST_KEY",
                         ui_menu_database = "genesis.sachsen-anhalt.de",
                         use_token = use_token)

    #-----------------------------------------------------------------------------

  } else if (database == "zensus") {

    set_credentials_auth(path = "auth_zensus.rds",
                         sys_env = "ZENSUS_KEY",
                         ui_menu_database = "Zensus 2022",
                         use_token = use_token)

    #-----------------------------------------------------------------------------

  } else if (database == "genesis") {

    set_credentials_auth(path = "auth_genesis.rds",
                         sys_env = "GENESIS_KEY",
                         ui_menu_database = "GENESIS",
                         use_token = use_token)

  } else {

    stop("Misspecification of parameter 'database' in function 'insert_and_save_credentials'.",
         call. = FALSE)

  }

}

#-------------------------------------------------------------------------------

#' set_credentials_auth
#'
#' @param path Path for the .rds file
#' @param sys_env System environment variable name for the key
#' @param ui_menu_database The database for the auth request ('GENESIS' or 'Zensus 2022')
#' @param use_token Boolean. Do you want to (if possible) set an API token instead of password + username? Defaults to FALSE.
#'
set_credentials_auth <- function(path,
                                 sys_env,
                                 ui_menu_database,
                                 use_token) {

  if (isTRUE(use_token)) {

    username <- gen_auth_ask(paste0("API token for ", ui_menu_database))
    password <- ""

  } else {

    username <- gen_auth_ask(paste0("username for ", ui_menu_database))
    password <- gen_auth_ask(paste0("password for ", ui_menu_database))

  }

  auth_path <- gen_auth_path(path)

  key <- httr2::secret_make_key()

  do.call(Sys.setenv, setNames(list(key), sys_env))

  message(paste0("Your credentials for the ", ui_menu_database, " database have been saved to "),
          auth_path,
          ".",
          "\n\n",
          "The key to the credentials file has been set as environment variable, which is lost after the R session is closed.",
          "\n\n",
          "If you want to keep the key saved for future sessions, please add the following line to your .Renviron file, ",
          "e.g. via the function `usethis::edit_r_environ()`:\n\n",
          paste0(sys_env, "="),
          key,
          "\n\n")

  dir.create(gen_auth_path(), showWarnings = FALSE, recursive = TRUE)

  credentials_list <- list(username = username, password = password)

  if (isTRUE(use_token)) {

    attr(credentials_list, "credential_type") <- "token"

  } else {

    attr(credentials_list, "credential_type") <- "username_password"

  }

  httr2::secret_write_rds(credentials_list,
                          path = auth_path,
                          key = sys_env)

}

#-------------------------------------------------------------------------------
