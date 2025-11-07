#-------------------------------------------------------------------------------
# Util functions related to API calls
#-------------------------------------------------------------------------------

#' test_if_json
#'
#' @param input Response object
#'
test_if_json <- function(input) {

  if ((httr2::resp_content_type(input) == "application/json") && !is.na(httr2::resp_content_type(input))) {

    results_json <- httr2::resp_body_json(input)

  } else {

    stop("No json-csv file detected.", call. = FALSE)

  }

  return(results_json)

}

#-------------------------------------------------------------------------------
#' test_if_okay
#'
#' @param input Response object
#'
test_if_okay <- function(input) {

  results_json <- ifelse(input$`User-Agent` == "https://github.com/CorrelAid/restatis",
                         "Yes",
                         "No"
  )

  return(results_json)

}
#-------------------------------------------------------------------------------

#' test_if_error_find
#'
#' @param input Response object
#' @param para Parameter TRUE/FALSE
#' @param verbose Verbose TRUE/FALSE
#'
test_if_error_find <- function(input, para, verbose = NULL) {

  if (input$Status$Code != 0 && isTRUE(para) && input$Status$Code != 22) {

    stop(input$Status$Content)

  } else if (input$Status$Code != 0 && isFALSE(para) && input$Status$Code != 22) {

    if(!is.null(verbose) && isTRUE(verbose)) {

      message(input$Status$Content)

      message("Artificial token is used.")

    }

    empty_object <- FALSE

  } else {

    empty_object <- "DONE"

  }

  return(empty_object)

}

#-------------------------------------------------------------------------------

#' test_if_error
#'
#' @param input Response object
#' @param para Parameter TRUE/FALSE
#' @param verbose Verbose TRUE/FALSE
#'
test_if_error <- function(input, para, verbose = NULL) {

  if (input$Status$Code == 104 && isFALSE(para)) {

    stop("No object found for your request. Check your parameters if you expected an object for this request.",
         call. = FALSE)

  } else if (input$Status$Code != 0 && isFALSE(para) && input$Status$Code != 22) {

    stop(input$Status$Content,
         call. = FALSE)

  } else if (input$Status$Code == 104 && isTRUE(para)) {

    if(!is.null(verbose) && isTRUE(verbose)){

      message("No object found for your request. Check your parameters if you expected an object for this request. Artificial token is used.")

    }

    empty_object <- TRUE

  } else if (input$Status$Code != 0 && isTRUE(para) && input$Status$Code != 22) {

    if( !is.null(verbose) && isTRUE(verbose)){

      message(input$Status$Content)

      message("Artificial token is used.")

    }

    empty_object <- FALSE

  } else {

    empty_object <- "DONE"

  }

  return(empty_object)

}

#-------------------------------------------------------------------------------

#' test_if_error_variables
#'
#' @param input Response object
#' @param para Parameter TRUE/FALSE
#'
test_if_error_variables <- function(input, para) {

  if (input$Status$Code == 104) {

    empty_object <- TRUE

  } else if (input$Status$Code != 0 && input$Status$Code != 22) {

    empty_object <- FALSE

  } else {

    empty_object <- "DONE"

  }

  return(empty_object)

}

#-------------------------------------------------------------------------------

#' test_if_process_further
#'
#' @param input Response object
#' @param para Parameter TRUE/FALSE
#' @param verbose Verbose TRUE/FALSE
#'
test_if_process_further <- function(input, para, verbose = NULL) {

  if (sum(unlist(lapply(input[4:8], function(x) {

    is.null(x)

  }))) == 5 && isFALSE(para)) {

    stop("No object found for your request. Check your parameters if you expected an object for this request.",
         call. = FALSE)

  } else if (sum(unlist(lapply(input[4:8], function(x) {

    is.null(x)

  }))) == 5 && isTRUE(para)) {

    if(!is.null(verbose) && isTRUE(verbose)){

      message("No object found for your request. Check your parameters if you expected an object for this request. Artificial token is used.")

    }

    empty_object <- TRUE

  } else {

    empty_object <- "DONE"

  }

  return(empty_object)

}

#-------------------------------------------------------------------------------

#' test_if_error_light
#'
#' @param input Response object
#' @param verbose Verbose TRUE/FALSE
#'
test_if_error_light <- function(input, verbose = NULL) {

  if (input$Status$Code != 0 && !is.null(verbose) && isTRUE(verbose) && input$Status$Code != 22) {

    warning(input$Status$Content,
            call. = FALSE)

  }

}

#-------------------------------------------------------------------------------

#' resp_check_data
#'
#' @param resp Response object
#'
resp_check_data <- function(resp) {

  if (!(httr2::resp_content_type(resp) %in% c("application/zip", "text/csv", "application/json"))) {

    stop(paste0("Encountered an invalid response type (", httr2::resp_content_type(resp), ")."),
         call. = FALSE)

  }

  return(httr2::resp_content_type(resp))

}

#-------------------------------------------------------------------------------

#' return_table_object
#'
#' @param response Response object
#' @param response_type Response type
#' @param language Language locale
#' @param all_character Read all variables as character?
#'
return_table_object <- function(response,
                                response_type,
                                language,
                                all_character) {

  #-----------------------------------------------------------------------------
  # Short parameter processing of 'all character' for later use in read_delim

  if (isTRUE(all_character)) {

    all_character <- expression(readr::cols(.default = readr::col_character()))

  } else if (isFALSE(all_character)) {

    all_character <- expression(readr::cols())

  } else {

    stop("Misspecification of parameter 'all_character'. Has to be TRUE or FALSE.",
         call. = FALSE)

  }

  #-----------------------------------------------------------------------------

  if (response_type == "application/json") {

    response_parsed <- httr2::resp_body_json(response)

    if (response_parsed$Status$Code == 98) {

      error_message <- paste0("You have requested a table too big for simple download. \n",
                              "Consider making a range of smaller requests or use the \n",
                              "option to create a job by setting the 'job' parameter \n",
                              "of 'gen_table()' to TRUE. You can then download the job \n",
                              "later (use the function 'gen_list_jobs()' to check its status) \n",
                              "and download it using gen_download_job().")

      stop(error_message, call. = FALSE)

    } else if (response_parsed$Status$Code == 99) {

      message <- paste0("You have successfully created a job with \n",
                        "your request. Use the function 'gen_list_jobs()' ",
                        "to check its status and download it once completed.")

      message(message)

    } else if (response_parsed$Status$Code == 104) {

      stop("There are no results for your request. Please check if the requested table code is valid for the database selected.",
           call. = FALSE)

    } else {

      stop(paste0("There has been an error with your request (API error code: '",
                  response_parsed$Status$Code,
                  "', error message: '",
                  response_parsed$Status$Content,
                  "').\n Please try again later or contact the package maintainer."),
           call. = FALSE)

    }

    #-----------------------------------------------------------------------------

  } else if (response_type == "text/csv") {

    # There has to be a check on language to display correct decimal marks
    # For German results, there needs to be a decimal mark set

    if (language == "de") {

      result <- response %>%
        httr2::resp_body_string() %>%
        readr::read_delim(delim = ";",
                          show_col_types = FALSE,
                          locale = readr::locale(decimal_mark = ",",
                                                 grouping_mark = "."),
                          name_repair = "minimal",
                          col_types = eval(all_character))

    } else if (language == "en") {

      result <- response %>%
        httr2::resp_body_string() %>%
        readr::read_delim(delim = ";",
                          show_col_types = FALSE,
                          name_repair = "minimal",
                          col_types = eval(all_character))

    } else {

      stop("Error handling language setting locale (values different from 'de' and 'en').",
           call. = FALSE)

    }

    return(result)

    #-----------------------------------------------------------------------------

    # If the API response is a ZIP file, we need to temporarily save it

  } else if (response_type == "application/zip") {

    content <- httr2::resp_body_raw(response)

    temp_zip_path <- tempfile(fileext = ".zip")

    writeBin(content,
             temp_zip_path)

    extract_dir <- tempdir()

    utils::unzip(temp_zip_path,
                 exdir = extract_dir)

    extracted_file <- list.files(extract_dir,
                                 full.names = TRUE,
                                 pattern = "_flat.csv")

    # We have to make sure there is only one flat csv to read in the tempdir
    if (length(extracted_file) != 1) {

      stop("There are ambiguous file names (ending on '_flat.csv') in your temporary directory.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------
    # Parsing of the result

    if (language == "de") {

      result <- readr::read_delim(file = extracted_file,
                                  delim = ";",
                                  show_col_types = FALSE,
                                  locale = readr::locale(decimal_mark = ",",
                                                         grouping_mark = "."),
                                  name_repair = "minimal",
                                  col_types = eval(all_character))

    } else if (language == "en") {

      result <- readr::read_delim(file = extracted_file,
                                  delim = ";",
                                  show_col_types = FALSE,
                                  name_repair = "minimal",
                                  col_types = eval(all_character))

    } else {

      stop("Error handling language setting locale (values different from 'de' and 'en').",
           call. = FALSE)

    }

    # Remove temporarily created .csv file from temporary directory
    file.remove(extracted_file)

    return(result)

    #-----------------------------------------------------------------------------

  } else {

    stop("Unknown API response type. Please refer to the package maintainers.",
         call. = FALSE)

  } # End of check application/json, application/zip, text/csv

}

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
      response_sa = .gen_api_core(endpoint = "helloworld/logincheck", database = "st", ...)
    )

    purrr::walk2(.x = response_list,
                 .y = databases,
                 .f = ~ logincheck_stop_or_warn(response = .x,
                                                database = .y,
                                                error = FALSE,
                                                verbose = verbose,
                                                ))

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
#' @param ... Additional parameters (unchecked)
#'
insert_and_save_credentials <- function(database,
                                        use_token,
                                        ...) {

  if (database == "regio") {

    if (isTRUE(use_token)) use_token <- FALSE

    set_credentials_auth(path = "auth_regio.rds",
                         sys_env = "REGIO_KEY",
                         ui_menu_database = "regionalstatistik.de",
                         use_token = use_token,
                         ...)

    #-----------------------------------------------------------------------------

  } else if (database == "bayern") {

    if (isTRUE(use_token)) use_token <- FALSE

    set_credentials_auth(path = "auth_bayern.rds",
                         sys_env = "BAYERN_KEY",
                         ui_menu_database = "statistikdaten.bayern.de",
                         use_token = use_token,
                         ...)

    #-----------------------------------------------------------------------------

  } else if (database == "nrw") {

    if (isTRUE(use_token)) use_token <- FALSE

    set_credentials_auth(path = "auth_nrw.rds",
                         sys_env = "NRW_KEY",
                         ui_menu_database = "landesdatenbank.nrw.de",
                         use_token = use_token,
                         ...)

    #-----------------------------------------------------------------------------

  } else if (database == "bildung") {

    if (isTRUE(use_token)) use_token <- FALSE

    set_credentials_auth(path = "auth_bildung.rds",
                         sys_env = "BILDUNG_KEY",
                         ui_menu_database = "bildungsmonitoring.de",
                         use_token = use_token,
                         ...)

    #-----------------------------------------------------------------------------

  } else if (database == "st") {

    if (isTRUE(use_token)) use_token <- FALSE

    set_credentials_auth(path = "auth_st.rds",
                         sys_env = "ST_KEY",
                         ui_menu_database = "genesis.sachsen-anhalt.de",
                         use_token = use_token,
                         ...)

    #-----------------------------------------------------------------------------

  } else if (database == "zensus") {

    set_credentials_auth(path = "auth_zensus.rds",
                         sys_env = "ZENSUS_KEY",
                         ui_menu_database = "Zensus 2022",
                         use_token = use_token,
                         ...)

    #-----------------------------------------------------------------------------

  } else if (database == "genesis") {

    set_credentials_auth(path = "auth_genesis.rds",
                         sys_env = "GENESIS_KEY",
                         ui_menu_database = "GENESIS",
                         use_token = use_token,
                         ...)

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
#' @param multiple
#'
set_credentials_auth <- function(path,
                                 sys_env,
                                 ui_menu_database,
                                 use_token,
                                 multiple = NULL) {

  if(isTRUE(use_token)) {

    if(multiple > 1){

      credentials_list <- list()

      for(i in 1:multiple){

        if(i == 1) {
          i <- "general"
          id_name  <- "general"
        }

        username <- gen_auth_ask(paste0("API token for ", ui_menu_database, " (Entry ", i, ")"))
        password <- ""

        if(i != "general") {

          id_name <- gen_auth_ask(paste0("ID for the specified credential for ", ui_menu_database, " (Entry ", i, ")"))

        }

        credential_vector <- c(username = username, password = password)

        credentials_list[[i]] <- credential_vector

        if(i != "general"){
          names(credentials_list)[i] <- id_name
        }

      }

    } else {

      username <- gen_auth_ask(paste0("API token for ", ui_menu_database))
      password <- ""


    }

  } else {

    if(multiple > 1){

      credentials_list <- list()

      for(i in 1:multiple){

        if(i == 1) {
          i <- "general"
          id_name  <- "general"
        }

        username <- gen_auth_ask(paste0("username for ", ui_menu_database, " (Entry ", i, ")"))
        password <- gen_auth_ask(paste0("password for ", ui_menu_database, " (Entry ", i, ")"))

        if(i != "general") {

          id_name <- gen_auth_ask(paste0("ID for the specified credential for ", ui_menu_database, " (Entry ", i, ")"))

        }

        credential_vector <- c(username = username, password = password)

        credentials_list[[i]] <- credential_vector

        if(i != "general"){
          names(credentials_list)[i] <- id_name
        }

      }

    } else {

      username <- gen_auth_ask(paste0("username for ", ui_menu_database))
      password <- gen_auth_ask(paste0("password for ", ui_menu_database))
      ID <- "general"
    }


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

  if(multiple == 1) {

    credentials_list <- list(c(username = username, password = password))
    names(credentials_list) <- ID

  }


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

#' check_credentials
#'
#' @param database
#' @param credential_type
#' @param credential_list
#' @param verbose
#'
check_credentials <- function(database,
                              credential_type,
                              credential_list,
                              verbose) {

  # Check Lenght and Values of credential_type
  if(length(credential_type) != 1 & length(credential_type) != 3) {

    stop("Parameter 'credential_type' has to be of length 1 or the default vector.",
         call. = FALSE)

  }

  if(!is.character(credential_type)){

    stop("Parameter 'credential_type' has to be of type character.",
         call. = FALSE)

  }

  if (!all(credential_type %in% c("general", "custom", "multiple"))) {

    stop("Parameter 'credential_type' has to be one of 'general', 'custom', 'multiple' or the default vector.",
         call. = FALSE)

  }

  # -------------------------------------------------

  # Case: General -------------------------------------------------------------
  if(identical(credential_type, c("general", "custom", "multiple")) || identical(credential_type, "general")){

    credential_type <- "general"
    credential_list <- NULL

    if(isTRUE(verbose)){
      message("Parameter 'credential_type' set to 'general' by default. Ignoring all other inputs related to credentials.")
    }

    # Retrieve credentials for all specified databases
    creds_obj <- lapply(database, function(db) {
      temp_obj <- unlist(gen_auth_get(database = db)["general"], recursive = FALSE)
      names(temp_obj) <- c("username", "password")

      return(temp_obj)
    })

    names(creds_obj) <- database


    # Case: Custom -------------------------------------------------------------
  } else if ( identical(credential_type, "custom") ) {

    if(is.null(credential_list)){

      stop("Parameter 'credential_list' has to be specified if 'credential_type' is set to 'custom'.",
           call. = FALSE)

    }

    if(!is.list(credential_list)){

      stop("Parameter 'credential_list' has to be of type list if 'credential_type' is set to 'custom'.",
           call. = FALSE)

    }

    if(!all(names(credential_list[[1]]) %in% c("username", "password"))) {

      stop("Every entry in the 'credential_list' has to include the entries 'username' and 'password' (e.g., list('genesis' = c(username = X, password = Y))) - if 'credential_type' is set to 'custom'.",
           call. = FALSE)

    }

    if(!all(names(credential_list) %in% database)) {

      stop("Every database that is requested in the parameter 'database' needs its own list entry including the entries 'username' and 'password' (e.g., list('genesis' = c(username = X, password = Y))) - if 'credential_type' is set to 'custom'.",
           call. = FALSE)

    }

    creds_obj <- credential_list


    # Case: Multiple -----------------------------------------------------------
  } else if ( identical(credential_type, "multiple") ) {

    if(is.null(credential_list)){

      stop("Parameter 'credential_list' has to be specified if 'credential_type' is set to 'multiple'.",
           call. = FALSE)

    }

    if(!is.list(credential_list)){

      stop("Parameter 'credential_list' has to be of type list if 'credential_type' is set to 'multiple'.",
           call. = FALSE)

    }

    if(!all(names(credential_list) %in% database)) {

      stop("Every database that is requested in the parameter 'database' needs its own list entry including the 'ID' of the aimed account for the specified database (e.g., list('genesis' = c(ID = 'acc2'))) - if 'credential_type' is set to 'multiple'.",
           call. = FALSE)

    }

    if(!all(sapply(credential_list, length) == 1)) {

      stop("Only one 'ID' is allowed per entry in the parameter 'database' - if 'credential_type' is set to 'multiple'.",
           call. = FALSE)

    }

    if(!all(sapply(credential_list, is.character) == TRUE)) {

      stop("Every entry needs to be of type 'character' - if 'credential_type' is set to 'multiple'.",
           call. = FALSE)

    }

    creds_obj <- lapply(names(credential_list), function(db) {
      temp_obj <- gen_auth_get(database = db)
      temp_obj <- temp_obj[credential_list[[db]]]
      temp_obj <- unlist(temp_obj, recursive = FALSE)
      names(temp_obj) <- c("username", "password")

      return(temp_obj)
    })

    names(creds_obj) <- names(credential_list)
  }

  # Return
  return(creds_obj)
}

#-------------------------------------------------------------------------------
