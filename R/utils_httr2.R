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

    stop("Encountered an invalid response type.",
         call. = FALSE)

  }

  return <- httr2::resp_content_type(resp)

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
                  "').\n Please try again later or contact the package maintainer."),
           call. = FALSE)

    }

  #-----------------------------------------------------------------------------

  } else if (response_type == "text/csv"){

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
#'
#' @return Informative error/warning messages + invisibly TRUE/FALSE
#'
logincheck_http_error <- function(database,
                                  verbose) {

  #-----------------------------------------------------------------------------

  if (length(database) == 1 && database != "all") {

    if (!(database %in% c("genesis", "zensus", "regio"))) {

      stop("Misspecified parameter 'database' (can only be 'all', 'genesis', 'zensus' or 'regio').",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (database == "genesis") response <- gen_api("helloworld/logincheck")
    if (database == "zensus") response <- gen_zensus_api("helloworld/logincheck")
    if (database == "regio") response <- gen_regio_api("helloworld/logincheck")

    logincheck_stop_or_warn(response = response,
                            error = TRUE,
                            verbose = verbose,
                            database = database)

  #-----------------------------------------------------------------------------

  } else if (length(database) == 1 && database == "all") {

    databases <- list("genesis", "zensus", "regio")

    response_list <- list(response_genesis = gen_api("helloworld/logincheck"),
                          response_zensus = gen_zensus_api("helloworld/logincheck"),
                          response_regio = gen_regio_api("helloworld/logincheck"))

    purrr::walk2(.x = response_list,
                 .y = databases,
                 .f = ~ logincheck_stop_or_warn(response = .x,
                                                database = .y,
                                                error = FALSE,
                                                verbose = verbose))

  #-----------------------------------------------------------------------------

  } else if (length(database) > 1 & !("all" %in% database)) {

    if (!(all(database %in% c("genesis", "zensus", "regio")))) {

      stop("You can only specify 'all', 'genesis', 'zensus' or 'regio' inside of the parameter 'database'.",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if ("genesis" %in% database) {

      logincheck_stop_or_warn(response = gen_api("helloworld/logincheck"),
                              error = FALSE,
                              verbose = verbose,
                              database = "genesis")

    }

    #---------------------------------------------------------------------------

    if ("zensus" %in% database) {

      logincheck_stop_or_warn(response = gen_zensus_api("helloworld/logincheck"),
                              error = FALSE,
                              verbose = verbose,
                              database = "zensus")

    }

    #---------------------------------------------------------------------------

    if ("regio" %in% database) {

      logincheck_stop_or_warn(response = gen_regio_api("helloworld/logincheck"),
                              error = FALSE,
                              verbose = verbose,
                              database = "regio")

    }

  #-----------------------------------------------------------------------------

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
#'
insert_and_save_credentials <- function(database) {

  if (database %in% c("genesis", "regio")) {

    username <- gen_auth_ask("username")
    password <- gen_auth_ask("password")

    auth_path <- gen_auth_path(paste0("auth_", database, ".rds"))

    key <- httr2::secret_make_key()

    key_name <- paste0(toupper(database), "_KEY")

    do.call("Sys.setenv", setNames(list(key), key_name))

    message(paste0("Saving '", database, "' database credentials to "),
            auth_path,
            "\n\n",
            "Please add the following line to your .Renviron, ",
            "e.g. via `usethis::edit_r_environ()`, ",
            "to use the specified username and password across sessions:\n\n",
            paste0(key_name, "="),
            key,
            "\n\n")

    dir.create(gen_auth_path(), showWarnings = FALSE, recursive = TRUE)

    httr2::secret_write_rds(list(username = username,
                                 password = password),
                            path = auth_path,
                            key = key_name)

  #-----------------------------------------------------------------------------

  } else if (database == "zensus") {

    want_token_resp <- menu(choices = c("Zensus 2022 API token",
                                        "mail address + password"),
                            graphics = FALSE,
                            title = "Do you want to specifiy a Zensus 2022 API token or regular credentials for access?")

    want_token <- ifelse(want_token_resp == 1L, TRUE, FALSE)

    if (isTRUE(want_token)) {

      username <- gen_auth_ask("API token")
      password <- ""

      auth_path <- gen_auth_path("auth_zensus.rds")

      key <- httr2::secret_make_key()

      Sys.setenv(ZENSUS_KEY = key)

      message("Saving Zensus 2022 database credentials to ",
              auth_path,
              "\n\n",
              "Please add the following line to your .Renviron, ",
              "e.g. via `usethis::edit_r_environ()`, ",
              "to use the specified username and password across sessions:\n\n",
              "ZENSUS_KEY=",
              key,
              "\n\n")

      dir.create(gen_auth_path(), showWarnings = FALSE, recursive = TRUE)

      httr2::secret_write_rds(list(username = username,
                                   password = password),
                              path = auth_path,
                              key = "ZENSUS_KEY")

    } else {

      username <- gen_auth_ask("username")
      password <- gen_auth_ask("password")

      auth_path <- gen_auth_path("auth_zensus.rds")

      key <- httr2::secret_make_key()

      Sys.setenv(ZENSUS_KEY = key)

      message("Saving Zensus 2022 database credentials to ",
              auth_path,
              "\n\n",
              "Please add the following line to your .Renviron, ",
              "e.g. via `usethis::edit_r_environ()`, ",
              "to use the specified username and password across sessions:\n\n",
              "ZENSUS_KEY=",
              key,
              "\n\n")

      dir.create(gen_auth_path(), showWarnings = FALSE, recursive = TRUE)

      httr2::secret_write_rds(list(username = username, password = password),
                              path = auth_path,
                              key = "ZENSUS_KEY")

    }

  } else {

    stop("Misspecification of parameter 'database' in function 'insert_and_save_credentials'.",
         call. = FALSE)

  }

}
