#-------------------------------------------------------------------------------
# Util functions related to API calls
#-------------------------------------------------------------------------------

#' resp_check_data_csv
#'
#' @param resp Response object
#'
resp_check_data_csv <- function(resp) {

  if (!(httr2::resp_content_type(resp) %in% c("text/csv", "application/zip"))) {

    stop("No data found that meets the specified parameters", call. = FALSE)

  }

  return <- httr2::resp_content_type(resp)

}

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

#' test_if_error_find
#'
#' @param input Response object
#' @param para Parameter TRUE/FALSE
#'
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

#' test_if_error
#'
#' @param input Response object
#' @param para Parameter TRUE/FALSE
#'
test_if_error <- function(input, para) {

  if (input$Status$Code == 104 && isFALSE(para)) {

    stop("No object found for your request. Check your parameters if you expected an object for this request.",
         call. = FALSE)

  } else if (input$Status$Code != 0 && isFALSE(para)) {

    stop(input$Status$Content,
         call. = FALSE)

  } else if (input$Status$Code == 104 && isTRUE(para)) {

    message("No object found for your request. Check your parameters if you expected an object for this request. Artificial token is used.",
            call. = FALSE)

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

#' test_if_error_variables
#'
#' @param input Response object
#' @param para Parameter TRUE/FALSE
#'
test_if_error_variables <- function(input, para) {

  if (input$Status$Code == 104) {

    empty_object <- TRUE

  } else if (input$Status$Code != 0) {

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
#'
test_if_process_further <- function(input, para) {

  if (sum(unlist(lapply(input[4:8], function(x) {

    is.null(x)

  }))) == 5 && isFALSE(para)) {

    stop("No object found for your request. Check your parameters if you expected an object for this request.",
         call. = FALSE)

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

#' test_if_error_light
#'
#' @param input Response object
#'
test_if_error_light <- function(input) {

  if (input$Status$Code != 0) {

    warning(input$Status$Content,
            call. = FALSE)

  }

}

#-------------------------------------------------------------------------------

#' return_table_object
#'
#' @param response
#' @param response_type
#' @param language
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

  #-------------------------------------------------------------------------------

  if (response_type == "text/csv"){

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

      stop("Error handling language setting locale (values different from 'de' and 'en'.",
           call. = FALSE)

    }

  }

  #-------------------------------------------------------------------------------

  # If the API response is a ZIP file, we need to temporarily save it

  if (response_type == "application/zip") {

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

  }

  return(result)

}

#-------------------------------------------------------------------------------

#' logincheck_http_error
#'
#' @param database The user input to 'gen_logincheck'
#' @param verbose Boolean. Should the function message in case of success?
#'
#' @return
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
