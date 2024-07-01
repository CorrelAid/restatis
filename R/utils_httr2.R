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

warn_if_http_error <- function(response,
                               database,
                               verbose) {

  if (response$status_code != 200) {

    warning("Database: '",
            database,
            "' There seems to be an issue with the authentication process (logincheck upon credential specification failed with code ",
            response$status_code,
            "). ",
            "Please retry specifying your credentials or check whether the API is currently down.",
            call. = FALSE)

    invisible(FALSE)

  } else {

    if(isTRUE(verbose)) {

      message(paste0("Login check for database '", database, "' succeeded."))

    }

    invisible(TRUE)

  }

}
