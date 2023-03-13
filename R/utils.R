resp_check_data_csv <- function(resp) {
  if (httr2::resp_content_type(resp) != "text/csv") {
    stop("No data found that meets the specified parameters", call. = FALSE)
  }
}

param_check_year <- function(year) {
  stopifnot(year >= 1900 && year <= 2100)
}

param_collapse_vec <- function(vec) {
  paste0(vec, collapse = ",")
}

check_table_input <- function(   name = NULL,
                                 compress = NULL,
                                 startyear = NULL,
                                 endyear = NULL,
                                 timeslices = NULL,
                                 language = NULL) {

  # name ----

  if (is.null(name)) {

    stop("No table name provided.",
         call. = FALSE)

  } else {

    if (length(name) != 1L) {

      stop("Name must be a single string.",
           call. = FALSE)

    }

    if (!is.character(name)) {

      stop("Name has to be of type 'character'.",
           call. = FALSE)

    }

  }

  # startyear ----

  if(!is.null(startyear)) {

    if (length(startyear) != 1L) {

      stop("Startyear must be a single number.",
           call. = FALSE)

    }

    if (!(startyear>=1900 & startyear<=2100)) {

      stop("Startyear must be between 1900 and 2100.",
           call. = FALSE)

    }

  }

  # endyear ----

  if (!is.null(endyear)) {

    if (length(endyear) != 1L) {

      stop("Endyear must be a single number.",
           call. = FALSE)

    }

    if (!(endyear>=1900 & endyear<=2100)) {

      stop("Endyear must be between 1900 and 2100.",
           call. = FALSE)

    }

  }

  # timeslice ----

  if(!is.null(timeslice)) {

    if (length(timeslice) != 1L) {

      stop("Timeslice must be a single number.",
           call. = FALSE)

    }

    if (!(timeslice>=0)) {

      stop("Timeslice must be a positive number",
           call. = FALSE)

    }

  }
}

