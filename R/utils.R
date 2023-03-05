`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

lgl_to_str <- function(x) {
  x <- x %||% logical()
  stopifnot(is.logical(x) && length(x) <= 1L)
  tolower(isTRUE(x))
}

str_collapse <- function(x) {
  x <- x %||% character()
  stopifnot(is.character(x))
  paste0(x, collapse = ",")
}

lang_check <- function(x) {
  x <- x %||% Sys.getenv("GENESIS_LANG")

  if (!(x %in% c("de", "en"))) {
    stop("Available languages are German (de) or English (en).")
  }
}

type_check <- function(x) {
  x <- x %||% "alle"

  valid_types <- c(
    "klassifizierend",
    "insgesamt",
    "räumlich",
    "sachlich",
    "wert",
    "zeitlich",
    "zeitidentifizierend",
    "alle"
  )

  if (!(x %in% valid_types)) {
    stop(
      "One of the following types must be specified:\n",
      paste(valid_types, collapse = ", "),
    )
  }
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

