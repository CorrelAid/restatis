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
