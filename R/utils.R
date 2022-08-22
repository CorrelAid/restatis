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
  if(is.null(x)){
    language = Sys.getenv("LANG")
  }
  if(!(x %in% c("de","eng"))){
    stop("Available languages are German (de) or English (eng).")
  }
}

type_check <- function(x) {
  if(is.null(x)){
    type = "alle"
    message("No type was specified so all types will be included.")
  }
  if(!(x %in% c("klassifizierend", "insgesamt", "räumlich", "sachlich", "wert", "zeitlich", "zeitidentifizierend", "alle"))){
    stop("One of the following types must be specified: klassifizierend, insgesamt, räumlich, sachlich, wert, zeitlich, zeitidentifizierend, alle.")
  }
}
