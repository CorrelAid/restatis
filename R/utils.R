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
