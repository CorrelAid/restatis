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
