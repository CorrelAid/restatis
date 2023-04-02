resp_check_data_csv <- function(resp) {
  if (httr2::resp_content_type(resp) != "text/csv") {
    stop("No data found that meets the specified parameters", call. = FALSE)
  }
}

param_check_year <- function(year) {

  if (as.integer(year) < 1900 | as.integer(year) > 2100)  {

    stop("The parameter 'year' has been misspecified (>= 1900 or <= 2100).",
         call. = FALSE)

  }

}

param_collapse_vec <- function(vec) {
  paste0(vec, collapse = ",")
}
