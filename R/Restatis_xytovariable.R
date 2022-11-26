#' Get Objects related to Variables
#'
#' @param code
#' @param category
#' @param detailed
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
xy_to_variable <- function(code = NULL,
                           category = c("tables", "statistics", "cubes"),
                           detailed = F,
                           ...) {
  # Checks ####
  if (!(is.character(code)) && length(code) < 1L && is.null(code)) {
    stop("code must be a single string or NULL", call. = FALSE)
  }

  if (detailed == FALSE) {
    message("Use detailed = TRUE to obtain the complete output.")
  }

  if (!all(category %in% c("tables", "statistics", "cubes"))) {
    stop("Available categories are tables, statistics, and cubes.")
  }

  if (!(isTRUE(detailed) | isFALSE(detailed))) {
    stop("detailed-parameter must be a TRUE or FALSE", call. = FALSE)
  }

  # Processing ####
  if ("tables" %in% category) {
    results_raw <- gen_api("catalogue/tables2variable",
      username = gen_auth_get()$username,
      password = gen_auth_get()$password,
      name = code,
      ...
    )

    if (httr2::resp_content_type(results_raw) == "application/json") {
      results_json <<- httr2::resp_body_json(results_raw)
    }

    if (results_json$Status$Code != 0) {
      message(results_json$Status$Content)
    }

    df_tables <- data.frame()
    if (detailed == TRUE) {
      lapply(results_json$List, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time))
        df_tables <<- rbind(df_tables, zwisch)
      })
      df_tables$Object_Type <- "Table"
    } else {
      lapply(results_json$List, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content))
        df_tables <<- rbind(df_tables, zwisch)
      })
      df_tables$Object_Type <- "Table"
    }
  }

  if ("statistics" %in% category) {
    results_raw <- gen_api("catalogue/statistics2variable",
      username = gen_auth_get()$username,
      password = gen_auth_get()$password,
      name = code,
      ...
    )

    if (httr2::resp_content_type(results_raw) == "application/json") {
      results_json <<- httr2::resp_body_json(results_raw)
    }

    if (results_json$Status$Code != 0) {
      message(results_json$Status$Content)
    }

    df_statistics <- data.frame()

    if (detailed == TRUE) {
      lapply(results_json$List, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Cubes" = x$Cubes, "Information" = x$Information))
        df_statistics <<- rbind(df_statistics, zwisch)
      })
      df_statistics$Object_Type <- "Statistic"
    } else {
      lapply(results_json$List, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content))
        df_statistics <<- rbind(df_statistics, zwisch)
      })
      df_statistics$Object_Type <- "Statistic"
    }
  }

  if ("cubes" %in% category) {
    results_raw <- gen_api("catalogue/timeseries2variable",
      username = gen_auth_get()$username,
      password = gen_auth_get()$password,
      name = code,
      ...
    )

    if (httr2::resp_content_type(results_raw) == "application/json") {
      results_json <<- httr2::resp_body_json(results_raw)
    }

    if (results_json$Status$Code != 0) {
      message(results_json$Status$Content)
    }

    df_cubes <- data.frame()

    if (detailed == TRUE) {
      lapply(results_json$List, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Time" = x$Time, "State" = x$State, "LatestUpdate" = x$LatestUpdate, "Information" = x$Information))
        df_cubes <<- rbind(df_cubes, zwisch)
      })
      df_cubes$Object_Type <- "Cube"
    } else {
      lapply(results_json$List, function(x) {
        zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content))
        df_cubes <<- rbind(df_cubes, zwisch)
      })
      df_cubes$Object_Type <- "Cube"
    }
  }



  # Summary ####
  if (all(c("tables", "variables", "cubes") %in% category)) {
    list_resp <- list(
      "Tables" = tibble::as_tibble(df_tables),
      "Statistics" = tibble::as_tibble(df_statistics),
      "Cubes" = tibble::as_tibble(df_cubes)
    )
  } else if (category == "tables") {
    list_resp <- tibble::as_tibble(df_tables)
  } else if (category == "statistics") {
    list_resp <- tibble::as_tibble(df_statistics)
  } else if (category == "cubes") {
    list_resp <- tibble::as_tibble(df_cubes)
  }

  attr(list_resp, "Code") <- results_json$Parameter$term
  attr(list_resp, "Category") <- category
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <- results_json$Copyright

  return(list_resp)
}
