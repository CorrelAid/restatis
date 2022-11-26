#####
#' Get Variables from Statistics
#'
#' @param code
#' @param detailed
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_variables_from_statistic <- function(code = NULL,
                                         detailed = F,
                                         ...) {
  # Check ####
  if (!(is.character(code)) && length(code) < 1L && is.null(code)) {
    stop("code must be a single string or NULL", call. = FALSE)
  }

  if (detailed == FALSE) {
    message("Use detailed = TRUE to obtain the complete output.")
  }

  if (!(isTRUE(detailed) | isFALSE(detailed))) {
    stop("detailed-parameter must be a TRUE or FALSE", call. = FALSE)
  }

  # Processing ####
  results_raw <- gen_api("catalogue/variables2statistic", username = gen_auth_get()$username, password = gen_auth_get()$password, name = code, ...)

  if (httr2::resp_content_type(results_raw) == "application/json") {
    results_json <<- httr2::resp_body_json(results_raw)
  }

  if (results_json$Status$Code != 0) {
    message(results_json$Status$Content)
  }


  list_of_variables <- data.frame()
  if (detailed) {
    lapply(results_json$List, function(x) {
      zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Type" = x$Type, "Values" = x$Values, "Information" = x$Information))
      list_of_variables <<- rbind(list_of_variables, zwisch)
    })
  } else {
    lapply(results_json$List, function(x) {
      zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Type" = x$Type))
      list_of_variables <<- rbind(list_of_variables, zwisch)
    })
  }

  list_of_variables$Object_Type <- "Variable"

  # Summary ####
  list_resp <- list("Variables" = tibble::as_tibble(list_of_variables))
  attr(list_resp, "Code") <- results_json$Parameter$name
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <- results_json$Copyright

  return(list_resp)
}


#####
#' Get Values from Variables
#'
#' @param code
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_values_from_variables <- function(code = NULL,
                                      ...) {
  if (!(is.character(code)) && length(code) < 1L && is.null(code)) {
    stop("code must be a single string or NULL", call. = FALSE)
  }

  results_raw <- gen_api("catalogue/values2variable", username = gen_auth_get()$username, password = gen_auth_get()$password, name = code, ...)

  if (httr2::resp_content_type(results_raw) == "application/json") {
    results_json <<- httr2::resp_body_json(results_raw)
  }

  if (results_json$Status$Code != 0) {
    message(results_json$Status$Content)
  }


  list_of_variables <- data.frame()

  lapply(results_json$List, function(x) {
    zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Variables" = x$Variables, "Information" = x$Information))
    list_of_variables <<- rbind(list_of_variables, zwisch)
  })

  if (nrow(list_of_variables) > 0) {
    list_of_variables$Object_Type <- "Value"
  }

  list_resp <- list("Values" = tibble::as_tibble(list_of_variables))
  attr(list_resp, "Name") <- results_json$Parameter$name
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <- results_json$Copyright
  names(list_resp) <- paste("Values of", results_json$Parameter$name)

  return(list_resp)
}

#####
#' Get Values from Variables from a Statistic
#'
#' @param code
#' @param detailed.variables
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_values_from_variables_from_statistic <- function(code = NULL,
                                                     detailed.variables = F,
                                                     ...) {
  if (!(is.character(code)) && length(code) < 1L && is.null(code)) {
    stop("code must be a single string or NULL", call. = FALSE)
  }

  if (detailed.variables == FALSE) {
    message("Use detailed = TRUE to obtain the complete output.")
  }

  if (!(isTRUE(detailed.variables) | isFALSE(detailed.variables))) {
    stop("detailed-parameter must be a TRUE or FALSE", call. = FALSE)
  }

  variables <- get_variables_from_statistic(code = code, detailed = detailed.variables, ...)

  list_values <- list()

  lapply(variables$Variables$Code, function(x) {
    zwisch <- get_values_from_variables(code = x)
    list_values <<- append(list_values, zwisch)
  })

  list_resp <- list(variables, list_values)

  return(list_resp)
}

# search variable ####
#' Search for Variable in Restatis
#'
#' @param code
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
search_variables <- function(code = NULL,
                             ...) {
  if (!(is.character(code)) && length(code) < 1L && is.null(code)) {
    stop("code must be a single string or NULL", call. = FALSE)
  }

  results_raw <- gen_api("catalogue/variables", username = gen_auth_get()$username, password = gen_auth_get()$password, selection = code, ...)

  if (httr2::resp_content_type(results_raw) == "application/json") {
    results_json <<- httr2::resp_body_json(results_raw)
  }

  if (results_json$Status$Code != 0) {
    message(results_json$Status$Content)
  }

  list_of_variables <- data.frame()

  lapply(results_json$List, function(x) {
    zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Type" = x$Type, "Information" = x$Information))
    list_of_variables <<- rbind(list_of_variables, zwisch)
  })

  list_resp <- list("Variables" = tibble::as_tibble(list_of_variables))
  attr(list_resp, "Code") <- results_json$Parameter$selection
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <- results_json$Copyright

  return(list_resp)
}
