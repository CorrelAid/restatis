# Restatis - variable function

get_variables_from_statistic <- function(code = NULL,
                                         detailed = F,
                          ...){
  if(!(is.character(code)) && length(code) < 1L && is.null(code)){
    stop("code must be a single string or NULL", call. = FALSE)
  }

  results_raw <- gen_api("catalogue/variables2statistic",  username = gen_auth_get()$username,password = gen_auth_get()$password, name = code, ...)

  if(resp_content_type(results_raw) == "application/json"){
    results_json <<- resp_body_json(results_raw)
  }

  list_of_variables <- data.frame()
  if(detailed){
  lapply(results_json$List, function(x){
    zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Type" = x$Type, "Values" = x$Values, "Information" = x$Information))
    list_of_variables <<- rbind(list_of_variables, zwisch)
  })
  } else {
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Type" = x$Type))
      list_of_variables <<- rbind(list_of_variables, zwisch)
    })
  }

  list_resp <- list("Variables" = list_of_variables)
  attr(list_resp, "Code") <-  results_json$Parameter$name
  attr(list_resp, "Language") <-  results_json$Parameter$language
  attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <-  results_json$Copyright

  return(list_resp)
}



get_values_from_variables <- function(name = NULL,
                                         ...){
  if(!(is.character(name)) && length(name) < 1L && is.null(name)){
    stop("name must be a single string or NULL", call. = FALSE)
  }

  results_raw <- gen_api("catalogue/values2variable",  username = gen_auth_get()$username,password = gen_auth_get()$password, name = name, ...)

  if(resp_content_type(results_raw) == "application/json"){
    results_json <<- resp_body_json(results_raw)
  }

  list_of_variables <- data.frame()

  lapply(results_json$List, function(x){
    zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Variables" = x$Variables, "Information" = x$Information))
    list_of_variables <<- rbind(list_of_variables, zwisch)
  })

  list_resp <- list("Values" = list_of_variables)
  attr(list_resp, "Name") <-  results_json$Parameter$name
  attr(list_resp, "Language") <-  results_json$Parameter$language
  attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <-  results_json$Copyright
  names(list_resp) <- paste("Values of", results_json$Parameter$name)

  return(list_resp)
}


get_values_from_variables_from_statistic <- function(code = NULL,
                                                     detailed.variables = F,
                                                     ...){
  if(!(is.character(code)) && length(code) < 1L && is.null(code)){
    stop("code must be a single string or NULL", call. = FALSE)
  }

  variables <- get_variables_from_statistic(code = code, detailed = detailed.variables, ...)

  list_values <- list()

  lapply(variables$Variables$Code, function(x){
    zwisch <- get_values_from_variables(name = x)
    list_values <<- append(list_values, zwisch)
  })

  list_resp <- list(variables, list_values)

  return(list_resp)
}

search_variables <- function(code = NULL, ...){
  if(!(is.character(code)) && length(code) < 1L && is.null(code)){
    stop("code must be a single string or NULL", call. = FALSE)
  }

  results_raw <- gen_api("catalogue/variables",  username = gen_auth_get()$username,password = gen_auth_get()$password, selection = code, ...)

  if(resp_content_type(results_raw) == "application/json"){
    results_json <<- resp_body_json(results_raw)
  }

  list_of_variables <- data.frame()

  lapply(results_json$List, function(x){
    zwisch <- rbind(c("Code" = x$Code, "Content" = x$Content, "Type" = x$Type, "Information" = x$Information))
    list_of_variables <<- rbind(list_of_variables, zwisch)
  })

  list_resp <- list("Variables" = list_of_variables)
  attr(list_resp, "Code") <-  results_json$Parameter$selection
  attr(list_resp, "Language") <-  results_json$Parameter$language
  attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <-  results_json$Copyright

  return(list_resp)
}
