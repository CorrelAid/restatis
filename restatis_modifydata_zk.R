# Restatis - modified_data function

modified_data <- function(code = NULL, 
                          type = c("all", "Tabellen", "Statistiken", "StatistikUpdates"), 
                          date = c("now", "week_before", "month_before", "year_before"),
                          ...){
  if(!(is.character(code)) && length(code) < 1L && is.null(code)){
    stop("code must be a single string or NULL", call. = FALSE)
  }
  
  type <- match.arg(type)
  
  if(date == "now"){
    date <- Sys.Date()
  }
  else if(date == "week_before"){
    today <- Sys.Date()
    date <- today - 7
  }
  else if(date == "month_before"){
    today <- Sys.Date()
    date <- today - as.difftime(4, units = "weeks")
  }
  else if(date == "year_before"){
    today <- Sys.Date()
    date <- today - as.difftime(52, units = "weeks")
  }
  
  if( type == "all"){
    results_raw <- gen_api("catalogue/modifieddata",  username = gen_auth_get()$username,password = gen_auth_get()$password, selection = code, type = "all", date = date, ...)
    
    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    } 
    
  }
  
  if( type == "Tabellen"){
    results_raw <- gen_api("catalogue/modifieddata",  username = gen_auth_get()$username,password = gen_auth_get()$password, selection = code, type = "Neue Tabellen", date = date, ...)
    
    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    } 
    
  }
  
  if( type == "Statistiken"){
    results_raw <- gen_api("catalogue/modifieddata",  username = gen_auth_get()$username,password = gen_auth_get()$password, selection = code, type = "Neue Statistiken", date = date, ...)
    
    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    } 
    
  }
  
  if( type == "StatistikUpdates"){
    results_raw <- gen_api("catalogue/modifieddata",  username = gen_auth_get()$username,password = gen_auth_get()$password, selection = code, type = "Aktualisierte Statistiken", date = date, ...)
    
    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    } 
    
  }
  
  if(length(results_json$List) == 0){
    cat("No modified objects found for your code.")
  } else {
  table <- data.frame()
  
  lapply(results_json$List, function(x){
    zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1,40), "Date" = x$Date, "Added" = x$Added, "Type" = x$Type))
    table <<- rbind(table, zwisch)
  })
  table$Date <- as.Date.character(table$Date, format = "%d.%m.%Y")
  table$Added <- as.Date.character(table$Added, format = "%d.%m.%Y")
  
  splittable <- split(table[order(table$Date, table$Added, decreasing = T), ], table$Type)
  }
  
  list_resp <- list("Modified" = splittable) 
  attr(list_resp, "Code") <-  results_json$Parameter$selection
  attr(list_resp, "Type") <-  results_json$Parameter$type
  attr(list_resp, "Date") <-  results_json$Parameter$date
  attr(list_resp, "Language") <-  results_json$Parameter$language
  attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <-  results_json$Copyright
  
  return(list_resp)
}

