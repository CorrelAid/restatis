# Restatis - hauptcatalouge function
# Abdecken: cubes statistics tables timeseries
# Aussehen wichtig nach EVAS
load("evas_list_20220724.RData")

catalouge <- function(code = NULL,
                      type = c("cubes", "statistics", "tables", "timeseries"),
                      detailed = F,
                          ...){
  if (!(is.character(code)) && length(code) < 1L && is.null(code)){
    stop("code must be a single string or NULL", call. = FALSE)
  }

  if(!(isTRUE(detailed) | isFALSE(detailed))){
    stop("detailed-parameter must be a TRUE or FALSE", call. = FALSE)
  }

  type <- match.arg(type)

  if( type == "cubes"){
    results_raw <- gen_api("catalogue/cubes",  username = gen_auth_get()$username ,password = gen_auth_get()$password, selection = code, ...)

    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    }

    list_of <- data.frame()

    if(detailed){
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1, 40), "Time" = x$Time, "Latest_Update" = x$LatestUpdate, "State" = x$State, "Information" = x$Information))
      list_of <<- rbind(list_of, zwisch)
    })
    } else {
      lapply(results_json$List, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1, 70)))
        list_of <<- rbind(list_of, zwisch)
      })
    }
  }


  if( type == "statistics"){
    results_raw <- gen_api("catalogue/statistics",  username = gen_auth_get()$username,password = gen_auth_get()$password, selection = code, ...)

    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    }

    list_of <- data.frame()

    if(detailed){
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1, 40), "Cubes" = x$Cubes, "Information" = x$Information))
      list_of <<- rbind(list_of, zwisch)
    })
  } else {
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1, 70)))
      list_of <<- rbind(list_of, zwisch)
    })
  }
}

  if( type == "tables"){
    results_raw <- gen_api("catalogue/tables",  username = gen_auth_get()$username ,password = gen_auth_get()$password, selection = code, ...)

    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    }

    list_of <- data.frame()

    if(detailed){
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1, 40), "Time" = x$Time))
      list_of <<- rbind(list_of, zwisch)
    })
    } else {
      lapply(results_json$List, function(x){
        zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1, 70)))
        list_of <<- rbind(list_of, zwisch)
      })
    }
  }

  if( type == "timeseries"){
    results_raw <- gen_api("catalogue/timeseries",  username = gen_auth_get()$username ,password = gen_auth_get()$password, selection = code, ...)

    if(resp_content_type(results_raw) == "application/json"){
      results_json <<- resp_body_json(results_raw)
    }

    list_of <- data.frame()

    if(detailed){
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1, 40), "Time" = x$Time, "Latest_Update" = x$LatestUpdate, "State" = x$State, "Information" = x$Information))
      list_of <<- rbind(list_of, zwisch)
    })
  } else {
    lapply(results_json$List, function(x){
      zwisch <- rbind(c("Code" = x$Code, "Content" = substr(x$Content, 1, 70)))
      list_of <<- rbind(list_of, zwisch)
    })
  }
}


  list_of$Main <- apply(list_of, 1, function(x){
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 1)]
  })

  list_of$Main2 <- apply(list_of, 1, function(x){
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 2)]
  })

  list_of$Main3 <- apply(list_of, 1, function(x){
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 3)]
  })

  list_of$Main5 <- apply(list_of, 1, function(x){
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 5)]
  })

  nestedlist <- split(list_of, list_of$Main, drop = T)
  nestedlist <- lapply(nestedlist, function(x){
    split(x, x["Main2"], drop = T)
  })
  nestedlist <- lapply(nestedlist, function(x){
    lapply(x, function(y){
      split(y, y["Main3"])
    })})
  nestedlist <- lapply(nestedlist, function(x){
    lapply(x, function(y){
      lapply(y, function(z){
        split(z, z["Main5"])
      })
    })
  })

  aba <- lapply(
    nestedlist, function(d){
      lapply(d,  function(z){
        lapply(z,  function(y){
          lapply(y,  function(x){
            x[!(
              names(x)
              %in% c("Main", "Main2", "Main3", "Main5"))]})
        })
      })
    })

  list_resp <- list("Output" = aba)
  attr(list_resp, "Code") <-  results_json$Parameter$selection
  attr(list_resp, "Type") <-  type
  attr(list_resp, "Language") <-  results_json$Parameter$language
  attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <-  results_json$Copyright

  return(list_resp)
}

