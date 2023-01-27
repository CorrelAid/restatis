.onLoad <- function(libname, pkgname) {
  gen_api <<- memoise::memoise(gen_api)
}

# (ZK): Ideas for refactoring functions ####
# json response ####
test_if_json <- function(input){
  if (httr2::resp_content_type(input) == "application/json") {

    results_json <- httr2::resp_body_json(input)

  } else {
    stop("No json-csv file detected")
  }

  return(results_json)
}

# error response ####
test_if_error <- function(input, para){
  if (input$Status$Code != 0 & isTRUE(para)) {
    stop(input$Status$Content)
  } else if (input$Status$Code != 0 & isFALSE(para)){
    message(input$Status$Content)
    message("Artificial token is used.")
    empty_object <- FALSE
  } else {
    empty_object <- "DONE"
  }
  return(empty_object)
}


# process further ####
test_if_process_further <- function(input, para){
  if (sum(unlist(lapply(input[4:8], function(x) {
    is.null(x)
  }))) == 5 & isFALSE(para)) {
    stop("No object found for your request. Check your parameters if you expected an object for this request.")
  } else if (sum(unlist(lapply(input[4:8], function(x) {
    is.null(x)
  }))) == 5 & isTRUE(para)){
    message("No object found for your request. Check your parameters if you expected an object for this request. Artificial token is used.")
    empty_object <- TRUE
  }
  return(empty_object)
}

# binding_function ####
binding_lapply <- function(x,
                           characteristics){
  list_of <- setNames(data.frame(matrix(ncol = length(characteristics), nrow = 0)), characteristics)

  lapply(x, function(x){
    zwisch <- unlist(x[characteristics])

    list_of <<- rbind(list_of, zwisch[characteristics])

  })

  colnames(list_of) <- characteristics

  return(list_of)
}

# bind_rows ####
# map_dfr ####
# It seems that the current version and the older version are not significantly different in execution time (seconds focused)

# gsub function ####
ggsub <- function(x){
  a <- gsub(".*:", "", x$Content)
  return(a)
}

# spezifisch function ####
spezifisch_create <- function(x){
  a <- unlist(lapply(strsplit(x$Spezifisch, ","), length))
  return(a)
}

# Titel function ####
titel_search <- function(x, term){
  a <- grepl(
    paste(
      unlist(
        strsplit(term,
                 c(" & | und "))),
      collapse = "|"),
    x$Content,
    ignore.case = T
  )

  return(a)
}


# test if error meta ___ identical to test_if_error from cataloge
test_if_error_meta <- function(input, para){

  if (input$Status$Code == 104 & isFALSE(para)){
    stop("No object found for your request. Check your parameters if you expected an object for this request.")
  } else if (input$Status$Code != 0 & isFALSE(para)){
    stop(input$Status$Content)
  } else if (input$Status$Code == 104 & isTRUE(para)){
    message("No object found for your request. Check your parameters if you expected an object for this request. Artificial token is used.")
    empty_object <- TRUE
  } else if (input$Status$Code != 0 & isTRUE(para)){
    message(input$Status$Content)
    message("Artificial token is used.")
    empty_object <- FALSE
  } else {
    empty_object <- "DONE"
  }

  return(empty_object)
}
