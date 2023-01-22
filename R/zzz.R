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
test_if_error <- function(input){
  if (input$Status$Code != 0) {

    warning(input$Status$Content, call. = FALSE)

  }
}

# process further ####
test_if_process_further <- function(input){
  if (sum(unlist(lapply(input[4:8], function(x) {
    is.null(x)
  }))) == 5) {
    stop("No related terms found for your code.")
  }
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
