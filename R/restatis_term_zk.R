# Restatis - term function

running_out_of_terms <- function(term = NULL, 
                                 similarity = T,
                                  ...){
  if(!(is.character(term)) && length(term) < 1L && is.null(code)){
    stop("code must be a single string or NULL", call. = FALSE)
  }
  
  results_raw <- gen_api("catalogue/terms",  username = gen_auth_get()$username,password = gen_auth_get()$password, selection = term, ...)
  
  if(resp_content_type(results_raw) == "application/json"){
    results_term <<- resp_body_json(results_raw)
  } 
  
  if(length(results_term$List) == 0){
    cat("No related terms found for your code.")
  } else {
    #similarity von Woertern berechnen und nach diesen Ordnen? 
    termslist <- c()
    termslist <- lapply(results_term$List, function(x){append(termslist, x$Content)})
    termslist <- lapply(termslist, function(x){gsub("\\s+"," ",x)})
    termslist <- unlist(termslist)
    
    if(similarity){
      # generalized levenstein edit distance
      termslist <- termslist[order(adist(term, termslist, ignore.case = T))]
    } else {
      # nchar order
      termslist <- termslist[order(unlist(lapply(termslist, nchar)))]
    }
   }
  
  list_resp <- list("Output" = termslist) 
  attr(list_resp, "Term") <-  results_json$Parameter$term
  attr(list_resp, "Language") <-  results_json$Parameter$language
  attr(list_resp, "Pagelength") <-  results_json$Parameter$pagelength
  attr(list_resp, "Copyrigtht") <-  results_json$Copyright
  
  return(list_resp)
}

