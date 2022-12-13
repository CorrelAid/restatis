#' Call For Similiar/Spelling Related Terms To Search
#'
#' Function for findings search terms which are similar or related to another and also represented in Destatis.
#'
#' @param term a string with a maximum length of 15 characters. Term or word for which you are searching for alternatives or related terms. Use of '*' as a placeholder is possible to generate broader searching areas.
#' @param similarity a logical. Indicator if the output of the function should be sorted based on a Levenshtein edit distance based on the adist-function.
#' @param ... Additional parameter of the Destatis call. These parameters are only affecting the Destatis call itself, no further processing.
#'
#' @return A list with all recalled elements from Destatis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' # Find terms that are similar (in spelling) to my search term "bus" and sort it by Levenshtein edit distance
#' object <- running_out_of_terms(term = "bus", similarity = T)
#'
#' # Find terms that are related (in spelling) to my search term "bus"
#' object <- running_out_of_terms(term = "bus*", similarity = T)
#'
running_out_of_terms <- function(term = NULL,
                                 similarity = T,
                                 ...) {
  if (!(is.character(term)) && length(term) < 1L && is.null(code)) {
    stop("code must be a single string or NULL", call. = T)
  }

  results_raw <- gen_api("catalogue/terms",
    username = gen_auth_get()$username,
    password = gen_auth_get()$password,
    selection = term, ...
  )

  if (httr2::resp_content_type(results_raw) == "application/json") {
    results_term <- httr2::resp_body_json(results_raw)
  }

  if (results_json$Status$Code != 0) {
    message(results_json$Status$Content)
  }

  if (length(results_term$List) == 0) {
    message("No related terms found for your code.")
  } else {
    # similarity von Woertern berechnen und nach diesen Ordnen?
    termslist <- c()
    termslist <- lapply(results_term$List, function(x) {
      append(termslist, x$Content)
    })
    termslist <- lapply(termslist, function(x) {
      gsub("\\s+", " ", x)
    })
    termslist <- unlist(termslist)

    if (similarity) {
      # generalized levenstein edit distance
      termslist <- termslist[order(utils::adist(term, termslist,
        ignore.case = T
      ))]
    } else {
      # nchar order
      termslist <- termslist[order(unlist(lapply(termslist, nchar)))]
    }

    list_resp <- list("Output" = termslist)
    attr(list_resp, "Term") <- term
    attr(list_resp, "Language") <- results_json$Parameter$language
    attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
    attr(list_resp, "Copyrigtht") <- results_json$Copyright

    return(list_resp)
  }
}
