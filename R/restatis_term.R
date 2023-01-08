#' Call for similiar or spelling related terms for further search
#'
#' @description Function to find search terms that are similar or related to one another and also represented in Genesis.
#'
#' @param term Character string. Maximum length of 15 characters. Term or word for which you are searching for alternative or related terms. Use of '*' as a placeholder is possible to generate broader search areas.
#' @param similarity Logical. Indicator if the output of the function should be sorted based on a Levenshtein edit distance based on the \code{adist()} function.
#' @param ... Additional parameters for the Genesis API call. These parameters are only affecting the Genesis call itself, no further processing.
#'
#' @return A list with all recalled elements from Genesis. Attributes are added to the data.frame, describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find terms that are similar (in spelling) to search term "bus" and sort them by Levenshtein edit distance
#' object <- running_out_of_terms(term = "bus", similarity = TRUE)
#'
#' # Find terms that are related (in spelling) to search term "bus"
#' object <- running_out_of_terms(term = "bus*", similarity = TRUE)
#' }
#'
running_out_of_terms <- function(term = NULL,
                                 similarity = TRUE,
                                 ...) {

  if (length(term) != 1L) {

    stop("Parameter 'term' must be a single string.", call. = FALSE)

  }

  if (!is.logical(similarity)) {

    stop("Parameter 'similarity' has to be of type logical.", call. = FALSE)

  }

  if (!is.null(term) & !is.character(term)) {

    stop("Parameter 'term' has to be of type character or NULL.", call. = FALSE)

  }

#-------------------------------------------------------------------------------

  results_raw <- gen_api("catalogue/terms",

    username = gen_auth_get()$username,
    password = gen_auth_get()$password,
    selection = term,
    ...)

  if (httr2::resp_content_type(results_raw) == "application/json") {

    results_term <- httr2::resp_body_json(results_raw)

  }

  if (results_term$Status$Code != 0) {

    stop(results_term$Status$Content, call. = FALSE)

  }

  if (length(results_term$List) == 0) {

    stop("No related terms found for your code.", call. = FALSE)

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

    if (isTRUE(similarity)) {

      # generalized levenstein edit distance
      termslist <- termslist[order(utils::adist(term,
                                                termslist,
                                                ignore.case = TRUE))]
    } else {

      # nchar order
      termslist <- termslist[order(unlist(lapply(termslist, nchar)))]

    }

    list_resp <- list("Output" = termslist)

    attr(list_resp, "Term") <- term
    attr(list_resp, "Language") <- results_term$Parameter$language
    attr(list_resp, "Pagelength") <- results_term$Parameter$pagelength
    attr(list_resp, "Copyright") <- results_term$Copyright

    return(list_resp)

  }

}
