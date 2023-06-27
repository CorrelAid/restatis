#' Search for Related Terms Based on the Same Specific String
#'
#' @description Function to find terms in GENESIS that are similar or related based on a simple comparison of strings. This can help to identify alternative search terms.
#'
#' @param term Character string. Maximum length of 15 characters. Term or word for which you are searching for alternative or related terms. Use of '*' as a placeholder is possible to generate broader search areas.
#' @param similarity Logical. Indicator if the output of the function should be sorted based on a Levenshtein edit distance based on the \code{adist()} function.
#' @param ... Additional parameter of the GENESIS API call. These parameters are only affecting the GENESIS API call itself, no further processing.
#'
#' @return A list with all recalled elements from GENESIS. Attributes are added to the data.frame, describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find terms that are similar (in spelling) to search term "bus"
#' # and sort them by Levenshtein edit distance
#' object <- gen_alternative_terms(term = "bus", similarity = TRUE)
#'
#' # Find terms that are related (in spelling) to search term "bus"
#' object <- gen_alternative_terms(term = "bus*", similarity = TRUE)
#' }
#'
gen_alternative_terms <- function(term = NULL,
                                  similarity = TRUE,
                                  ...) {
  caller <- as.character(match.call()[1])

  check_function_input(
    term = term,
    similarity = similarity,
    caller = caller
  )

  #-------------------------------------------------------------------------------

  results_raw <- gen_api("catalogue/terms",
    selection = term,
    ...
  )

  results_json <- test_if_json(results_raw)

  if (length(results_json$List) == 0) {
    stop("No related terms found for your code.", call. = FALSE)
  } else {
    # similarity von Woertern berechnen und nach diesen Ordnen?
    termslist <- c()

    termslist <- lapply(results_json$List, function(x) {
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
        ignore.case = TRUE
      ))]
    } else {
      # nchar order
      termslist <- termslist[order(unlist(lapply(termslist, nchar)))]
    }

    list_resp <- list("Output" = termslist)

    attr(list_resp, "Term") <- term
    attr(list_resp, "Language") <- results_json$Parameter$language
    attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
    attr(list_resp, "Copyright") <- results_json$Copyright

    return(list_resp)
  }
}
