#' gen_alternative_terms: Call For Similiar or Spelling Related Terms for Further Search
#'
#' @description Function to find search terms that are similar or related to one another and also represented in Genesis.
#'
#' @param term Character string. Maximum length of 15 characters. Term or word for which you are searching for alternative or related terms. Use of '*' as a placeholder is possible to generate broader search areas.
#' @param similarity Logical. Indicator if the output of the function should be sorted based on a Levenshtein edit distance based on the \code{adist()} function.
#' @param database Character string. Indicator if the Destatis or Zensus database is called.
#' @param ... Additional parameters for the Genesis API call. These parameters are only affecting the Genesis call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list with all recalled elements from Genesis. Attributes are added to the data.frame, describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find terms at Destatis that are similar (in spelling) to search term "bus"
#' # and sort them by Levenshtein edit distance
#' object <- gen_alternative_terms(term = "bus", similarity = TRUE, database = "destatis")
#'
#' # Find terms at Destatis that are related (in spelling) to search term "bus"
#' object <- gen_alternative_terms(term = "bus*", similarity = TRUE, database = "destatis")
#'
#' # Find terms at Zensus that are related (in spelling) to search term "bus"
#' object <- gen_alternative_terms(term = "bus*", similarity = TRUE, database = "zensus")
#' }
#'
gen_alternative_terms <- function(term = NULL,
                                  similarity = TRUE,
                                  database = c("zensus", "destatis"),
                                  ...) {

  caller <- as.character(match.call()[1])

  check_function_input(term = term,
                       similarity = similarity,
                       caller = caller)

#-------------------------------------------------------------------------------

  if( length(database) == 1 & database == "zensus" ){

    results_raw <- gen_zensus_api("catalogue/terms",
                           username = gen_zensus_auth_get()$username,
                           password = gen_zensus_auth_get()$password,
                           selection = term,
                           ...)

  } else if ( length(database) == 1 & database == "destatis" ){

    results_raw <- gen_api("catalogue/terms",
                           username = gen_auth_get()$username,
                           password = gen_auth_get()$password,
                           selection = term,
                           ...)

  } else {

    stop("Parameter 'database' has to be 'zensus' or 'destatis'.",
         call. = FALSE)

  }

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
                                                ignore.case = TRUE))]
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
