#' gen_alternative_terms: Call For Similiar or Spelling Related Terms for Further Search
#'
#' @description Function to find search terms that are similar or related to one another and also represented in Genesis/Zensus.
#'
#' @param term Character string. Maximum length of 15 characters. Term or word for which you are searching for alternative or related terms. Use of '*' as a placeholder is possible to generate broader search areas.
#' @param similarity Logical. Indicator if the output of the function should be sorted based on a Levenshtein edit distance based on the \code{adist()} function. Default option is 'TRUE'.
#' @param database Character string. Indicator if the Genesis or Zensus database is called. Only one database can be addressed per function call. Default option is 'genesis'.
#' @param ... Additional parameters for the Genesis or Zensus API call. These parameters are only affecting the call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list with all recalled elements from Genesis/Zensus. Attributes are added to the data.frame, describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find terms at Destatis that are similar (in spelling) to search term "bus"
#' # and sort them by Levenshtein edit distance
#' object <- gen_alternative_terms(term = "bus", similarity = TRUE, database = "genesis")
#'
#' # Find terms at Destatis that are related (in spelling) to search term "bus"
#' object <- gen_alternative_terms(term = "bus*", similarity = TRUE, database = "genesis")
#'
#' # Find terms at Zensus that are related (in spelling) to search term "bus"
#' object <- gen_alternative_terms(term = "bus*", similarity = TRUE, database = "zensus")
#' }
#'
gen_alternative_terms <- function(term = NULL,
                                  similarity = TRUE,
                                  database = c("genesis", "zensus"),
                                  ...) {

  caller <- as.character(match.call()[1])

  gen_fun <- test_database_function(database)

  check_function_input(term = term,
                       similarity = similarity,
                       caller = caller)

  #-----------------------------------------------------------------------------

  if(gen_fun == "gen_api"){

    par_list <-  list(
      endpoint = "catalogue/terms",
      username = gen_auth_get()$username,
      password = gen_auth_get()$password,
      selection = term,
      ...
    )

  } else if ( gen_fun == "gen_zensus_api"){

    par_list <-  list(
      endpoint = "catalogue/terms",
      username = gen_zensus_auth_get()$username,
      password = gen_zensus_auth_get()$password,
      selection = term,
      ...
    )

  }

  results_raw <- do.call(gen_fun, par_list)

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
    attr(list_resp, "Database") <- database[1]
    attr(list_resp, "Language") <- results_json$Parameter$language
    attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
    attr(list_resp, "Copyright") <- results_json$Copyright

    return(list_resp)

  }

}
