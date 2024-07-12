#' gen_alternative_terms
#'
#' @description Function to find search terms that are similar or related to one another in spelling and also represented in the GENESIS, Zensus 2022 or regionalstatistik.de databases. Important note: The API call is searching for terms with the same characters. To be useful in searching for related terms it is highly recommended to work with "*" placeholders (see examples). The placeholder can be placed before and/or after the search term.
#'
#' @param term Character string. Maximum length of 15 characters. Term or word for which you are searching for alternative or related terms. Use of '*' as a placeholder is possible to generate broader search areas.
#' @param similarity Boolean. Indicator if the output of the function should be sorted based on a Levenshtein edit distance based on the \code{adist()} function. Default is 'TRUE'.
#' @param database Character string. Indicator if the GENESIS ('genesis'), Zensus 2022 ('zensus') or regionalstatistik.de ('regio') database is called. Only one database can be addressed per function call. Default option is 'genesis'.
#' @param verbose Boolean. Indicator if the output of the function should include detailed messages and warnings. Default option is 'TRUE'. Set the parameter to 'FALSE' to suppress additional messages and warnings.
#' @param ... Additional parameters for the API call. These parameters are only affecting the call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list with all recollected elements from the respective database. Attributes are added to the data.frame, describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find terms at GENESIS that are the same (in spelling) to search term "bus"
#' # and sort them by Levenshtein edit distance
#' object <- gen_alternative_terms(term = "bus", similarity = TRUE, database = "genesis")
#'
#' # Find terms at GENESIS that are related (in spelling) to search term "bus"
#' object <- gen_alternative_terms(term = "bus*", similarity = TRUE, database = "genesis")
#'
#' # Find terms at Zensus 2022 that are related (in spelling) to search term "wohn"
#' object <- gen_alternative_terms(term = "wohn*", similarity = TRUE, database = "zensus")
#' }
#'
gen_alternative_terms <- function(term = NULL,
                                  similarity = TRUE,
                                  database = c("all", "genesis", "zensus", "regio"),
                                  verbose = TRUE,
                                  ...) {

  caller <- as.character(match.call()[1])

  check_function_input(verbose = verbose)

  gen_fun <- test_database_function(database,
                                    error.input = T,
                                    text = verbose)

  check_function_input(term = term,
                       similarity = similarity,
                       caller = caller,
                       verbose = verbose)

  #-----------------------------------------------------------------------------

  res <- lapply(gen_fun, function(db){

    if (verbose) {

      info <- paste("Started the processing of", rev_database_function(db), "database.")

      message(info)

    }

    par_list <-  list(endpoint = "catalogue/terms",
                      username = gen_auth_get(database = rev_database_function(db))$username,
                      password = gen_auth_get(database = rev_database_function(db))$password,
                      selection = term,
                      ...)

    results_raw <- do.call(db, par_list)

    #---------------------------------------------------------------------------

    results_json <- test_if_json(results_raw)

    if (length(results_json$List) == 0  & length(gen_fun) == 1) {

      stop("No related terms found for your code.", call. = FALSE)

    } else if (length(results_json$List) == 0  & length(gen_fun) > 1) {

      termslist <- "No related terms found for your code."

      list_resp <- list("Output" = termslist)

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

      #-------------------------------------------------------------------------

      if (isTRUE(similarity)) {

        # generalized Levenshtein edit distance
        termslist <- termslist[order(utils::adist(term,
                                                  termslist,
                                                  ignore.case = TRUE))]
      } else {

        # nchar order
        termslist <- termslist[order(unlist(lapply(termslist, nchar)))]

      }

      list_resp <- list("Output" = termslist)

    }

    attr(list_resp, "Term") <- term
    attr(list_resp, "Database") <- rev_database_function(db)
    attr(list_resp, "Language") <- results_json$Parameter$language
    attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
    attr(list_resp, "Copyright") <- results_json$Copyright

    return(list_resp)

  })

  res <- check_results(res)

  return(res)

}
