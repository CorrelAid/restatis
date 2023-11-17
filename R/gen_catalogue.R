#' catalogue: Explore Different Objects and Their Structural Embedding in Genesis/Zensus
#'
#' Function to enable searching for tables, statistics, and cubes from Genesis or Zensus. Additionally, it structures the Genesis-output based on the internal tree structure of Genesis itself based on the EVAS-numbers. Time-series are represented as cubes with a specified time span in Genesis.
#'
#' @param code A string with a maximum length of 10 characters for a Genesis-Object and 15 characters for a Zensus-Object. Only one code per iteration. "*"-Notations are possible.
#' @param database Character string. Indicator if the Genesis or Zensus database is called. Only one database can be addressed per function call. Default option is 'genesis'.
#' @param category A string. Includes specific Genesis-Object-types: 'tables', 'statistics', and 'cubes' - and specific Zensus-Object-types: "tables" and "statistics". All types that are specific for one database can be used together. Default option is to use all types that are possible for the specific database.
#' @param area A string. Indicator from which area of the database the results are called. In general, "all" is the appropriate solution. Default option is 'all'. Not used for "statistics".
#' @param detailed A logical. Indicator if the function should return the detailed output of the iteration including all object-related information or only a shortened output including only code and object title. Default option is 'FALSE'.
#' @param error.ignore  A logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response. Default option is 'FALSE'.
#' @param sortcriterion A string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the Genesis/Zensus API call itself. The default is "code".
#' @param ... Additional parameters for the Genesis/Zensus API call. These parameters are only affecting the Genesis/Zensus call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list with all recalled elements from Genesis/Zensus API. Based on the detailed-parameter it contains more or less information, but always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing with the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Scroll through Objects under the topic "12*"
#' # which is "Bevölkerung" in Destatis from all categories and
#' # with a detailed output
#' object <- gen_catalogue(code = "12*", detailed = T)
#'
#' # Search tables under the topic "12*" which is "Bevölkerung"
#' # without a detailed output
#' object <- gen_catalogue(code = "12*", category = "tables")
#' }
#'
gen_catalogue <- function(code = NULL,
                          database = c("genesis", "zensus"),
                          category = c("tables", "statistics", "cubes"),
                          area = c("all", "public", "user"),
                          detailed = FALSE,
                          error.ignore = FALSE,
                          sortcriterion = c("code", "content"),
                          ...) {

  caller <- as.character(match.call()[1])

  gen_fun <- test_database_function(database)

  check_function_input(code = code,
                       category = category,
                       detailed = detailed,
                       error.ignore = error.ignore,
                       database = gen_fun,
                       sortcriterion = sortcriterion,
                       caller = caller)

  area <- match.arg(area)

  area <- switch(area, all = "all", public = "\u00F6ffentlich", user = "benutzer")

  sortcriterion <- match.arg(sortcriterion)

  #-----------------------------------------------------------------------------

  # Processing ####
  if ("cubes" %in% category && gen_fun == "gen_zensus_api") {

    list_of_cubes <- "No 'cubes' object available for 'zensus' database."

  } else if ("cubes" %in% category && gen_fun == "gen_api") {

    results_raw <- do.call(gen_fun, list(
                             endpoint = "catalogue/cubes",
                             username = gen_auth_get()$username,
                             password = gen_auth_get()$password,
                             selection = code,
                             sortcriterion = sortcriterion,
                             area = area,
                             ...
                           ))

    results_json <- test_if_json(results_raw)

    empty_object <- test_if_error(results_json, para = error.ignore)

    if(isTRUE(empty_object)){

      list_of_cubes <- "No 'cubes' object found for your request."

    } else if(isFALSE(empty_object)){

      list_of_cubes <- results_json$Status$Content

    } else if(empty_object == "DONE"){

    if (isTRUE(detailed)) {

      list_of_cubes <- binding_lapply(results_json$List,
                                      characteristics = c("Code",
                                                          "Content",
                                                          "Time",
                                                          "LatestUpdate",
                                                          "State",
                                                          "Information"))

    } else {

      list_of_cubes <- binding_lapply(results_json$List,
                                      characteristics = c("Code",
                                                          "Content"))



    }

    list_of_cubes$Object_Type <- "Cube"

    list_of_cubes <- tibble::as_tibble(list_of_cubes)

    }
  }


  #-----------------------------------------------------------------------------

  if ("statistics" %in% category) {

    if(gen_fun == "gen_api"){

      par_list <-  list(
        endpoint = "catalogue/statistics",
        username = gen_auth_get()$username,
        password = gen_auth_get()$password,
        selection = code,
        sortcriterion = sortcriterion,
        ...
      )

    } else if ( gen_fun == "gen_zensus_api"){

      par_list <-  list(
        endpoint = "catalogue/statistics",
        username = gen_zensus_auth_get()$username,
        password = gen_zensus_auth_get()$password,
        selection = code,
        sortcriterion = sortcriterion,
        ...
      )

    }

    results_raw <- do.call(gen_fun, par_list)

    results_json <- test_if_json(results_raw)

    empty_object <- test_if_error(results_json, para = error.ignore)

    if(isTRUE(empty_object)){

      list_of.stats <- "No 'statistics' object found for your request."

    } else if(isFALSE(empty_object)){

      list_of.stats <- results_json$Status$Content

    } else if(empty_object == "DONE"){

    if (isTRUE(detailed)) {

      list_of.stats <- binding_lapply(results_json$List,
                                      characteristics = c("Code",
                                                          "Content",
                                                          "Cubes",
                                                          "Information"))


    } else {

      list_of.stats <- binding_lapply(results_json$List,
                                      characteristics = c("Code",
                                                          "Content"))



    }

    list_of.stats$Object_Type <- "Statistic"

    list_of.stats <- tibble::as_tibble(list_of.stats)

    }
  }



  #-----------------------------------------------------------------------------

  if ("tables" %in% category) {

    if(gen_fun == "gen_api"){

      par_list <-  list(
        endpoint = "catalogue/tables",
        username = gen_auth_get()$username,
        password = gen_auth_get()$password,
        selection = code,
        area = area,
        sortcriterion = sortcriterion,
        ...
      )

    } else if (gen_fun == "gen_zensus_api"){

      par_list <-  list(
        endpoint = "catalogue/tables",
        username = gen_zensus_auth_get()$username,
        password = gen_zensus_auth_get()$password,
        selection = code,
        area = area,
        sortcriterion = sortcriterion,
        ...
      )

    }

    results_raw <- do.call(gen_fun, par_list)

    results_json <- test_if_json(results_raw)

    empty_object <- test_if_error(results_json, para = error.ignore)

    if(isTRUE(empty_object)){

      list_of.tabs <- "No 'tables' object found for your request."

    } else if(isFALSE(empty_object)){

      list_of.tabs <- results_json$Status$Content

    } else if(empty_object == "DONE"){

    if (isTRUE(detailed)) {

      list_of.tabs <- binding_lapply(results_json$List,
                                      characteristics = c("Code",
                                                          "Content",
                                                          "Time"))



    } else {

      list_of.tabs <- binding_lapply(results_json$List,
                                      characteristics = c("Code",
                                                          "Content"))


    }

    list_of.tabs$Object_Type <- "Table"

    list_of.tabs <- tibble::as_tibble(list_of.tabs)

    }
  }


  #-----------------------------------------------------------------------------

  # Summary ####
  if (all(c("tables", "statistics", "cubes") %in% category)) {

    list_resp <- list(
                  "Cubes" = if(length(list_of_cubes) == 1 | gen_fun == "gen_zensus_api"){tibble::as_tibble(list_of_cubes)} else {forming_evas(list_of_cubes)},
                  "Statistics" = if(length(list_of.stats) == 1 | gen_fun == "gen_zensus_api"){tibble::as_tibble(list_of.stats)} else {forming_evas(list_of.stats)},
                  "Tables" = if(length(list_of.tabs) == 1 | gen_fun == "gen_zensus_api"){tibble::as_tibble(list_of.tabs)} else {forming_evas(list_of.tabs)}
                  )

  } else if (category == "cubes") {

    if(length(list_of_cubes) == 1 | gen_fun == "gen_zensus_api"){

      list_resp <- list("Cubes" = tibble::as_tibble(list_of_cubes))

    } else {

      list_resp <- list("Cubes" = forming_evas(list_of_cubes))

    }

  } else if (category == "statistics") {

    if(length(list_of.stats) == 1 | gen_fun == "gen_zensus_api"){

      list_resp <- list("Statistics" = tibble::as_tibble(list_of.stats))

    } else {

      list_resp <- list("Statistics" = forming_evas(list_of.stats))

    }

  } else if (category == "tables") {

    if(length(list_of.tabs) == 1 | gen_fun == "gen_zensus_api"){

      list_resp <- list("Tables" = tibble::as_tibble(list_of.tabs))

    } else {

      list_resp <- list("Tables" = forming_evas(list_of.tabs))

    }

  }

  attr(list_resp, "Code") <- results_json$Parameter$selection
  attr(list_resp, "Database") <- database[1]
  attr(list_resp, "Category") <- category
  attr(list_resp, "Language") <- results_json$Parameter$language
  attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
  attr(list_resp, "Copyright") <- results_json$Copyright

  return(list_resp)

}
