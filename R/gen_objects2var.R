#' gen_objects2var: Get Objects Related To Variable
#'
#' @description Function to find objects related to a variable in Genesis/Zensus.
#'
#' @param code A string with a maximum length of 15 characters. Code from a Genesis/Zensus-Object. Only one code per iteration.
#' @param database Character string. Indicator if the Genesis or Zensus database is called. Only one database can be addressed per function call. Default option is 'genesis'.
#' @param category A string. Includes specific Genesis-Object-types: 'tables', 'statistics', and 'cubes' - and specific Zensus-Object-types: "tables" and "statistics". All types that are specific for one database can be used together. Default option is to use all types that are possible for the specific database.
#' @param area A string. Indicator from which area of the database the results are called. In general, "all" is the appropriate solution. Default option is 'all'.
#' @param detailed A logical. Indicator if function should return the detailed output of the iteration including all object-related information or only a shortened output including only code and object title. The default is detailed = FALSE.
#' @param error.ignore  A logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response. Default option is 'FALSE'.
#' @param sortcriterion A string. Indicator if the output should be sorted by 'code' or 'content'. This is a parameter of the Genesis/Zensus API call itself. The default is "code".
#' @param verbose Logical. Indicator if the output of the function should include detailed messages and warnings. Default option is 'TRUE'. Set the parameter to 'FALSE' to suppress additional messages and warnings.
#' @param ... Additional parameters for the Genesis/Zensus API call. These parameters are only affecting the Genesis/Zensus call itself, no further processing. For more details see `vignette("additional_parameter")`.
#'
#' @return A list with all recalled elements from Genesis/Zensus. Based on the detailed-parameter it contains more or less information, but always includes the code of the object, the title, and the type of the object. This is done to facilitate further processing of the data. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find Tables for Variable "Kreise" and return detailed output
#' object <- gen_objects2var(code = "Kreise", category = "tables", detailed = T)
#'
#' # Find everything for Variable "GES"
#' object <- gen_objects2var(code = "GES")
#' # Default of detailed-parameter is FALSE, and default of the
#' # category-parameter is to include all object types.
#' }
#'
gen_objects2var <- function(code = NULL,
                            database = c("all", "genesis", "zensus", "regio"),
                            category = c("tables", "statistics", "cubes"),
                            area = c("all", "public", "user"),
                            detailed = FALSE,
                            error.ignore = FALSE,
                            sortcriterion = c("code", "content"),
                            verbose = TRUE,
                            ...) {

  caller <- as.character(match.call()[1])

  gen_fun <- test_database_function(database)

  check_function_input(code = code,
                       category = category,
                       detailed = detailed,
                       error.ignore = error.ignore,
                       database = gen_fun,
                       sortcriterion = sortcriterion,
                       caller = caller,
                       verbose = verbose)

  area <- match.arg(area)

  area <- switch(area, all = "all", public = "\u00F6ffentlich", user = "benutzer")

  sortcriterion <- match.arg(sortcriterion)

  #-----------------------------------------------------------------------------

  res <- lapply(gen_fun, function(db){

    if(verbose) {
      info <- paste("Started the processing of", rev_database_function(db), "database.")
      message(info)
    }

    #---------------------------------------------------------------------------
    if ("tables" %in% category) {

      par_list <-  list(
        endpoint = "catalogue/tables2variable",
        username = gen_auth_get(database = rev_database_function(db))$username,
        password = gen_auth_get(database = rev_database_function(db))$password,
        name = code,
        area = area,
        sortcriterion = sortcriterion,
        ...
      )

      results_raw <- do.call(db, par_list)

      results_json <- test_if_json(results_raw)

      empty_object <- test_if_error(results_json, para = error.ignore, verbose = verbose)


      if(isTRUE(empty_object)){

        df_tables <- "No 'tables' object found for your request."

      } else if(isFALSE(empty_object)){

        df_tables <- results_json$Status$Content

      } else if(empty_object == "DONE"){

        if (isTRUE(detailed)) {

          df_tables <- binding_lapply(results_json$List,
                                      characteristics = c("Code",
                                                          "Content",
                                                          "Time"))


        } else {

          df_tables <- binding_lapply(results_json$List,
                                      characteristics = c("Code",
                                                          "Content"))

        }

        df_tables$Object_Type <- "Table"

        df_tables <- tibble::as_tibble(df_tables)
      }
    }

    #---------------------------------------------------------------------------
    if ("statistics" %in% category) {

      par_list <-  list(
        endpoint = "catalogue/statistics2variable",
        username = gen_auth_get(database = rev_database_function(db))$username,
        password = gen_auth_get(database = rev_database_function(db))$password,
        name = code,
        area = area,
        sortcriterion = sortcriterion,
        ...
      )

      results_raw <- do.call(db, par_list)

      results_json <- test_if_json(results_raw)

      empty_object <- test_if_error(results_json, para = error.ignore, verbose = verbose)


      if(isTRUE(empty_object)){

        df_statistics <- "No 'statistics' object found for your request."

      } else if(isFALSE(empty_object)){

        df_statistics <- results_json$Status$Content

      } else if(empty_object == "DONE"){

        if (isTRUE(detailed)) {

          df_statistics <- binding_lapply(results_json$List,
                                          characteristics = c("Code",
                                                              "Content",
                                                              "Cubes",
                                                              "Information"))

        } else {

          df_statistics <- binding_lapply(results_json$List,
                                          characteristics = c("Code",
                                                              "Content"))
        }

        df_statistics$Object_Type <- "Statistic"

        df_statistics <- tibble::as_tibble(df_statistics)
      }
    }

    #---------------------------------------------------------------------------
    if ("cubes" %in% category && db == "gen_zensus_api") {

      df_cubes <- "No 'cubes' object available for 'zensus'-database."

      return(df_cubes)

    } else if ("cubes" %in% category && (db == "gen_api" || db == "gen_regio_api")) {

      results_raw <- do.call(db, list(
        endpoint = "catalogue/timeseries2variable",
        username = gen_auth_get(database = rev_database_function(db))$username,
        password = gen_auth_get(database = rev_database_function(db))$password,
        name = code,
        area = area,
        sortcriterion = sortcriterion,
        ...
      ))

      results_json <- test_if_json(results_raw)

      empty_object <- test_if_error(results_json, para = error.ignore, verbose = verbose)


      if(isTRUE(empty_object)){

        df_cubes <- "No 'cubes' object found for your request."

      } else if(isFALSE(empty_object)){

        df_cubes <- results_json$Status$Content

      } else if(empty_object == "DONE"){

        if (isTRUE(detailed)) {

          df_cubes <- binding_lapply(results_json$List,
                                     characteristics = c("Code",
                                                         "Content",
                                                         "Time",
                                                         "State",
                                                         "LatestUpdate",
                                                         "Information"))

        } else {

          df_cubes <- binding_lapply(results_json$List,
                                     characteristics = c("Code",
                                                         "Content"))
        }

        df_cubes$Object_Type <- "Cube"

        df_cubes <- tibble::as_tibble(df_cubes)
      }
    }

    #---------------------------------------------------------------------------
    # Summary
    if (all(c("tables", "statistics", "cubes") %in% category)) {

      list_resp <- list(
        "Tables" = df_tables,
        "Statistics" = df_statistics,
        "Cubes" = df_cubes
      )

    } else if (category == "tables") {

      list_resp <- df_tables

    } else if (category == "statistics") {

      list_resp <- df_statistics

    } else if (category == "cubes") {

      list_resp <- df_cubes

    }

    attr(list_resp, "Code") <- results_json$Parameter$term
    attr(list_resp, "Database") <- rev_database_function(db)
    attr(list_resp, "Category") <- category
    attr(list_resp, "Language") <- results_json$Parameter$language
    attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
    attr(list_resp, "Copyright") <- results_json$Copyright

    return(list_resp)

  })

  #-----------------------------------------------------------------------------

  res <- check_results(res)

  return(res)

}
