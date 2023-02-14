#' gen_find: Search Function For Objects In Genesis
#'
#' Function to search through Genesis. It is similar in usage as the search function on the Destatis mainpage (https://www.destatis.de/DE/Home/_inhalt.html).
#' In the search query "AND" and "OR" can be included. An additional information here is that time-series are treated as cubes - they are not longer distinguished. If you want to find a specific object with a clear code with this find-function you need to specify the object type or search for all object types.
#'
#' @param term a string with no maximum character length.
#' @param category a string. Specific object-types: 'tables', 'statistics', "variables", and 'cubes'. Using all together is possible. Default option are "all" objects.
#' @param detailed a logical. Indicator if the function should return the detailed output of the iteration including all object-related information or only a shortened output including only code and object title. Default Option is detailed = FALSE.
#' @param ordering a logical. Indicator if the function should return the output of the iteration ordered first based on the fact if the searched term is appearing in the title of the object and in second on an estimator of the number of variables in this object. Default option is ordering = TRUE.
#' @param error.ignore  a logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response.
#' @param ... Additional parameter of the Genesis API call. These parameters are only affecting the Genesis API call itself, no further processing.
#'
#' @return A list with all recalled elements from Genesis. Attributes are added to the dataframe describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find objects related to "bus" in Genesis
#' object <- gen_find(term = "bus")
#'
#' # Find tables related to "bus" in Genesis and return a unordered detailed output
#' object <- gen_find(term = "bus", detailed = T, ordering = F)
#' }
gen_find <- function(term = NULL,
                       category = c("all", "tables", "statistics", "variables", "cubes"),
                       detailed = FALSE,
                       ordering = TRUE,
                       error.ignore = FALSE,
                       ...) {
  # Check of parameter ####
  if (!(is.character(term)) && length(term) < 1L && is.null(term)) {
    stop("term must be a single string or NULL", call. = F)
  }

  if (detailed == FALSE) {
    message("Use detailed = TRUE to obtain the complete output.")
  }

  if (!all(category %in% c("all", "tables", "statistics", "variables", "cubes"))) {
    stop("category must be one of the offered options", call. = F)
  }

  category <- match.arg(category)

  if (!(is.logical(detailed))) {
    stop("parameter has to be logical", call. = F)
  }

  if (!(is.logical(ordering))) {
    stop("parameter has to be logical", call. = F)
  }

  if (!(is.logical(error.ignore))) {
    stop("parameter has to be logical", call. = F)
  }

  # Data ####
  results_raw <- gen_api("find/find",
                         username = gen_auth_get()$username,
                         password = gen_auth_get()$password,
                         term = term,
                         category = category,
                         ...)

  results_json <- test_if_json(results_raw)

  empty_object <- test_if_error(results_json, para = error.ignore)

  empty_object <- test_if_process_further(results_json, para = error.ignore)

  if(isTRUE(empty_object)){
    list_resp <- list(
      "Output" = "No object found for your request.")

    attr(list_resp, "Term") <- results_json$Parameter$term
    attr(list_resp, "Language") <- results_json$Parameter$language
    attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
    attr(list_resp, "Copyrigtht") <- results_json$Copyright

    return(list_resp)

  } else if(isFALSE(empty_object)){
    list_resp <- list(
      "Output" = results_json$Status$Content)

    attr(list_resp, "Term") <- results_json$Parameter$term
    attr(list_resp, "Language") <- results_json$Parameter$language
    attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
    attr(list_resp, "Copyrigtht") <- results_json$Copyright

    return(list_resp)
  } else if(empty_object == "DONE"){
  if (detailed == T) {
    if (category == "all") {
      # Table ####
      df_table <- binding_lapply(results_json$Tables,
                                 characteristics = c("Code",
                                                     "Content",
                                                     "Time"))

      df_table$Spezifisch <- ggsub(df_table)

      df_table$Variablen <- spezifisch_create(df_table)

      df_table$Object_Type <- "Table"

      # Statistic ####
      df_stats <- binding_lapply(results_json$Statistics,
                                 characteristics = c("Code",
                                                     "Content",
                                                     "Information",
                                                     "Cubes"))

      df_stats$Spezifisch <- ggsub(df_stats)

      df_stats$Variablen <- spezifisch_create(df_stats)

      df_stats$Object_Type <- "Statistic"

      # Variables ####
      df_variables <- binding_lapply(results_json$Variables,
                                     characteristics = c("Code",
                                                         "Content",
                                                         "Type",
                                                         "Values",
                                                         "Information"))

      df_variables$Spezifisch <- ggsub(df_variables)

      df_variables$Variablen <- spezifisch_create(df_variables)

      df_variables$Object_Type <- "Variable"

      # Cubes ####
      df_cubes <- binding_lapply(results_json$Cubes,
                                 characteristics = c("Code",
                                                     "Content",
                                                     "Time",
                                                     "LatestUpdate",
                                                     "State",
                                                     "Information"))

      df_cubes$Spezifisch <- ggsub(df_cubes)

      df_cubes$Variablen <- spezifisch_create(df_cubes)

      df_cubes$Object_Type <- "Cube"

      # Search for title-term match ####
      if (nrow(df_table) != 0) {
        df_table$Titel <- titel_search(df_table, term)
      }
      if (nrow(df_stats) != 0) {
        df_stats$Titel <- titel_search(df_stats, term)
      }
      if (nrow(df_variables) != 0) {
        df_variables$Titel <- titel_search(df_variables, term)
      }
      if (nrow(df_cubes) != 0) {
        df_cubes$Titel <- titel_search(df_cubes, term)
      }

      # Ordering ####
      if(ordering){
        df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c("Code",
                                                                          "Content",
                                                                          "Titel",
                                                                          "Time",
                                                                          "Variablen",
                                                                          "Spezifisch",
                                                                          "Object_Type")]

        df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c( "Code",
                                                                           "Content",
                                                                           "Titel",
                                                                           "Information",
                                                                           "Cubes",
                                                                           "Variablen",
                                                                           "Spezifisch",
                                                                           "Object_Type")]

        df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c( "Code",
                                                                                       "Content",
                                                                                       "Titel",
                                                                                       "Values",
                                                                                       "Information",
                                                                                       "Variablen",
                                                                                       "Spezifisch",
                                                                                       "Object_Type")]

        df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c( "Code",
                                                                           "Content",
                                                                           "Titel",
                                                                           "Time",
                                                                           "LatestUpdate",
                                                                           "State",
                                                                           "Information",
                                                                           "Variablen",
                                                                           "Spezifisch",
                                                                           "Object_Type")]
      } else {

        df_table <- df_table[, c("Code",
                                 "Content",
                                 "Titel",
                                 "Time",
                                 "Variablen",
                                 "Spezifisch",
                                 "Object_Type")]

        df_stats <- df_stats[, c("Code",
                                 "Content",
                                 "Titel",
                                 "Information",
                                 "Cubes",
                                 "Variablen",
                                 "Spezifisch",
                                 "Object_Type")]

        df_variables <- df_variables[, c("Code",
                                         "Content",
                                         "Titel",
                                         "Values",
                                         "Information",
                                         "Variablen",
                                         "Spezifisch",
                                         "Object_Type")]

        df_cubes <- df_cubes[, c("Code",
                                 "Content",
                                 "Titel",
                                 "Time",
                                 "LatestUpdate",
                                 "State",
                                 "Information",
                                 "Variablen",
                                 "Spezifisch",
                                 "Object_Type")]

      }

      # Combine ####
      list_resp <- list(
        "Tables" = tibble::as_tibble(df_table), "Statistics" = tibble::as_tibble(df_stats),
        "Variables" = tibble::as_tibble(df_variables), "Cubes" = tibble::as_tibble(df_cubes))

      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright

      return(list_resp)
    }
    if (category == "tables") {
      # Table ####
      df_table <- binding_lapply(results_json$Tables,
                                 characteristics = c("Code",
                                                     "Content",
                                                     "Time"))

      df_table$Spezifisch <- ggsub(df_table)

      df_table$Variablen <- spezifisch_create(df_table)

      df_table$Object_Type <- "Table"

      # Search for title-term match ####
      if (nrow(df_table) != 0) {
        df_table$Titel <- titel_search(df_table, term)
      }

      # Ordering ####
      if(ordering){
        df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c("Code",
                                                                          "Content",
                                                                          "Titel",
                                                                          "Time",
                                                                          "Variablen",
                                                                          "Spezifisch",
                                                                          "Object_Type")]
      } else {

        df_table <- df_table[, c("Code",
                                 "Content",
                                 "Titel",
                                 "Time",
                                 "Variablen",
                                 "Spezifisch",
                                 "Object_Type")]
      }

      # Combine ####
      list_resp <- list("Tables" = tibble::as_tibble(df_table))

      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright

      return(list_resp)

    }
    if (category == "statistics") {
      # Statistic ####
      df_stats <- binding_lapply(results_json$Statistics,
                                 characteristics = c("Code",
                                                     "Content",
                                                     "Information",
                                                     "Cubes"))

      df_stats$Spezifisch <- ggsub(df_stats)

      df_stats$Variablen <- spezifisch_create(df_stats)

      df_stats$Object_Type <- "Statistic"

      # Search for title-term match ####
      if (nrow(df_stats) != 0) {
        df_stats$Titel <- titel_search(df_stats, term)
      }

      # Ordering ####
      if(ordering){
        df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c( "Code",
                                                                           "Content",
                                                                           "Titel",
                                                                           "Information",
                                                                           "Cubes",
                                                                           "Variablen",
                                                                           "Spezifisch",
                                                                           "Object_Type")]

      } else {
        df_stats <- df_stats[, c("Code",
                                 "Content",
                                 "Titel",
                                 "Information",
                                 "Cubes",
                                 "Variablen",
                                 "Spezifisch",
                                 "Object_Type")]

      }

      # Combine ####
      list_resp <- list("Statistics" = tibble::as_tibble(df_stats))

      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright

      return(list_resp)




    }
    if (category == "variables") {
      # Variables ####
      df_variables <- binding_lapply(results_json$Variables,
                                     characteristics = c("Code",
                                                         "Content",
                                                         "Type",
                                                         "Values",
                                                         "Information"))

      df_variables$Spezifisch <- ggsub(df_variables)

      df_variables$Variablen <- spezifisch_create(df_variables)

      df_variables$Object_Type <- "Variable"

      # Search for title-term match ####
      if (nrow(df_variables) != 0) {
        df_variables$Titel <- titel_search(df_variables, term)
      }

      # Ordering ####
      if(ordering){
        df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c( "Code",
                                                                                       "Content",
                                                                                       "Titel",
                                                                                       "Values",
                                                                                       "Information",
                                                                                       "Variablen",
                                                                                       "Spezifisch",
                                                                                       "Object_Type")]

      } else {

        df_variables <- df_variables[, c("Code",
                                         "Content",
                                         "Titel",
                                         "Values",
                                         "Information",
                                         "Variablen",
                                         "Spezifisch",
                                         "Object_Type")]

      }

      # Combine ####
      list_resp <- list("Variables" = tibble::as_tibble(df_variables))

      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright

      return(list_resp)




    }
    if (category == "cubes") {
      # Cubes ####
      df_cubes <- binding_lapply(results_json$Cubes,
                                 characteristics = c("Code",
                                                     "Content",
                                                     "Time",
                                                     "LatestUpdate",
                                                     "State",
                                                     "Information"))

      df_cubes$Spezifisch <- ggsub(df_cubes)

      df_cubes$Variablen <- spezifisch_create(df_cubes)

      df_cubes$Object_Type <- "Cube"

      # Search for title-term match ####
      if (nrow(df_cubes) != 0) {
        df_cubes$Titel <- titel_search(df_cubes, term)
      }

      # Ordering ####
      if(ordering){
        df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c( "Code",
                                                                           "Content",
                                                                           "Titel",
                                                                           "Time",
                                                                           "LatestUpdate",
                                                                           "State",
                                                                           "Information",
                                                                           "Variablen",
                                                                           "Spezifisch",
                                                                           "Object_Type")]
      } else {

        df_cubes <- df_cubes[, c("Code",
                                 "Content",
                                 "Titel",
                                 "Time",
                                 "LatestUpdate",
                                 "State",
                                 "Information",
                                 "Variablen",
                                 "Spezifisch",
                                 "Object_Type")]

      }

      # Combine ####
      list_resp <- list("Cubes" = tibble::as_tibble(df_cubes))

      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright

      return(list_resp)
    }
  }

  if (detailed == F) {
    if (category == "all") {

      # Table ####
      df_table <- binding_lapply(results_json$Tables,
                                 characteristics = c("Code",
                                                     "Content"))

      df_table$Spezifisch <- ggsub(df_table)

      df_table$Variablen <- spezifisch_create(df_table)

      df_table$Object_Type <- "Table"

      # Statistic ####
      df_stats <- binding_lapply(results_json$Statistics,
                                 characteristics = c("Code",
                                                     "Content"))

      df_stats$Spezifisch <- ggsub(df_stats)

      df_stats$Variablen <- spezifisch_create(df_stats)

      df_stats$Object_Type <- "Statistic"

      # Variables ####
      df_variables <- binding_lapply(results_json$Variables,
                                     characteristics = c("Code",
                                                         "Content"))

      df_variables$Spezifisch <- ggsub(df_variables)

      df_variables$Variablen <- spezifisch_create(df_variables)

      df_variables$Object_Type <- "Variable"

      # Cubes ####
      df_cubes <- binding_lapply(results_json$Cubes,
                                 characteristics = c("Code",
                                                     "Content"))

      df_cubes$Spezifisch <- ggsub(df_cubes)

      df_cubes$Variablen <- spezifisch_create(df_cubes)

      df_cubes$Object_Type <- "Cube"

      # Search for title-term match ####
      if (nrow(df_table) != 0) {
        df_table$Titel <- titel_search(df_table, term)
      }
      if (nrow(df_stats) != 0) {
        df_stats$Titel <- titel_search(df_stats, term)
      }
      if (nrow(df_variables) != 0) {
        df_variables$Titel <- titel_search(df_variables, term)
      }
      if (nrow(df_cubes) != 0) {
        df_cubes$Titel <- titel_search(df_cubes, term)
      }

      # Ordering ####
      if(ordering){
        df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c("Code",
                                                                          "Content",
                                                                          "Object_Type")]

        df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c( "Code",
                                                                           "Content",
                                                                           "Object_Type")]

        df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c( "Code",
                                                                                       "Content",
                                                                                       "Object_Type")]

        df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c( "Code",
                                                                           "Content",
                                                                           "Object_Type")]
      } else {

        df_table <- df_table[, c("Code",
                                 "Content",
                                 "Object_Type")]

        df_stats <- df_stats[, c("Code",
                                 "Content",
                                 "Object_Type")]

        df_variables <- df_variables[, c("Code",
                                         "Content",
                                         "Object_Type")]

        df_cubes <- df_cubes[, c("Code",
                                 "Content",
                                 "Object_Type")]

      }

      # Combine ####
      list_resp <- list(
        "Tables" = tibble::as_tibble(df_table), "Statistics" = tibble::as_tibble(df_stats),
        "Variables" = tibble::as_tibble(df_variables), "Cubes" = tibble::as_tibble(df_cubes)
      )
      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright

      return(list_resp)

    }
    if (category == "tables") {

      # Table ####
      df_table <- binding_lapply(results_json$Tables,
                                 characteristics = c("Code",
                                                     "Content"))

      df_table$Spezifisch <- ggsub(df_table)

      df_table$Variablen <- spezifisch_create(df_table)

      df_table$Object_Type <- "Table"

      # Search for title-term match ####
      if (nrow(df_table) != 0) {
        df_table$Titel <- titel_search(df_table, term)
      }

      # Ordering ####
      if(ordering){
        df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c("Code",
                                                                          "Content",
                                                                          "Object_Type")]
      } else {

        df_table <- df_table[, c("Code",
                                 "Content",
                                 "Object_Type")]
      }

      # Combine ####
      list_resp <- list("Tables" = tibble::as_tibble(df_table))

      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright

      return(list_resp)







    }
    if (category == "statistics") {
      # Statistic ####
      df_stats <- binding_lapply(results_json$Statistics,
                                 characteristics = c("Code",
                                                     "Content"))

      df_stats$Spezifisch <- ggsub(df_stats)

      df_stats$Variablen <- spezifisch_create(df_stats)

      df_stats$Object_Type <- "Statistic"

      # Search for title-term match ####
      if (nrow(df_stats) != 0) {
        df_stats$Titel <- titel_search(df_stats, term)
      }

      # Ordering ####
      if(ordering){
        df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c( "Code",
                                                                           "Content",
                                                                           "Object_Type")]

      } else {
        df_stats <- df_stats[, c("Code",
                                 "Content",
                                 "Object_Type")]

      }

      # Combine ####
      list_resp <- list("Statistics" = tibble::as_tibble(df_stats))

      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright

      return(list_resp)




    }
    if (category == "variables") {

      # Variables ####
      df_variables <- binding_lapply(results_json$Variables,
                                     characteristics = c("Code",
                                                         "Content"))

      df_variables$Spezifisch <- ggsub(df_variables)

      df_variables$Variablen <- spezifisch_create(df_variables)

      df_variables$Object_Type <- "Variable"

      # Search for title-term match ####
      if (nrow(df_variables) != 0) {
        df_variables$Titel <- titel_search(df_variables, term)
      }

      # Ordering ####
      if(ordering){
        df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c( "Code",
                                                                                       "Content",
                                                                                       "Object_Type")]

      } else {

        df_variables <- df_variables[, c("Code",
                                         "Content",
                                         "Object_Type")]

      }

      # Combine ####
      list_resp <- list("Variables" = tibble::as_tibble(df_variables))

      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright

      return(list_resp)




    }
    if (category == "cubes") {
      # Cubes ####
      df_cubes <- binding_lapply(results_json$Cubes,
                                 characteristics = c("Code",
                                                     "Content"))

      df_cubes$Spezifisch <- ggsub(df_cubes)

      df_cubes$Variablen <- spezifisch_create(df_cubes)

      df_cubes$Object_Type <- "Cube"

      # Search for title-term match ####
      if (nrow(df_cubes) != 0) {
        df_cubes$Titel <- titel_search(df_cubes, term)
      }

      # Ordering ####
      if(ordering){
        df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c( "Code",
                                                                           "Content",
                                                                           "Object_Type")]
      } else {

        df_cubes <- df_cubes[, c("Code",
                                 "Content",
                                 "Object_Type")]

      }

      # Combine ####
      list_resp <- list("Cubes" = tibble::as_tibble(df_cubes))

      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyrigtht") <- results_json$Copyright

      return(list_resp)
    }
  }

  return(list_resp)
  }
}
