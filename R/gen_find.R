#' General Search for Objects Through GENESIS
#'
#' @description Function to search through GENESIS. It is similar in usage as the search function on the Destatis main page (https://www.destatis.de/DE/Home/_inhalt.html).
#' In the search query, "UND" (german word for: and; can also be written "und" or "&") as well as "ODER" (german word for: or; can also be written "oder" or "|") can be included and logically combined. Furthermore, wildcards are possible by including "*". If more then one word is included in the term-string, automatically "and" is used to combine the different words.
#' Important note: Time-series are treated as cubes, they are not longer distinguished. If you want to find a specific object with a clear code with this find function, you need to specify the object type or search for all object types.
#'
#' @param term A string with no maximum character length, but a word limit of five words.
#' @param category A string. Specific object types: 'tables', 'statistics', 'variables', and 'cubes'. Using all together is possible. Default option are 'all' objects.
#' @param detailed A logical. Indicator if the function should return the detailed output of the iteration including all object related information or only a shortened output including only code and object title. Default Option is FALSE.
#' @param ordering A logical. Indicator if the function should return the output of the iteration ordered first based on the fact if the searched term is appearing in the title of the object and secondly on an estimator of the number of variables in this object. Default option is TRUE.
#' @param error.ignore  A logical. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce an artificial response (e.g., for complex processes not to fail).
#' @param ... Additional parameter of the GENESIS API call. These parameters are only affecting the GENESIS API call itself, no further processing.
#'
#' @return A list with all elements retrieved from GENESIS. Attributes are added to the data.frame describing the search configuration for the returned output.
#' @export
#'
#' @examples
#' \dontrun{
#' # Find objects related to "bus" in GENESIS
#' object <- gen_find(term = "bus")
#'
#' # Find tables related to "bus" in GENESIS and return a unordered detailed output
#' object <- gen_find(term = "bus", detailed = TRUE, ordering = FALSE)
#'
#' # Find tables related to "Autos" or "Corona" in GENESIS and return a unordered detailed output
#' object <- gen_find(term = "autos ODER corona", detailed = TRUE, ordering = FALSE)
#'
#' #' # Find tables related to "Autos" and "Corona" in GENESIS and return a unordered detailed output
#' object <- gen_find(term = "autos UND corona", detailed = TRUE, ordering = FALSE)
#' }
#'
gen_find <- function(term = NULL,
                     category = c("all", "tables", "statistics", "variables", "cubes"),
                     detailed = FALSE,
                     ordering = TRUE,
                     error.ignore = FALSE,
                     ...) {
  caller <- as.character(match.call()[1])

  check_function_input(
    term = term,
    category = category,
    detailed = detailed,
    ordering = ordering,
    error.ignore = error.ignore,
    caller = caller
  )

  category <- match.arg(category)

  #-----------------------------------------------------------------------------

  results_raw <- gen_api("find/find",
    term = term,
    category = category,
    ...
  )

  results_json <- test_if_json(results_raw)

  empty_object <- test_if_error_find(results_json, para = error.ignore)

  empty_object <- test_if_process_further(results_json, para = error.ignore)

  #-----------------------------------------------------------------------------

  if (isTRUE(empty_object)) {
    list_resp <- list("Output" = "No object found for your request.")

    attr(list_resp, "Term") <- results_json$Parameter$term
    attr(list_resp, "Language") <- results_json$Parameter$language
    attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
    attr(list_resp, "Copyright") <- results_json$Copyright

    return(list_resp)
  } else if (isFALSE(empty_object)) {
    list_resp <- list("Output" = results_json$Status$Content)

    attr(list_resp, "Term") <- results_json$Parameter$term
    attr(list_resp, "Language") <- results_json$Parameter$language
    attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
    attr(list_resp, "Copyright") <- results_json$Copyright

    return(list_resp)
  } else if (empty_object == "DONE") {
    if (detailed == TRUE) {
      #-----------------------------------------------------------------------------

      if (category == "all") {
        df_table <- binding_lapply(results_json$Tables,
          characteristics = c(
            "Code",
            "Content",
            "Time"
          )
        )

        df_table$Spezifisch <- ggsub(df_table)

        df_table$Variablen <- spezifisch_create(df_table)

        df_table$Object_Type <- "Table"

        #-------------------------------------------------------------------------

        df_stats <- binding_lapply(results_json$Statistics,
          characteristics = c(
            "Code",
            "Content",
            "Information",
            "Cubes"
          )
        )

        df_stats$Spezifisch <- ggsub(df_stats)

        df_stats$Variablen <- spezifisch_create(df_stats)

        df_stats$Object_Type <- "Statistic"

        #-------------------------------------------------------------------------

        df_variables <- binding_lapply(results_json$Variables,
          characteristics = c(
            "Code",
            "Content",
            "Type",
            "Values",
            "Information"
          )
        )

        df_variables$Spezifisch <- ggsub(df_variables)

        df_variables$Variablen <- spezifisch_create(df_variables)

        df_variables$Object_Type <- "Variable"

        #-------------------------------------------------------------------------

        df_cubes <- binding_lapply(results_json$Cubes,
          characteristics = c(
            "Code",
            "Content",
            "Time",
            "LatestUpdate",
            "State",
            "Information"
          )
        )

        df_cubes$Spezifisch <- ggsub(df_cubes)

        df_cubes$Variablen <- spezifisch_create(df_cubes)

        df_cubes$Object_Type <- "Cube"

        #-------------------------------------------------------------------------

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

        #-------------------------------------------------------------------------

        if (isTRUE(ordering)) {
          df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Titel",
            "Time",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]

          df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Titel",
            "Information",
            "Cubes",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]

          df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Titel",
            "Values",
            "Information",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]

          df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Titel",
            "Time",
            "LatestUpdate",
            "State",
            "Information",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]
        } else {
          df_table <- df_table[, c(
            "Code",
            "Content",
            "Titel",
            "Time",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]

          df_stats <- df_stats[, c(
            "Code",
            "Content",
            "Titel",
            "Information",
            "Cubes",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]

          df_variables <- df_variables[, c(
            "Code",
            "Content",
            "Titel",
            "Values",
            "Information",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]

          df_cubes <- df_cubes[, c(
            "Code",
            "Content",
            "Titel",
            "Time",
            "LatestUpdate",
            "State",
            "Information",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]
        }

        #-------------------------------------------------------------------------

        list_resp <- list(
          "Tables" = tibble::as_tibble(df_table),
          "Statistics" = tibble::as_tibble(df_stats),
          "Variables" = tibble::as_tibble(df_variables),
          "Cubes" = tibble::as_tibble(df_cubes)
        )

        attr(list_resp, "Term") <- results_json$Parameter$term
        attr(list_resp, "Language") <- results_json$Parameter$language
        attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
        attr(list_resp, "Copyright") <- results_json$Copyright

        return(list_resp)
      }

      if (category == "tables") {
        df_table <- binding_lapply(results_json$Tables,
          characteristics = c(
            "Code",
            "Content",
            "Time"
          )
        )

        df_table$Spezifisch <- ggsub(df_table)

        df_table$Variablen <- spezifisch_create(df_table)

        df_table$Object_Type <- "Table"

        #-------------------------------------------------------------------------

        if (nrow(df_table) != 0) {
          df_table$Titel <- titel_search(df_table, term)
        }

        if (isTRUE(ordering)) {
          df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Titel",
            "Time",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]
        } else {
          df_table <- df_table[, c(
            "Code",
            "Content",
            "Titel",
            "Time",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]
        }

        #-------------------------------------------------------------------------

        list_resp <- list("Tables" = tibble::as_tibble(df_table))

        attr(list_resp, "Term") <- results_json$Parameter$term
        attr(list_resp, "Language") <- results_json$Parameter$language
        attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
        attr(list_resp, "Copyright") <- results_json$Copyright

        return(list_resp)
      }

      #---------------------------------------------------------------------------

      if (category == "statistics") {
        df_stats <- binding_lapply(results_json$Statistics,
          characteristics = c(
            "Code",
            "Content",
            "Information",
            "Cubes"
          )
        )

        df_stats$Spezifisch <- ggsub(df_stats)

        df_stats$Variablen <- spezifisch_create(df_stats)

        df_stats$Object_Type <- "Statistic"

        #-------------------------------------------------------------------------

        if (nrow(df_stats) != 0) {
          df_stats$Titel <- titel_search(df_stats, term)
        }

        if (isTRUE(ordering)) {
          df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Titel",
            "Information",
            "Cubes",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]
        } else {
          df_stats <- df_stats[, c(
            "Code",
            "Content",
            "Titel",
            "Information",
            "Cubes",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]
        }

        list_resp <- list("Statistics" = tibble::as_tibble(df_stats))

        attr(list_resp, "Term") <- results_json$Parameter$term
        attr(list_resp, "Language") <- results_json$Parameter$language
        attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
        attr(list_resp, "Copyright") <- results_json$Copyright

        return(list_resp)
      }

      #---------------------------------------------------------------------------

      if (category == "variables") {
        df_variables <- binding_lapply(results_json$Variables,
          characteristics = c(
            "Code",
            "Content",
            "Type",
            "Values",
            "Information"
          )
        )

        df_variables$Spezifisch <- ggsub(df_variables)

        df_variables$Variablen <- spezifisch_create(df_variables)

        df_variables$Object_Type <- "Variable"

        #-------------------------------------------------------------------------

        if (nrow(df_variables) != 0) {
          df_variables$Titel <- titel_search(df_variables, term)
        }

        if (isTRUE(ordering)) {
          df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Titel",
            "Values",
            "Information",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]
        } else {
          df_variables <- df_variables[, c(
            "Code",
            "Content",
            "Titel",
            "Values",
            "Information",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]
        }

        #-------------------------------------------------------------------------

        list_resp <- list("Variables" = tibble::as_tibble(df_variables))

        attr(list_resp, "Term") <- results_json$Parameter$term
        attr(list_resp, "Language") <- results_json$Parameter$language
        attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
        attr(list_resp, "Copyright") <- results_json$Copyright

        return(list_resp)
      }

      #---------------------------------------------------------------------------

      if (category == "cubes") {
        df_cubes <- binding_lapply(results_json$Cubes,
          characteristics = c(
            "Code",
            "Content",
            "Time",
            "LatestUpdate",
            "State",
            "Information"
          )
        )

        df_cubes$Spezifisch <- ggsub(df_cubes)

        df_cubes$Variablen <- spezifisch_create(df_cubes)

        df_cubes$Object_Type <- "Cube"

        #-------------------------------------------------------------------------

        if (nrow(df_cubes) != 0) {
          df_cubes$Titel <- titel_search(df_cubes, term)
        }

        if (isTRUE(ordering)) {
          df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Titel",
            "Time",
            "LatestUpdate",
            "State",
            "Information",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]
        } else {
          df_cubes <- df_cubes[, c(
            "Code",
            "Content",
            "Titel",
            "Time",
            "LatestUpdate",
            "State",
            "Information",
            "Variablen",
            "Spezifisch",
            "Object_Type"
          )]
        }

        #-------------------------------------------------------------------------

        list_resp <- list("Cubes" = tibble::as_tibble(df_cubes))

        attr(list_resp, "Term") <- results_json$Parameter$term
        attr(list_resp, "Language") <- results_json$Parameter$language
        attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
        attr(list_resp, "Copyright") <- results_json$Copyright

        return(list_resp)
      }
    }

    #-----------------------------------------------------------------------------

    if (detailed == FALSE) {
      if (category == "all") {
        #-------------------------------------------------------------------------

        df_table <- binding_lapply(results_json$Tables,
          characteristics = c(
            "Code",
            "Content"
          )
        )

        df_table$Spezifisch <- ggsub(df_table)

        df_table$Variablen <- spezifisch_create(df_table)

        df_table$Object_Type <- "Table"

        #-------------------------------------------------------------------------

        df_stats <- binding_lapply(results_json$Statistics,
          characteristics = c(
            "Code",
            "Content"
          )
        )

        df_stats$Spezifisch <- ggsub(df_stats)

        df_stats$Variablen <- spezifisch_create(df_stats)

        df_stats$Object_Type <- "Statistic"

        #-------------------------------------------------------------------------

        df_variables <- binding_lapply(results_json$Variables,
          characteristics = c(
            "Code",
            "Content"
          )
        )

        df_variables$Spezifisch <- ggsub(df_variables)

        df_variables$Variablen <- spezifisch_create(df_variables)

        df_variables$Object_Type <- "Variable"

        #-------------------------------------------------------------------------

        df_cubes <- binding_lapply(results_json$Cubes,
          characteristics = c(
            "Code",
            "Content"
          )
        )

        df_cubes$Spezifisch <- ggsub(df_cubes)

        df_cubes$Variablen <- spezifisch_create(df_cubes)

        df_cubes$Object_Type <- "Cube"

        #-------------------------------------------------------------------------

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

        #-------------------------------------------------------------------------

        if (isTRUE(ordering)) {
          df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Object_Type"
          )]

          df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Object_Type"
          )]

          df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Object_Type"
          )]

          df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Object_Type"
          )]
        } else {
          df_table <- df_table[, c(
            "Code",
            "Content",
            "Object_Type"
          )]

          df_stats <- df_stats[, c(
            "Code",
            "Content",
            "Object_Type"
          )]

          df_variables <- df_variables[, c(
            "Code",
            "Content",
            "Object_Type"
          )]

          df_cubes <- df_cubes[, c(
            "Code",
            "Content",
            "Object_Type"
          )]
        }

        #-------------------------------------------------------------------------

        list_resp <- list(
          "Tables" = tibble::as_tibble(df_table),
          "Statistics" = tibble::as_tibble(df_stats),
          "Variables" = tibble::as_tibble(df_variables),
          "Cubes" = tibble::as_tibble(df_cubes)
        )

        attr(list_resp, "Term") <- results_json$Parameter$term
        attr(list_resp, "Language") <- results_json$Parameter$language
        attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
        attr(list_resp, "Copyright") <- results_json$Copyright

        return(list_resp)
      }

      #---------------------------------------------------------------------------

      if (category == "tables") {
        df_table <- binding_lapply(results_json$Tables,
          characteristics = c(
            "Code",
            "Content"
          )
        )

        df_table$Spezifisch <- ggsub(df_table)

        df_table$Variablen <- spezifisch_create(df_table)

        df_table$Object_Type <- "Table"

        #-------------------------------------------------------------------------

        if (nrow(df_table) != 0) {
          df_table$Titel <- titel_search(df_table, term)
        }

        if (isTRUE(ordering)) {
          df_table <- df_table[with(df_table, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Object_Type"
          )]
        } else {
          df_table <- df_table[, c(
            "Code",
            "Content",
            "Object_Type"
          )]
        }

        #-------------------------------------------------------------------------

        list_resp <- list("Tables" = tibble::as_tibble(df_table))

        attr(list_resp, "Term") <- results_json$Parameter$term
        attr(list_resp, "Language") <- results_json$Parameter$language
        attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
        attr(list_resp, "Copyright") <- results_json$Copyright

        return(list_resp)
      }

      if (category == "statistics") {
        df_stats <- binding_lapply(results_json$Statistics,
          characteristics = c(
            "Code",
            "Content"
          )
        )

        df_stats$Spezifisch <- ggsub(df_stats)

        df_stats$Variablen <- spezifisch_create(df_stats)

        df_stats$Object_Type <- "Statistic"

        #-------------------------------------------------------------------------

        if (nrow(df_stats) != 0) {
          df_stats$Titel <- titel_search(df_stats, term)
        }

        if (isTRUE(ordering)) {
          df_stats <- df_stats[with(df_stats, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Object_Type"
          )]
        } else {
          df_stats <- df_stats[, c(
            "Code",
            "Content",
            "Object_Type"
          )]
        }

        #-------------------------------------------------------------------------

        list_resp <- list("Statistics" = tibble::as_tibble(df_stats))

        attr(list_resp, "Term") <- results_json$Parameter$term
        attr(list_resp, "Language") <- results_json$Parameter$language
        attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
        attr(list_resp, "Copyright") <- results_json$Copyright

        return(list_resp)
      }

      #---------------------------------------------------------------------------

      if (category == "variables") {
        df_variables <- binding_lapply(results_json$Variables,
          characteristics = c(
            "Code",
            "Content"
          )
        )

        df_variables$Spezifisch <- ggsub(df_variables)

        df_variables$Variablen <- spezifisch_create(df_variables)

        df_variables$Object_Type <- "Variable"

        #-------------------------------------------------------------------------

        if (nrow(df_variables) != 0) {
          df_variables$Titel <- titel_search(df_variables, term)
        }

        #-------------------------------------------------------------------------

        if (isTRUE(ordering)) {
          df_variables <- df_variables[with(df_variables, order(-Titel, -Variablen)), c(
            "Code",
            "Content",
            "Object_Type"
          )]
        } else {
          df_variables <- df_variables[, c(
            "Code",
            "Content",
            "Object_Type"
          )]
        }

        #-------------------------------------------------------------------------

        list_resp <- list("Variables" = tibble::as_tibble(df_variables))

        attr(list_resp, "Term") <- results_json$Parameter$term
        attr(list_resp, "Language") <- results_json$Parameter$language
        attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
        attr(list_resp, "Copyright") <- results_json$Copyright

        return(list_resp)
      }
    }

    #---------------------------------------------------------------------------

    if (category == "cubes") {
      df_cubes <- binding_lapply(results_json$Cubes,
        characteristics = c(
          "Code",
          "Content"
        )
      )

      df_cubes$Spezifisch <- ggsub(df_cubes)

      df_cubes$Variablen <- spezifisch_create(df_cubes)

      df_cubes$Object_Type <- "Cube"

      #-------------------------------------------------------------------------

      if (nrow(df_cubes) != 0) {
        df_cubes$Titel <- titel_search(df_cubes, term)
      }

      if (isTRUE(ordering)) {
        df_cubes <- df_cubes[with(df_cubes, order(-Titel, -Variablen)), c(
          "Code",
          "Content",
          "Object_Type"
        )]
      } else {
        df_cubes <- df_cubes[, c(
          "Code",
          "Content",
          "Object_Type"
        )]
      }

      #-------------------------------------------------------------------------

      list_resp <- list("Cubes" = tibble::as_tibble(df_cubes))

      attr(list_resp, "Term") <- results_json$Parameter$term
      attr(list_resp, "Language") <- results_json$Parameter$language
      attr(list_resp, "Pagelength") <- results_json$Parameter$pagelength
      attr(list_resp, "Copyright") <- results_json$Copyright

      return(list_resp)
    }
  }

  return(list_resp)
}
