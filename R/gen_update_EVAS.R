#' gen_update_evas
#'
#' @description Function to web scrape the EVAS numbers from the EVAS website and save them as a RData file. Takes no parameters.
#'
#' @return An updated .RData file containing the latest EVAS numbers
#' @export
#'
gen_update_evas <- function(){

  # Check rvest package
  if (!requireNamespace("rvest", quietly = TRUE)) {

    stop("If you want to use this specific function, the package {rvest} needs to be installed.",
         call. = FALSE)

  }

  # Path selection
  data_path <- system.file("data", "EVAS_numbers.RData", package = "restatis")

  # Define the src URL of the EVAS numbers
  url <- "https://erhebungsdatenbank.estatistik.de/eid/TabelleEvas.jsp"

  # Read the HTML content of the URL
  html <- rvest::read_html(url)
  html <- rvest::html_nodes(html, "table")
  html <- purrr::map_dfr(html, function(table){ rvest::html_table(table, convert = FALSE) })
  html <- html[, c("EVAS", "Beschreibung")]
  html$Titel <- paste(html$EVAS, html$Beschreibung, sep = " - ")
  attr(html, "Update_Date") <- format(Sys.Date(), "%Y%m%d")

  # Modify the data object
  evas_list <- html

  # Return the modified data object
  save(evas_list ,file = data_path)

}
