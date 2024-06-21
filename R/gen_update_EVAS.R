#' gen_update_EVAS: Update the EVAS numbers used in the gen_catalogue function
#'
#' @description Function to web scrape the EVAS numbers from the EVAS website and save them as a RData file.
#'
#' @return An update RData file containing the EVAS numbers.
#' @export
#'
gen_update_EVAS <- function(){

  # Path selection
  data_path <- system.file("data", "EVAS_numbers.RData", package = "restatis")

  # Load the data object
  data("EVAS_numbers", package = "restatis")

  # Define the src URL of the EVAS numbers
  url <- "https://erhebungsdatenbank.estatistik.de/eid/TabelleEvas.jsp"

  # Read the HTML content of the URL
  html <- read_html(url)
  html <- html_nodes(html, "table")
  html <- map_dfr(html, html_table, convert = FALSE)
  html <- html[, c("EVAS", "Beschreibung")]
  html$Titel <- paste(html$EVAS, html$Beschreibung, sep = " - ")
  attr(html, "Update_Date") <- format(Sys.Date(), "%Y%m%d")

  # Modify the data object
  html <- html

  # Return the modified data object
  save(html ,file = data_path)
}




