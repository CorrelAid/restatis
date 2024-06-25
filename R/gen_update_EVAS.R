#' gen_update_EVAS: Update the EVAS numbers used in the gen_catalogue function
#'
#' @description Function to web scrape the EVAS numbers from the EVAS website and save them as a RData file.
#'
#' @return An update RData file containing the EVAS numbers.
#' @export
#'
gen_update_EVAS <- function(){

  # Check rvest and purrr packages
  if (!requireNamespace("rvest", quietly = TRUE)) {

    stop("If you want to use this specific function, the package {rvest} needs to be installed.",
         call. = FALSE)

  }

  if (!requireNamespace("purrr", quietly = TRUE)) {

    stop("If you want to use this specific function, the package {purrr} needs to be installed.",
         call. = FALSE)

  }

  # Path selection
  data_path <- system.file("data", "EVAS_numbers.RData", package = "restatis")

  # Define the src URL of the EVAS numbers
  url <- "https://erhebungsdatenbank.estatistik.de/eid/TabelleEvas.jsp"

  # Read the HTML content of the URL
  html <- rvest::read_html(url)
  html <- rvest::html_nodes(html, "table")
  html <- purrr::map_dfr(html, html_table, convert = FALSE)
  html <- html[, c("EVAS", "Beschreibung")]
  html$Titel <- paste(html$EVAS, html$Beschreibung, sep = " - ")
  attr(html, "Update_Date") <- format(Sys.Date(), "%Y%m%d")

  # Modify the data object
  html <- html

  # Return the modified data object
  save(html ,file = data_path)
}




