#' Get the Data of a Cube From GENESIS
#'
#' @description Download a cube with data from GENESIS.
#'
#' @param name Name of the data cube
#' @param ... Optional parameters passed on to the GENESIS API call:
#'   \describe{
#'     \item{\code{area}}{a string. The area in which the table is stored. Possible values:
#'     \itemize{
#'       \item \code{"public"}: cube in the public catalogue
#'       \item \code{"user"}: cube in the user's account
#'     }}
#'     \item{\code{values}}{a logical. Should values be included?}
#'     \item{\code{metadata}}{a logical. Should metadata be included?}
#'     \item{\code{additionals}}{a logical. Should additional metadata be included?}
#'     \item{\code{contents}}{a string. Names of required statistical specifications}
#'     \item{\code{startyear,endyear}}{a number. Only retrieve data between these years.}
#'     \item{\code{timeslices}}{a number. Number of timeslices (cumulative to startyear or endyear)}
#'     \item{\code{regionalvariable}}{character. Code of the regional variable
#'     whose value is specified in \code{regionalkey} to filter the results.}
#'     \item{\code{regionalkey}}{character. One or more regional keys. Multiple
#'       values can be supplied as a character vector or as a single string,
#'       with the regional keys separated by commas. Use of wildcard (`*`) allowed.}
#'     \item{\code{classifyingvariable1,classifyingvariable2
#'       ,classifyingvariable3}}{character. Code of the subject classification
#'       (SK-Merkmal) to which the selection by means of the corresponding
#'       `classifyingkey` parameter is to be applied.}
#'     \item{\code{classifyingkey1,classifyingkey2,classifyingkey3}}{character.
#'       One or more values of a subject classification (e.g. "WZ93012"). Applied
#'       to the corresponding `classifyingvariable` parameter. Multiple
#'       keys can be supplied as a character vector or as a single string,
#'       with the keys separated by commas. Use of wildcard (`*`) allowed.}
#'     \item{\code{stand}}{a string \code{"DD.MM.YYYY"}. Only retrieve data
#'       updated after this #' date.}
#'     \item{\code{language}}{Search terms, returned messages and data
#'       descriptions in German (`"de"`) or English (`"en"`)?}
#'   }
#'
#' @return A [tibble][tibble::tibble()]. Non-data contents of the data cube object are saved in
#'   the `metadata` [attribute][base::attr()] of the data frame.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gen_cube("47414BJ002")
#' }
#'
gen_cube <- function(name, ...) {
  gen_cube_(name, ...)
}

#-------------------------------------------------------------------------------

gen_cube_ <- function(name,
                      area = c("public", "user"),
                      values = TRUE,
                      metadata = TRUE,
                      additionals = FALSE,
                      startyear = 1900,
                      endyear = 2100,
                      timeslices = NULL,
                      contents = NULL,
                      regionalvariable = NULL,
                      regionalkey = NULL,
                      classifyingvariable1 = NULL,
                      classifyingkey1 = NULL,
                      classifyingvariable2 = NULL,
                      classifyingkey2 = NULL,
                      classifyingvariable3 = NULL,
                      classifyingkey3 = NULL,
                      stand = NULL,
                      language = Sys.getenv("GENESIS_LANG")) {
  area <- match.arg(area)

  if (!isTRUE(language == "en")) {
    area <- switch(area,
      public = "\u00F6ffentlich",
      user = "benutzer"
    )
  }

  param_check_year(startyear)
  param_check_year(endyear)

  contents <- param_collapse_vec(contents)
  regionalkey <- param_collapse_vec(regionalkey)
  classifyingkey1 <- param_collapse_vec(classifyingkey1)
  classifyingkey2 <- param_collapse_vec(classifyingkey2)
  classifyingkey3 <- param_collapse_vec(classifyingkey3)

  cube <- gen_api("data/cubefile",
    name = name,
    name = name,
    area = area,
    values = values,
    metadata = metadata,
    additionals = additionals,
    startyear = startyear,
    endyear = endyear,
    timeslices = timeslices,
    contents = contents,
    regionalvariable = regionalvariable,
    regionalkey = regionalkey,
    classifyingvariable1 = classifyingvariable1,
    classifyingkey1 = classifyingkey1,
    classifyingvariable2 = classifyingvariable2,
    classifyingkey2 = classifyingkey2,
    classifyingvariable3 = classifyingvariable3,
    classifyingkey3 = classifyingkey3,
    stand = stand,
    language = language,
    job = FALSE
  ) %>%
    read_cube() %>%
    rename_cube_data_columns()

  structure(
    cube$QEI,
    metadata = cube[names(cube) != "QEI"]
  )
}

#-------------------------------------------------------------------------------

read_cube <- function(resp) {
  cube_str <- resp %>%
    httr2::resp_body_string() %>%
    readr::read_lines()

  blocks <- split_cube(cube_str)

  parsed <- lapply(blocks, read_cube_block)

  block_names <- sapply(parsed, attr, "block_name")

  parsed <- lapply(parsed, `attr<-`, "block_name", NULL)

  stats::setNames(parsed, block_names)
}

#-------------------------------------------------------------------------------

split_cube <- function(lines) {
  block_idx <- ifelse(is_cube_metadata_header(lines), seq_along(lines), NA)
  block_idx <- vctrs::vec_fill_missing(block_idx, "down")

  unname(split(lines, block_idx))
}

#-------------------------------------------------------------------------------

is_cube_metadata_header <- function(lines) {
  startsWith(lines, "K")
}

#-------------------------------------------------------------------------------

read_cube_block <- function(lines) {
  header <- read_cube_metadata_header(lines[1])

  structure(
    read_cube_data_lines(lines[-1], header$cols),
    block_name = header$block_name
  )
}

#-------------------------------------------------------------------------------

read_cube_metadata_header <- function(line, rename_dups = TRUE) {
  stopifnot(length(line) == 1L)

  line_splitted <- strsplit(line, ";", fixed = TRUE)[[1]]

  block_name <- line_splitted[2]

  col_names <- line_splitted[3:length(line_splitted)]
  col_names <- col_names[!col_names %in% c("\"nur Werte\"", "\"mit Werten\"")]

  if (rename_dups) col_names <- make.unique(col_names)

  list(block_name = block_name, cols = col_names)
}

#-------------------------------------------------------------------------------

read_cube_data_lines <- function(lines, col_names) {
  lines <- sub("D;", "", lines, fixed = TRUE)

  class(lines) <- "AsIs" # so that vroom treats lines as literal data

  readr::read_delim(
    lines,
    delim = ";",
    col_names = col_names,
    name_repair = "minimal",
    show_col_types = FALSE
  )
}

#-------------------------------------------------------------------------------

rename_cube_data_columns <- function(cube) {
  data_cols <- names(cube$QEI)

  # Datenquader-Achsen
  data_cols[startsWith(data_cols, "FACH-SCHL")] <- cube$DQA[order(cube$DQA$`RHF-ACHSE`), ]$NAME

  # Zeitachse
  data_cols[data_cols == "ZI-WERT"] <- cube$DQZ$NAME

  # Datenquaderinhalte
  dqi_cols <- data_cols[which(data_cols == "WERT"):length(data_cols)]

  dqi_default_names <- dqi_cols[!startsWith(dqi_cols, "X")]

  # Recycle default column names for each feature
  dqi_cols_new <- unlist(.mapply(
    paste,
    expand.grid(
      suffix = dqi_default_names,
      feature_name = cube$DQI$NAME
    )[, c("feature_name", "suffix")],
    MoreArgs = list(sep = "_")
  ))

  data_cols[data_cols %in% dqi_cols] <- dqi_cols_new

  names(cube$QEI) <- data_cols

  cube
}
