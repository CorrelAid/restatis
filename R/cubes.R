#' Get a data cube
#'
#' @param name Name of the data cube
#' @param ... TODO
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gen_cube("47414BJ002")
#' }
gen_cube <- function(name, ...) {
  cube <- gen_api("data/cubefile", name = name, ...) %>%
    read_cube() %>%
    rename_cube_data_columns()

  structure(
    cube$QEI,
    metadata = cube[names(cube) != "QEI"]
  )
}

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

split_cube <- function(lines) {
  block_idx <- ifelse(is_cube_metadata_header(lines), seq_along(lines), NA)
  block_idx <- vctrs::vec_fill_missing(block_idx, "down")

  unname(split(lines, block_idx))
}

is_cube_metadata_header <- function(lines) {
  startsWith(lines, "K")
}

read_cube_block <- function(lines) {
  header <- read_cube_metadata_header(lines[1])

  structure(
    read_cube_data_lines(lines[-1], header$cols),
    block_name = header$block_name
  )
}

read_cube_metadata_header <- function(line, rename_dups = TRUE) {
  stopifnot(length(line) == 1L)

  line_splitted <- strsplit(line, ";", fixed = TRUE)[[1]]

  block_name <- line_splitted[2]

  col_names <- line_splitted[3:length(line_splitted)]
  col_names <- col_names[!col_names %in% c("\"nur Werte\"", "\"mit Werten\"")]

  if (rename_dups) col_names <- make.unique(col_names)

  list(block_name = block_name, cols = col_names)
}

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

rename_cube_data_columns <- function(cube) {
  data_cols <- names(cube$QEI)

  data_cols[startsWith(data_cols, "FACH-SCHL")] <- cube$DQA[order(cube$DQA$`RHF-ACHSE`), ]$NAME

  data_cols[data_cols == "ZI-WERT"] <- cube$DQZ$NAME

  data_cols[data_cols == "WERT"] <- cube$DQI$NAME

  names(cube$QEI) <- data_cols

  cube
}
