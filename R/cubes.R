# gen_cube <- function()

read_cube <- function(resp) {
  cube_str <- resp %>%
    httr2::resp_body_string() %>%
    readr::read_lines()

  blocks <- split_cube(cube_str)

  parsed <- lapply(blocks, read_cube_block)

  stats::setNames(parsed, sapply(parsed, attr, "block_name"))
}

split_cube <- function(lines) {
  idx <- ifelse(is_cube_metadata_header(lines), seq_along(lines), NA)
  idx <- vctrs::vec_fill_missing(idx, "down")

  unname(split(lines, idx))
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

  if (rename_dups) col_names <- make.unique(col_names, sep = "_")

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
