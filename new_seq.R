library(tidyr)
library(dplyr)
fl <- "inst/extdata/sequence.gb"

lines <- readr::read_lines(fl)

find_sequence <- function(lines) {
  max(stringr::str_which(lines, "^ORIGIN"))
}

find_features <- function(lines) {
  stringr::str_which(lines, '^FEATURES')
}

start_features <- find_features(lines)
start_seq <- find_sequence(lines)

parse_features <- function(lines, start_features, start_seq) {

  dat <- readr::read_fwf(
    file = paste(lines[start_features:(start_seq - 1)], collapse = "\n")
  ) |>
    janitor::row_to_names(1) |>
    janitor::clean_names()

  dat |>
    mutate(
      id = cumsum(!is.na(features))
    ) |>
    fill(features) |>
    group_by(features, id) |>
    summarise(
      details = paste0(location_qualifiers, collapse = ";")
    ) |>
    arrange(id) |>
    mutate(
      details = stringr::str_split(details, ";")
    ) |>
    mutate(
      range = purrr::map_chr(details, ~.x[1]),
      details = purrr::map_chr(details, ~paste0(.x[-1], collapse = " "))
    ) |>
    separate_rows(details, sep = "/") |>
    filter(details != "") |>
    separate(details, sep = "=", into = c("name", "value")) |>
    mutate(
      value = stringr::str_trim(stringr::str_remove_all(value, '\\\"')),
      complement = stringr::str_detect(range, stringr::fixed("complement", TRUE)),
      range = dplyr::if_else(
        complement,
        stringr::str_extract(range, "(?<=\\().+(?=\\))"),
        range
      ),
      start = as.numeric(stringr::str_extract(range, "^\\d+")),
      end = as.numeric(stringr::str_extract(range, "\\d+$"))
    ) |>
    filter(features != "source") |>
    group_by(features, id, complement, start, end) |>
    nest()

}

features <- parse_features(lines, start_features, start_seq)

parse_sequence <- function(lines, start_seq) {
  readr::read_fwf(
    file = paste(lines[(start_seq + 1):length(lines)], collapse = "\n"),
    col_types = readr::cols()
    ) |>
    select(-c(1, 2)) |>
    unlist() |>
    paste(collapse = "") |>
    stringr::str_remove_all("NA")
}

parse_sequence(lines, start_seq)

readr::read_fwf(paste(lines[1:(start_features-1)], collapse = "\n"))


