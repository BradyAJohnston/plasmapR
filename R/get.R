.get_label <- function(x) {
  stringr::str_extract(x, "(?<=\\/)\\w+")
}

.get_value <- function(x) {
  if (.is_label_start(x)) {
    stringr::str_extract(x, "(?<=(\"|\\=)).+") |>
      stringr::str_remove_all("^\"|\"$")
  } else {
    stringr::str_sub(x, 22) |>
      stringr::str_remove("^\"|\"$")
  }
}

.get_start_end <- function(x) {
  vector <- stringr::str_extract(x, "\\d+\\.\\.\\d+") |>
    stringr::str_split("\\.\\.") |>
    unlist() |>
    as.numeric()
  if (length(vector) == 1) {
    c(vector, vector)
  } else {
    vector
  }
}

.get_feature_type <- function(x) {
  stringr::str_trim(x) |>
    stringr::str_extract("^[^ ]+")
}

.get_direction <- function(x) {
  ifelse(stringr::str_detect(x, "complement"), -1, 1)
}

.get_line_types <- function(x) {
  line_type <- rep("", length(x))

  line_type <- stringr::str_extract(x, "^[A-Z]+")
  for (i in seq_along(line_type)) {
    line <- line_type[i]
    if (!is.na(line)) {
      current_type <- line
    } else {
      line_type[i] <- current_type
    }
  }
  line_type
}

.get_lines_features <- function(x) {
  line_types <- .get_line_types(x)
  x[line_types == "FEATURES"]
}
.get_lines_origin <- function(x) {
  line_types <- .get_line_types(x)
  x[line_types == "ORIGIN"]
}

.is_feature_start <- function(x) {
  stringr::str_detect(stringr::str_sub(x, 6, 7), " ", negate = TRUE)
}

.is_label_start <- function(x) {
  stringr::str_detect(x, "\\/")
}

.get_feature_index <- function(x) {
  x <- stringr::str_subset(x, "^[^ ]", negate = TRUE)
  feature_start <- .is_feature_start(x)
  cumsum(feature_start)
}

.add_feature_name <- function(x) {
  if (length(x) == 4) {
    x$name <- x$type
  } else if ("label" %in% names(x)){
    x$name <- x$label
  } else {
    x$name <- x[[5]]
  }
  x
}

.get_features_list <- function(x) {
  x <- .get_lines_features(x)
  x <- stringr::str_subset(x, "^[^ ]", negate = TRUE)
  index <- .get_feature_index(x)
  features <- list()

  for (i in seq(max(index))) {
    lines <- x[index == i]
    feature <- list()

    for (line in lines) {
      if (.is_feature_start(line)) {
        feature[['type']] <- .get_feature_type(line)
        feature[['index']] <- i
        feature[['start_end']] <- .get_start_end(line)
        feature[['direction']] <- .get_direction(line)
      } else if (.is_label_start(line)) {
        current_label <- .get_label(line)
        value <- .get_value(line)

        if (current_label == "direction") {
          value <- c(
            "RIGHT" = 1,
            "LEFT" = -1
          )[value]
        }

        feature[[current_label]] <- value
      } else {

        if (!exists("current_label")) current_label <- .get_label(line)

        feature[[current_label]] <- paste0(feature[current_label], .get_value(line))
      }
    }
    feature <- .add_feature_name(feature)
    features[[i]] <- feature
  }
  features
}

.get_sequence <- function(x) {
  x <- .get_lines_origin(x)
  x <- x[stringr::str_detect(x, "^ORIGIN", negate = TRUE)]
  seq <- stringr::str_sub(x, 11)
  seq
}

.collapse_sequence <- function(x) {
  stringr::str_trim(x) |>
    stringr::str_remove_all(" ") |>
    stringr::str_c(collapse = "")
}

#' Read a `.gb` GenBank File
#'
#' @param file File path or connection, that can be handled by readr::read_lines().
#'
#' @return a list of class 'plasmid' which can be further coerced for plotting.
#' @export
#'
#' @examples
#'
#' fl <- system.file("extdata", "petm20.gb", package = "plasmapR")
#'
#' fl |>
#'   read_gb() |>
#'   as.data.frame()

read_gb <- function(file) {
  lines <- readr::read_lines(file)
  features <- .get_features_list(lines)
  sequence <- .get_sequence(lines) |>
    .collapse_sequence()
  plasmid <- list(
    length = nchar(sequence),
    features = features,
    sequence = sequence
  )
  class(plasmid) <- c("plasmid", class(plasmid))
  plasmid
}
