#' Extract features into a dataframe.
#'
#' Parses lines of text from a .gb format plasmid sequence file. Detects
#' features and details for the features from the file and returns it in a data
#' frame to for use in creating arrows.
#'
#' @param input \code{readlines()} of a \code{.gb} file.
#'
#' @export
create_feature_df <- function(input) {
  counter <- 0
  features <- data.frame(
    type = "",
    start = 0,
    end = 0,
    direction = ""
  )

  for (i in seq_along(input)) {
    line <- input[i]

    if (grepl("\\w", substr(line, 1, 10))) {
      new_feature <- TRUE
      direction <- "LEFT"
    } else {
      new_feature <- FALSE
    }

    if (new_feature) {
      counter <- counter + 1

      feature_type <- trimws(substr(line, 1, 15))


      feat_pos <- trimws(substr(line, 19, 50))
      feat_pos <- strsplit(line, "\\.\\.")[[1]]

      feat_pos[1] <-
        paste0(stringr::str_extract_all(feat_pos, "\\d")[[1]], collapse = "")
      feat_pos[2] <-
        paste0(stringr::str_extract_all(feat_pos, "\\d")[[2]], collapse = "")


      if (stringr::str_detect(line, "complement")) {
        direction <- "RIGHT"
      }

      features[counter, ] <- features[counter, 1]
      features$type[counter] <- feature_type
      features$start[counter] <- as.numeric(feat_pos[1])
      features$end[counter] <- as.numeric(feat_pos[2])
      features$index[counter] <- counter
    }

    if (!new_feature) {
      if (stringr::str_detect(line, "/label")) {
        features$name[counter] <-
          stringr::str_split(line, "=")[[1]][2]
      }

      if (stringr::str_detect(line, "direction=")) {
        direction <- stringr::str_split(line, "direction=")[[1]][2]
        direction <- stringr::str_trim(direction)
      }
    }

    features$direction[counter] <- direction
  }

  features$start <- as.numeric(features$start)
  features$end <- as.numeric(features$end)

  features <- features[!is.na(features$name), ]
  features$name <-
    unlist(lapply(
      stringr::str_split(string = features$name, pattern = "\\\\"),
      `[[`,
      1
    ))

  features
}
