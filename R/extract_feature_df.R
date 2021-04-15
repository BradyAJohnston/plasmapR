#' Extract features into a dataframe.
#' 
#' @export
create_feature_df <- function(input) {
  counter <- 0
  features <- data.frame(
    type = "",
    start = 0,
    end = 0
  )

  for (i in seq_along(input)) {
    line <- input[i]

    if (stringr::str_detect(substr(line, 1, 10), "\\w")) {
      new_feature <- TRUE
    } else {
      new_feature <- FALSE
    }

    if (new_feature) {
      counter <- counter + 1

      feature_type <- stringr::str_trim(substr(line, 1, 15), side = "both")


      feat_pos <- stringr::str_trim(substr(line, 19, 50), side = "both")
      feat_pos <- stringr::str_split(line, pattern = "\\.\\.")[[1]]

      feat_pos[1] <- paste0(stringr::str_extract_all(feat_pos, "\\d")[[1]], collapse = "")
      feat_pos[2] <- paste0(stringr::str_extract_all(feat_pos, "\\d")[[2]], collapse = "")

      features[counter,] <- features[counter,1]
      features$type[counter] <- feature_type
      features$start[counter] <- as.numeric(feat_pos[1])
      features$end[counter] <- as.numeric(feat_pos[2])
      features$index[counter] <- counter

    }

    if (!new_feature) {
      if (stringr::str_detect(line, "/label"))
        features$name[counter] <- stringr::str_split(line, "=")[[1]][2]

    }

  }

  features$start <- as.numeric(features$start)
  features$end <- as.numeric(features$end)

  features <- features[!is.na(features$name), ]

  features

}