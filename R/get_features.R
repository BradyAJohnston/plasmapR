#' Extract Lines that Contain Feature Information
#'
#' @param plasmid Result of \code{readLines()}. Parses the file to get features
#'   from the read .gb file.
#'
#' @export
get_features <- function(plasmid) {
  is_feature <- FALSE
  feature_list <- c()

  for (i in seq_along(plasmid)) {
    line <- plasmid[i]


    if (stringr::str_detect(substr(line, 1, 10), "ORIGIN")) {
      is_feature <- FALSE
    }

    if (stringr::str_detect(substr(line, 1, 10), "BASE COUNT")) {
      is_feature <- FALSE
    }

    if (is_feature) {
      feature_list <- c(feature_list, line)
      line
    }

    if (stringr::str_detect(substr(line, 1, 10), "FEATURES")) {
      is_feature <- TRUE
    }
  }


  feature_list
}
