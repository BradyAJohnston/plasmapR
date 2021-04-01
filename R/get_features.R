#' Extract Lines that Contain Feature Information
#' 
#' @export
get_features <- function(plasmid) {

  is_feature <- FALSE
  feature_list <- c()

  # sapply(plasmid, function(i) {
  for (i in seq_along(plasmid)) {
    line <- plasmid[i]
    # print()
    # line <- i
    # print(i)

    if (str_detect(substr(line, 1, 10), "ORIGIN")) {
      is_feature <- FALSE
    }

    if (str_detect(substr(line, 1, 10), "BASE COUNT")) {
      is_feature <- FALSE
    }

    if (is_feature) {
      feature_list <- c(feature_list, line)
      line
    }

    if (str_detect(substr(line, 1,10), "FEATURES")) {
      is_feature <- TRUE
    }
  }

  # }, simplify = TRUE)


  feature_list
}