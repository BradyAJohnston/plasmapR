#' Extract sequence from readlines(plasmid)
#'
#' @param plasmid Takes result of \code{readLines()} and parses it to extract
#'   the final plasmid sequence. Currently only used to final the plasmid
#'   length.
#'
#' @export
extract_sequence <- function(plasmid) {
  is_sequence <- FALSE
  sequence <- c()

  for (i in seq_along(plasmid)) {
    line <- plasmid[i]

    if (grepl("//", line)) {
      is_sequence <- FALSE
    }

    if (is_sequence) {
      tmp_seq <- substr(line, 10, 78)
      tmp_seq <- trimws(tmp_seq)
      tmp_seq <- gsub(" ", "", tmp_seq)
      sequence <- c(sequence, tmp_seq)
    }

    if (grepl("ORIGIN", substr(line, 1, 10))) {
      is_sequence <- TRUE
    }
  }
  sequence
}
