#' Extract sequence from readlines(plasmid)
#' 
#' @export
extract_sequence <- function(plasmid) {

is_sequence <- FALSE
sequence <- c()

    for (i in seq_along(plasmid)) {
    line <- plasmid[i]

    if (str_detect(line, "//")) {
        is_sequence <- FALSE
    }

    if (is_sequence) {
        tmp_seq <- substr(line, 10, 78) %>%
        str_trim() %>%
        str_replace_all(pattern = " ", replacement = "")
        sequence <- c(sequence, tmp_seq)
    }

    if (str_detect(substr(line, 1, 10), "ORIGIN")) {
        is_sequence <- TRUE
    }
    }
    sequence
}