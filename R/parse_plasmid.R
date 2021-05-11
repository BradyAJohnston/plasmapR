#' Parse Plasmid
#'
#' Parses a \code{.gb} Genbank file. Returns a list containing a dataframe of
#' features, the sequence, and the length of the plasmid for use in the
#' \code{render_plasmid()} function.
#'
#' @param file A file containing a \code{.gb} Genbank formatted plasmid
#'   sequence. File should contain list of features in standard Genbank
#'   notation.
#'
#' @export
parse_plasmid <- function(file) {
  plasmid <- readLines(file)

  sequence <- extract_sequence(plasmid)

  sequence <- paste0(sequence, collapse = "")

  feat_df <- create_feature_df(get_features(plasmid))

  parsed_plasmid <- list(
    features = feat_df,
    length = nchar(sequence),
    sequence = sequence
  )

  parsed_plasmid
}
