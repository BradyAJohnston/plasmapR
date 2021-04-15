#' Parse Plasmid
#'
#' @export
parse_plasmid <- function(file) {
  plasmid <- readLines(file)

  sequence <- extract_sequence(plasmid)

  sequence <- paste0(sequence, collapse = "")

  feat_df <- create_feature_df(get_features(plasmid))

  list(
    features = feat_df,
    length = nchar(sequence),
    sequence = sequence
  )
}
