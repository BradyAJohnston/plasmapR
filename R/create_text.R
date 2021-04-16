#' Create dataframe containing split and spaced text.
#'
#' Takes a feature name and splits it into individual letters for rotation and
#' position.
#'
#' @param feat_length Length of the feature in bp.
#' @param feat_middle Mid-point of the feature in bp.
#' @param name String name of the feature.
#' @param char_spacing Spacing to place in between the individual letters.
#' @param reverse Logical, whether to reverse the arrangement of the letters for
#'   what woudld otherwise be an upside down label.
#'
#' @export
split_text_df <-
  function(feat_length,
           feat_middle,
           name,
           char_spacing,
           reverse = FALSE) {
    if (is.na(name)) {
      name_len <- 0
    } else {
      name_len <- nchar(name)
    }

    label_characters <- strsplit(name, "")[[1]]

    if (reverse) {
      label_characters <- rev(label_characters)
    }

    char_df <- data.frame(
      num = 1:name_len,
      char = label_characters
    )

    calculate_pos <- function(index, spacing, offset) {
      index * spacing + offset
    }

    char_df$pos <- calculate_pos(char_df$num, char_spacing, feat_middle)

    char_df
  }
