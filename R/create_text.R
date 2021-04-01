#' Create dataframe containing split and spaced text.
#'
#' @export
split_text_df  <-
    function(feat_length, feat_middle, name, char_spacing, reverse = FALSE) {
        if (is.na(name)) {
            name_len <- 0
        } else {
            name_len <- nchar(name)
        }

        label_characters <- stringr::str_split(name, "")[[1]]

        if (reverse) {
            label_characters <- rev(label_characters)
        }

        char_df <- data.frame(num = 1:name_len,
                              char = label_characters)

        calculate_pos <- function(index, spacing, offset) {
            index * spacing + offset
        }

        char_df$pos <- calculate_pos(char_df$num, char_spacing, feat_middle)

        char_df
    }
