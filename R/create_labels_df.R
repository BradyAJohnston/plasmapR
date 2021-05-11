#' Create Labels DF
#'
#' @param df Dataframe of features that includes start, end and name.
#' @param plasmid_length Total plasmid length in bp.
#' @param label_hjust Adjusts justification of the curved labels towards the
#'   start (0) or end (1) of the arrow.
#' @param label_length_cutoff Proportion of the arrow the text is allowed to
#'   take up before becoming a rep
#' @param rotation Angular rotation of the resulting plasmid map (in degrees).
#' @param spacing_scale Scalar for the spacing between the curved text on the
#'   arrows.
#'
#' @export
create_labels <-
  function(df,
           plasmid_length,
           label_hjust = 0.5,
           label_length_cutoff = 0.9,
           rotation = 0,
           spacing_scale) {
    annotation_list <- list()
    curved_text_list <- list()


    for (i in seq(nrow(df))) {
      label_length <- nchar(df$name[i])

      feat_length <- df[i, "end"] - df[i, "start"]

      feat_middle <- df[i, "start"] + feat_length / 2

      circle_pos <- feat_middle / plasmid_length

      char_spacing <- plasmid_length * spacing_scale

      text_overflow <-
        ((label_length + 1) * char_spacing) > (label_length_cutoff * feat_length)

      # calculate rotations and new positions

        rotation_ratio <- rotation / 360
        circle_pos <- circle_pos + rotation_ratio

        if (circle_pos > 0.25 && circle_pos < 0.75) {
          reverse <- TRUE
          angle_adjustment <- 180
        } else {
          reverse <- FALSE
          angle_adjustment <- 0
        }


      if (text_overflow) {
        temp_annotation <- df[i, ]
        temp_annotation$middle <- feat_middle

        # temp_annotation$rotation <- angle_adjustment

        annotation_list[[i]] <- temp_annotation
      } else {





        # if (reverse) {
        #   name = sapply(lapply(strsplit(df$name[i], NULL), rev), paste0, collapse = "")
        # } else {
        #   name = df$name[i]
        # }

        name <- df$name[i]

        curved_text <- data.frame(
          pos = feat_middle,
          name = name,
          feat_length = feat_length,
          rotation = angle_adjustment
        )

        # curved_text <- split_text_df(
        #   feat_length = feat_length,
        #   feat_middle = feat_middle,
        #   name = df$name[i],
        #   char_spacing = char_spacing,
        #   reverse = reverse
        # )
        #
        # num_letters <- nrow(curved_text)
        #
        # curved_text$pos <-
        #   curved_text$pos - num_letters * (0.5 / label_hjust) / 2 * char_spacing
        #
        # curved_text$angle <- (-curved_text$pos) / plasmid_length * 360
        #
        # curved_text$angle <- curved_text$angle + angle_adjustment
        #
        curved_text_list[[i]] <- curved_text
      }
    }

    list(
      annotations = do.call(rbind, annotation_list),
      curved = do.call(rbind, curved_text_list)
    )
  }
