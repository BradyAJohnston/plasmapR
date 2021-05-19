#' Create Labels DF
#'
#' @param df Dataframe of features that includes start, end and name.
#' @param plasmid_length Total plasmid length in bp.
#' @param label_length_cutoff Proportion of the arrow the text is allowed to
#'   take up before becoming a rep
#' @param rotation Angular rotation of the resulting plasmid map (in degrees).
#'
#' @export
create_labels <-
  function(df,
           plasmid_length,
           label_length_cutoff = 0.9,
           rotation = 0) {
    annotation_list <- list()
    curved_text_list <- list()


    for (i in seq(nrow(df))) {
      label_length <- nchar(df$name[i])

      feat_length <- df[i, "end"] - df[i, "start"]

      feat_middle <- df[i, "start"] + feat_length / 2

      circle_pos <- feat_middle / plasmid_length

      char_spacing <- plasmid_length / 400

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


        name <- df$name[i]

        curved_text <- data.frame(
          pos = feat_middle,
          name = name,
          feat_length = feat_length,
          rotation = angle_adjustment
        )

        curved_text_list[[i]] <- curved_text
      }
    }

    list(
      annotations = do.call(rbind, annotation_list),
      curved = do.call(rbind, curved_text_list)
    )
  }
