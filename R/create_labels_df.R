#' Create Labels DF
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


    for (i in 1:nrow(df)) {
      label_length <- nchar(df[i, "name"])

      feat_length <- df[i, "end"] - df[i, "start"]

      feat_middle <- df[i, "start"] + feat_length / 2

      circle_pos <- feat_middle / plasmid_length
      # ratio_of_map <- feat_length / plasmid_length * label_length

      char_spacing <- plasmid_length * spacing_scale

      text_overflow <- ((label_length + 1) * char_spacing) > (label_length_cutoff * feat_length)


      if (text_overflow) {
        temp_annotation <- df[i, ]
        temp_annotation$middle <- feat_middle

        annotation_list[[i]] <- temp_annotation
      } else {
        rotation_ratio <- rotation / 360
        circle_pos <- circle_pos + rotation_ratio



        if (circle_pos > 0.25 && circle_pos < 0.75) {
          reverse <- TRUE
          angle_adjustment <- 180
        } else {
          reverse <- FALSE
          angle_adjustment <- 0
        }

        curved_text <- split_text_df(
          feat_length = feat_length,
          feat_middle = feat_middle,
          name = df[i, "name"],
          char_spacing = char_spacing,
          reverse = reverse
        )

        num_letters <- nrow(curved_text)

        curved_text$pos <- curved_text$pos - num_letters * (0.5 / label_hjust) / 2 * char_spacing

        curved_text$angle <- -curved_text$pos / plasmid_length * 360

        curved_text$angle <- curved_text$angle + angle_adjustment

        curved_text_list[[i]] <- curved_text
      }
    }

    list(
      annotations = do.call(rbind, annotation_list),
      curved = do.call(rbind, curved_text_list)
    )
  }
