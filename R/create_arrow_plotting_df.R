#' Create df for plotting arrows.
#'
#' @export
create_arrow_plotting_df <-
  function(df,
           middle,
           width,
           plasmid_length,
           arrowhead_size = 8) {
    arrow_list <- list()

    for (i in seq(nrow(df))) {
      start <- df[i, "start"]
      end <- df[i, "end"]
      direction <- df[i, "direction"]

      temp_arrow_df <- create_arrow_positions(
        start = start,
        end = end,
        middle = middle,
        width = width,
        arrowhead_size = arrowhead_size,
        plasmid_length = plasmid_length
      )

      temp_arrow_df$type <- df[i, "type"]
      temp_arrow_df$name <- df[i, "name"]
      temp_arrow_df$index <- df[i, "index"]

      if (grepl("RIGHT", direction)) {
        temp_arrow_df$x <- -temp_arrow_df$x + start + end
      }

      arrow_list[[i]] <- temp_arrow_df
    }

    out_df <- do.call(rbind, arrow_list)
    out_df
  }
