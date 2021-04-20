#' Plasmid Plotting Function
#'
#' Plotting function to create the final plasmid map from properly file.
#'
#' @param arrow_df Dataframe of points from the
#'   \code{create_arrow_plotting_df()} function to draw arrows.
#' @param labels List resulting from \code{create_labels()} function containing
#'   curved text dataframe and labels for annotation.
#' @param angle_adjustment Angle (in degrees) to rotate the resulting plasmid
#'   map by.
#' @param zoom_y Zoom level for the resulting plasmid map. Sets
#'   \code{ylim(0,zoom_y)} and can be overriden.
#' @param plasmid_name String name for the plasmid map.
#' @param font_family Font family to use in all of the text on the plot.
#' @param name_size Size of the plasmid name annotation.
#' @param curve Curvature of the repelled text labels.
#' @param label_nudge Amount to nudge the labels away from the plasmid arrows to
#'   allow for neater packing.
#' @param label_size Size of the repelled labels.
#' @param repel_box Distance to repel between the labels.
#' @param plasmid_length Total length of the plasmid.
#' @param plasmid_y Position on the y axis of the original plasmid.
#' @param plasmid_width Width of the arrows of the plasmid.
#' @param curved_size Font size of the curved labels (in .pt).
#'
#' @export

plasmid_plot <-
  function(arrow_df,
           labels,
           angle_adjustment,
           zoom_y,
           plasmid_name,
           font_family = "mono",
           name_size,
           bp_count = TRUE,
           curved_size,
           curve,
           label_nudge,
           label_size,
           repel_box,
           plasmid_length,
           plasmid_y = 2,
           plasmid_width = 0.1) {


    # The actual ggplot call for plotting
    plt <- ggplot2::ggplot() +

      # Add line that becomes the central plasmid line
      ggplot2::geom_hline(yintercept = plasmid_y) +

      # Turn plot into circular, do rotation depending on the angle adjustment
      # that was specified to rotate the plot (in radians).

      ggplot2::coord_polar(start = angle_adjustment / 360 * 2 * pi) +

      # Specify how long the plasmid will be (using length of sequence)
      ggplot2::xlim(c(0, plasmid_length)) +

      # Zoom the circle (how much of the 'viewport' should it take up?)
      # The lower the zoom level, the more room there is for labels to be
      # arranged around the outside of the plot

      ggplot2::ylim(c(0, zoom_y)) +


      # Remove basically everything from the plot
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "") +

      # Add the plasmid title in the centre.
      ggplot2::annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = plasmid_name,
        size = name_size,
        family = font_family
      ) +


      # Add the repelled feature labels to the plasmid
      ggrepel::geom_label_repel(
        data = labels$annotations,
        mapping = ggplot2::aes(
          x = middle,
          y = plasmid_y + plasmid_width,
          label = name,
          fill = type
        ),
        family = font_family,
        colour = "black",
        segment.color = "black",
        size = label_size / ggplot2::.pt,
        nudge_y = label_nudge,
        ylim = c(2.2, 5),
        box.padding = repel_box,
        max.overlaps = 20,
        segment.curvature = 1 * 10 ^ (-curve),
        segment.inflect = FALSE,
        segment.square = TRUE,
        # direction = "y",
        hjust = 0.5
      ) +

      # Add the arrows to the plot

      ggplot2::geom_polygon(
        data = arrow_df,
        ggplot2::aes(
          x = x,
          y = y,
          fill = type,
          group = index
        ),
        colour = "black"
      ) +
      ggplot2::labs(fill = "Feature Type") +

      # Add the curved labels to the features
      # ggplot2::geom_text(
      #   data = labels$curved,
      #   mapping = ggplot2::aes(
      #     x = pos,
      #     y = 2,
      #     angle = angle - angle_adjustment,
      #     label = char
      #   ),
      #   size = curved_size / ggplot2::.pt,
      #   family = font_family
      # ) +
      NULL

    if (bp_count) {
      plt +
        ggplot2::annotate(
          geom = "text",
          x = 0,
          y = 0,
          label = paste("\n\n", plasmid_length, "bp"),
          size = name_size * 0.8,
          family = font_family
        )
    } else {
      plt
    }
  }
