#' Plasmid Plotting Function
#'
#' @export

plasmid_plot <-
  function(feature_df,
           arrow_df,
           labels,
           angle_adjustment,
           features = NULL,
           zoom_y,
           plasmidName,
           font_family = "mono",
           nameSize,
           curve,
           rotation,
           labelNudge,
           labelSize,
           repelBox,
           plasmid_length,
           plasmid_y = 2,
           plasmid_width = 0.1,
           arrowhead_size = 4,
           spacing_scale = 0.006,
           label_hjust = 0.4,
           label_length_cutoff = 0.85) {


    # The actual ggplot call for plotting

    ggplot2::ggplot() +

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
        label = plasmidName,
        size = nameSize,
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
        size = labelSize / ggplot2::.pt,
        nudge_y = labelNudge,
        ylim = c(2.2, 5),
        box.padding = repelBox,
        max.overlaps = 20,
        segment.curvature = 1 * 10^(-curve),
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

      # Add the curved labels to the features
      ggplot2::geom_text(
        data = labels$curved,
        mapping = ggplot2::aes(
          x = pos,
          y = 2,
          angle = angle - angle_adjustment,
          label = char
        ),
        family = font_family
      )
  }
