.plot_plasmid <- function(df) {
  bp <- df$end[df$type == "source"]
  df <- df[df$type != "source", ]

  df |>
    ggplot2::ggplot(ggplot2::aes(
      start = start,
      end = end,
      direction = direction,
      fill = type,
      group = index
    )) +
    ggplot2::geom_hline(yintercept = 4) +
    ggplot2::coord_polar(start = pi / 4) +


    ggrepel::geom_label_repel(
      ggplot2::aes(label = stringr::str_wrap(name, 10)),
      stat = "arrowLabel",
      box.padding = 0.6,
      size = 3,
      nudge_y = 1,
      segment.curvature = 0.01,
      label.r = 0,
      bp = 400
    ) +
    stat_arrow(
      colour = "black",
      bp = bp,
      arrowhead_size = 1
      ) +
    ggfittext::geom_fit_text(
      ggplot2::aes(
        label = name,
        ymin = 3.8,
        ymax = 4.2
      ),
      stat = "arrowLabel",
      grow = FALSE,
      size = 10,
      position = ggplot2::position_dodge2(),
      min.size = 1,
      invert = FALSE,
      flip = TRUE

    ) +
    ggplot2::ylim(c(0, NA)) +
    ggplot2::xlim(c(0, bp)) +
    ggplot2::theme_void() +
    ggplot2::annotate(
      geom = "text",
      x = 0,
      y = 0,
      label = stringr::str_glue("Some Plasmid\n{bp} bp")
    ) +
    ggplot2::scale_fill_brewer(type = 'qual', palette = 5) +
    ggplot2::theme(
      legend.position = ""
    )
  }

plot_plasmid <- function(plasmid) {
  features <- as.data.frame(plasmid)
  .plot_plasmid(features)
}
