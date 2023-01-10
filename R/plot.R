.plot_plasmid <- function(dat, bp, name = "Plasmid Name", label_wrap = 20) {
  dat <- dat[dat$type != "source", ]

  dat |>
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
      ggplot2::aes(label = stringr::str_wrap(name, label_wrap)),
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
    geom_fit_text(
      ggplot2::aes(
        label = name,
        y = 4
      ),
      stat = "arrowLabel",
      grow = FALSE,
      size = 10,
      position = ggplot2::position_dodge2(),
      min.size = 1,
      invert = FALSE,
      flip = FALSE

    ) +
    ggplot2::ylim(c(0, NA)) +
    ggplot2::xlim(c(0, bp)) +
    ggplot2::theme_void() +
    ggplot2::annotate(
      geom = "text",
      x = 0,
      y = 0,
      label = stringr::str_glue("{name}\n{bp} bp")
    ) +
    ggplot2::scale_fill_brewer(type = 'qual', palette = 5) +
    ggplot2::theme(
      legend.position = ""
    )
  }

#' Plot a Plasmid
#'
#' Create a `{ggplot2}` plot of a plasmid in polar coordinates. Extracts the
#' features as a data.frame from the plasmid and uses these to construct arrows
#' that are added to the plot.
#'
#' @param plasmid A list of class 'plasmid' created through `read_gb()`.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
plot_plasmid <- function(plasmid, name = "Plasmid Name", label_wrap = 20) {
  features <- as.data.frame(plasmid)

  # remove NA values for start and end, need better handling of this
  fil <- is.na(features$start) | is.na(features$end) | features$type == "gene"
  features <- features[!fil, ]

  .plot_plasmid(
    features,
    bp  = plasmid$length,
    name = name,
    label_wrap = label_wrap
    )
}
