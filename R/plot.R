
#' @importFrom rlang .data
#' @noRd
.plot_plasmid <- function(
    dat,
    bp,
    name = "Plasmid Name",
    rotation = 0,
    label_wrap = 20
    ) {
  dat <- dat[dat$type != "source", ]

  name_supplied <- !is.null(name) & name != ""

  if (name_supplied) {
    yintercept = 4
  } else {
    yintercept = 0
  }

  plt <- dat |>
    ggplot2::ggplot(ggplot2::aes(
      start = .data$start,
      end = .data$end,
      direction = .data$direction,
      fill = .data$type,
      group = .data$index
    )) +
    ggplot2::geom_hline(yintercept = yintercept) +
    ggplot2::coord_polar(
      start = rotation
      ) +


    ggrepel::geom_label_repel(
      ggplot2::aes(label = stringr::str_wrap(.data$name, label_wrap)),
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
        label = .data$name,
        y = yintercept
      ),
      stat = "arrowLabel",
      grow = FALSE,
      size = 10,
      position = ggplot2::position_dodge2(),
      min.size = 1,
      invert = FALSE,
      flip = FALSE

    ) +
    ggplot2::ylim(c(yintercept - 4, NA)) +
    ggplot2::xlim(c(0, bp)) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_brewer(type = 'qual', palette = 5) +
    ggplot2::theme(
      legend.position = ""
    )

  if (name_supplied) {
    plt <- plt +
      ggplot2::annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = stringr::str_glue("{name}\n{bp} bp")
      )
  }

  plt

  }

#' Plot a Plasmid
#'
#' Create a `{ggplot2}` plot of a plasmid in polar coordinates. Extracts the
#' features as a data.frame from the plasmid and uses these to construct arrows
#' that are added to the plot.
#'
#' @param plasmid A list of class 'plasmid' created through `read_gb()`.
#' @param name Name of the plasmid, to be shown in the center.
#' @param label_wrap Passed to `stringr::str_wrap()` to wrap the long labels.
#'
#' @return A ggplot object.
#' @export
plot_plasmid <- function(plasmid, name = "Plasmid Name", label_wrap = 20) {
  if (methods::is(plasmid, "plasmid")) {
    features <- as.data.frame(plasmid, bp = plasmid$length)
  } else if (methods::is(plasmid, "data.frame")) {
    features <- plasmid
  } else {
    cli::cli_abort("Must be either plasmid or data.frame")
  }

  if (methods::is(plasmid, "data.frame")) {
    bp <- max(c(features$start, features$end))
  } else {
    bp <- plasmid$length
  }


  # remove NA values for start and end, need better handling of this
  fil <- is.na(features$start) | is.na(features$end) | features$type == "gene"
  features <- features[!fil, ]

  .plot_plasmid(
    features,
    bp  = bp,
    name = name,
    label_wrap = label_wrap
    )
}
