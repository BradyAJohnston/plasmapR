geom_feature_arrow <- function(mapping = NULL,
                               data = NULL,
                               position = "identity",
                               show.legend = NA,
                               width = 0.5,
                               head_width = 1,
                               middle = 4,
                               head_size = 30,
                               inherit.aes = TRUE,
                               ...) {
  ggplot2::layer(
    stat = StatFeatureArrow,
    geom = ggplot2::GeomPolygon,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      head_width = head_width,
      head_size = head_size,
      middle = middle,
      ...
    )
  )
}


.compute_group_arrow <- function(data,
                                 scales,
                                 width = 0.5,
                                 middle = 4,
                                 head_width = 1,
                                 head_size = 30L) {
  if (!("direction" %in% colnames(data))) {
    data$direction <- 1
  }

  .create_arrow(
    start = data$start,
    end = data$end,
    phlange = max(data$end - head_size, data$start),
    middle = middle,
    direction = data$direction,
    width = width,
    arrowhead_width = head_width
  )
}

.compute_panel_plasmid <- function(data,
                                   scales,
                                   width = 0.5,
                                   middle = 4,
                                   head_width = 1,
                                   head_size = 30L) {
  data
}


StatFeatureArrow <- ggplot2::ggproto(
  `_class` = "StatFeatureArrow",
  `_inherit` = ggplot2::Stat,
  required_aes = c("start", "end"),
  compute_group = .compute_group_arrow#,
  # compute_panel = .compute_panel_plasmid

)
