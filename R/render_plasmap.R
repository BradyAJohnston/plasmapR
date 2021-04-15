#' Render Plasmap
#'
#' @export

render_plasmap <- function(plasmid,
                           middle = 2,
                           width = 0.1,
                           arrowhead_size = 4,
                           rotation = 0,
                           spacing_scale = 0.006,
                           label_hjust = 0.4,
                           label_length_cutoff = 0.85,
                           label_curve = 0,
                           zoom_y = 3,
                           plasmid_name = "plasmid_name",
                           nameSize = 10,
                           curve = 0,
                           labelNudge = 0.5,
                           labelSize = 10,
                           repelBox = 0.5
                           ) {
  arrow_df <-
    create_arrow_plotting_df(
      df = plasmid$features,
      middle = middle,
      width = width,
      arrowhead_size = arrowhead_size,
      plasmid_length = plasmid$length
    )

  angle_adjustment <-
    ifelse(rotation < 180, rotation, -(360 - rotation))

  labels <- create_labels(
    df = plasmid$features,
    plasmid_length = plasmid$length,
    spacing_scale = spacing_scale,
    label_hjust = label_hjust,
    rotation = angle_adjustment,
    label_length_cutoff = label_length_cutoff
  )

  plasmid_plot(
    plasmid$features,
    features = NULL,
    zoom_y = zoom_y,
    plasmidName = plasmid_name,
    nameSize = nameSize,
    curve = curve,
    rotation = rotation,
    labelNudge = labelNudge,
    labelSize = labelSize,
    repelBox = repelBox,
    plasmid_length = plasmid$length

  )

}

