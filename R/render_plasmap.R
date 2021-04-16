#' Render Plasmap
#'
#' @param plasmid A list containing at least a dataframe of features and the
#'   plasmid length.
#' @param arrowhead_size Size (in degrees) of the arrowheads. Sets how much of
#'   the circle they will take up (total of circle being 360 degrees).
#' @param spacing_scale Scalar for the spacing between the curved text on the arrows.
#' @param label_hjust Adjusts justification of the curved labels towards the start (0) or end (1) of the arrow.
#' @param label_length_cutoff Proportion of the arrow the text is allowed to take up before becoming a rep
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
                           nameSize = 6,
                           curve = 10,
                           labelNudge = 0.8,
                           labelSize = 10,
                           repelBox = 0.2) {
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

  p <- plasmid_plot(
    arrow_df = arrow_df,
    labels = labels,
    angle_adjustment = angle_adjustment,
    zoom_y = zoom_y,
    plasmidName = plasmid_name,
    nameSize = nameSize,
    curve = curve,
    labelNudge = labelNudge,
    labelSize = labelSize,
    repelBox = repelBox,
    plasmid_length = plasmid$length
  )

  p
}
