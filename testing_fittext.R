library(ggplot2)
library(ggfittext)

# ggplot(gold,
#        aes(
#          xmin = xmin,
#          xmax = xmax,
#          ymin = ymin,
#          ymax = ymax,
#          fill = linenumber,
#          label = line
#        )) +
#   coord_polar() +
#   geom_rect() +
#   geom_fit_text(min.size = 0, grow = TRUE) +
#   scale_fill_gradient(low = "#fee391", high = "#238443")


library(plasmapR)

plasmid <- parse_plasmid(file = "data/20.gb")

# render_plasmap(plasmid)

middle = 2
width = 0.1
arrowhead_size = 4
rotation = 0
# scales text size and how far apart
curved_scaling = 1
# additional scaling for apart-ness
size_scale = 1
# spacing_scale = 0.006
label_hjust = 0.4
label_length_cutoff = 0.85
label_curve = 0
zoom_y = 3
plasmid_name = "plasmid_name"
name_size = 6
curve = 10
label_nudge = 0.8
# label_size = 10
repel_box = 0.2
bp_count = TRUE



arrow_df <-
  create_arrow_plotting_df(
    df = plasmid$features,
    middle = middle,
    width = width,
    arrowhead_size = arrowhead_size,
    plasmid_length = plasmid$length
  )

angle_adjustment <-
  ifelse(rotation < 180, rotation, - (360 - rotation))

spacing_scale <- curved_scaling * 0.01 * size_scale

labels <- create_labels(
  df = plasmid$features,
  plasmid_length = plasmid$length,
  spacing_scale = spacing_scale,
  label_hjust = label_hjust,
  rotation = angle_adjustment,
  label_length_cutoff = label_length_cutoff
)

curved_size <- curved_scaling * 12
repel_text_size <- curved_scaling * 12

p <- plasmid_plot(
  arrow_df = arrow_df,
  labels = labels,
  angle_adjustment = angle_adjustment,
  zoom_y = zoom_y,
  plasmid_name = plasmid_name,
  name_size = name_size,
  curve = curve,
  curved_size = curved_size,
  label_nudge = label_nudge,
  label_size = repel_text_size,
  repel_box = repel_box,
  plasmid_length = plasmid$length,
  bp_count = bp_count
)

p + geom_fit_text(
  data = plasmid$features,
  mapping = aes(
  ymin = 1.9,
  ymax = 2.1,
  xmin = start,
  xmax = end,
  label = name,
  fill = type
),
fullheight = TRUE, grow = TRUE, )
