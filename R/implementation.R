library(tidyverse)



arrow_df <- data.frame(
  molecule = "Genome2",
  start = 0,
  end = 1985,
  x = c(0, 0, 1000, 1000, 1100, 1000, 1000, 0),
  y = c(2.1, 1.9, 1.9, 1.8, 2, 2.2, 2.1, 2.1)
)

new_arrow_df <- data.frame(
  molecule = "Genome2",
  start = 0,
  end = 1985,
  x = c(4000, 4000, 6000, 6000, 6100, 6000, 6000, 4000),
  y = c(2.1, 1.9, 1.9, 1.8, 2, 2.2, 2.1, 2.1)
)

feature <- data.frame(
  name = c("NcoI", "EcoRI"),
  location = c(1900, 3875)
)

create_text <- function(start, end, name, spacing, scale = 1) {

  # length of the feature name in characters
  name_len <- stringr::str_length(name)

  # length of feature (bp)
  feat_len <- end - start

  # mid point of feature (in bp)
  feat_mid <- feat_len / 2 + start

  label_width = (name_len / 20 * scale)
  offset = (0.9 - label_width) / 2

  # decide on x positions of characters
  name_radius <- feat_len * label_width


  char_spacing <- round(name_radius / name_len, spacing)

  char_df <- data.frame(
    num = 1:name_len,
    char = stringr::str_split(name, "")[[1]]
  )

  char_df$pos <- char_df$num * char_spacing + (start + feat_len * offset)

  # char_df$angle <- char_df$pos /

  char_df
}


# create_text(0, 1100, "Gene1")


gggenes::example_genes %>%
  filter(molecule %in% c("Genome2")) %>%
  mutate(end = end - min(start)) %>%
  mutate(start = start - min(start)) %>%

  ggplot() +
  geom_hline(yintercept = 2) +
  xlim(c(0,6574)) +
  # geom_gene_arrow(
  #   aes(y = 2)
  # ) +
  # geom_segment(
  #   aes(
  #     x = start,
  #     xend = end,
  #     y = 2,
  #     yend = 2
  #   ),
  #   arrow = arrow()
  # ) +
  ylim(c(0,2.2)) +
  coord_polar() +
  # facet_wrap(~molecule) +
  geom_polygon(
    data  = arrow_df,
    aes(
      x = x,
      y = y,
      group = molecule
    ),
    fill = "green",
    colour = "black"
  ) +
  geom_polygon(
    data = new_arrow_df,
    aes(
      x = x,
      y = y,
      group = molecule
    ),
    fill = "lightblue",
    colour = "gray20"
  ) +
  geom_text(
    data = rbind(
      create_text(0, 1100, "Another Gene", scale = 0.6),
      create_text(4000, 6100, "A Slightly Bigger Gene", scale = 0.3)),
    mapping = aes(
      x = pos,
      y = 2,
      label = char,
      angle = -(pos / 6574) * 360
    ),
    colour = "Black",
    family = "mono"#,
    # size = 12 / .pt
  ) +
  theme_void() +
  annotate(
    geom = "text",
      x = 0,
      y = 0,
      label = "Plasmid Name",
    size = 20 / .pt,
    family = "mono"
  ) +
  geom_segment(
    data = feature,
    mapping = aes(
      x = location,
      xend = location,
      y = 1.9,
      yend = 2.1
    )
  ) +
  ggrepel::geom_text_repel(
    data = feature,
    mapping = aes(
      x = location,
      y = 2.1,
      label = name
    ),
    nudge_y = 0.2,
    nudge_x = 100,
    family = "mono"
  ) +


  NULL

