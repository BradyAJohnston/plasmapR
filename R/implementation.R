library(tidyverse)


create_text <- function(start, end, name, spacing, scale = 1, reverse = FALSE) {

  # length of the feature name in characters
  if (is.na(name)) {
    name_len <- 0
  } else {
    name_len <- stringr::str_length(name)
  }

  # length of feature (bp)
  feat_len <- end - start

  # mid point of feature (in bp)
  feat_mid <- feat_len / 2 + start

  label_width = (name_len / 20 * scale)
  offset = (0.9 - label_width) / 2

  # decide on x positions of characters
  name_radius <- feat_len * label_width


  char_spacing <- round(name_radius / name_len, spacing)

  label_characters <- stringr::str_split(name, "")[[1]]

  if (reverse) {
    label_characters <- rev(label_characters)
  }

  char_df <- data.frame(
    num = 1:name_len,
    char = label_characters
  )

  char_df$pos <- char_df$num * char_spacing + (start + feat_len * offset)

  # char_df$angle <- char_df$pos /

  char_df
}

arrow_positions <- function(start, end, middle, width, scale = 0.9) {

  length = end - start

  head = start + length * scale

  data.frame(
    x = c(
      start,
      start,
      head,
      head,
      end,
      head,
      head,
      start
      ),
    y = c(
      middle + width,
      middle - width,
      middle - width,
      middle - width * 2,
      middle,
      middle + width * 2,
      middle + width,
      middle + width
    )
  )
}

# arrow_positions(1, 1800, 2, 0.1)



create_arrows <- function(df, middle, width, scale = 0.9) {
  arrow_list <- list()

  for (i in 1:nrow(df)) {

    start = df[i, "start"]
    end = df[i, "end"]

    temp_arrow_df <- arrow_positions(
      start = start,
      end = end,
      middle = middle,
      width = width
    )

    temp_arrow_df$type <- df[i, "type"]
    temp_arrow_df$name <- df[i, "name"]

    arrow_list[[i]] <- temp_arrow_df
  }

  out_df <- do.call(rbind, arrow_list)
  out_df
}


create_curved_labels <- function(df, plasmid_length, spacing_scale, ratio_cutoff = 0.1) {

  annotation_list <- list()
  curved_text_list <- list()


  for (i in 1:nrow(df)) {
    label_length <- nchar(df[i, "name"])

    feat_length <- df[i, "end"] - df[i, "start"]

    feat_middle <- df[i, "start"] + feat_length / 2

    circle_pos <- feat_middle / plasmid_length
    # print(circle_pos)
    ratio_of_map <- feat_length / plasmid_length



    # print(ratio_of_map)
    if (ratio_of_map < ratio_cutoff) {
      annotation_list[[i]] <- df[i,]
    } else {

      if (circle_pos > 0.25 && circle_pos < 0.75) {
        reverse <- TRUE
        angle_adjustment <- 180
      } else {
        reverse <- FALSE
        angle_adjustment <- 0
      }

      curved_text <- create_text(
        start = df[i, "start"],
        end = df[i, "end"],
        name = df[i, "name"],
        spacing = spacing_scale,
        scale = 1 / label_length * spacing_scale,
        reverse = reverse
      )

      curved_text$angle <- -curved_text$pos / plasmid_length * 360

      # angle_adjustment <-  180 #ifelse(circle_pos > 0.25 & circle_pos < 0.75, 0, 180)

      curved_text$angle <- curved_text$angle + angle_adjustment

      curved_text_list[[i]] <- curved_text
    }

  }

  list(
    annotations = do.call(rbind, annotation_list),
    curved = do.call(rbind, curved_text_list)
  )


}


plasmid_length <- nchar(sequence)

feature_labels <- create_curved_labels(feat_df, plasmid_length, 4.5, ratio_cutoff = 0.05)

data_df <- create_arrows(feat_df, 2, 0.1)



feature <- data.frame(
  name = c("NcoI", "EcoRI"),
  location = c(1900, 3875)
)


data_df

data_df %>%
  filter(!is.na(name)) %>%
  filter(name != "6xHis") %>%
  # filter(name != "primer_bin") %>%
  # filter(type == "CDS") %>%
  ggplot() +
  geom_hline(yintercept = 2) +
  xlim(c(1, plasmid_length)) +

  ylim(c(0,2.2)) +
  coord_polar() +
  geom_polygon(
    aes(
      x = x,
      y = y,
      group = name,
      fill = as.factor(type)
    ),
    # fill = "lightblue",
    colour = "gray20"
  ) +
  geom_text(
    data = feature_labels$curved,
    mapping = aes(
      x = pos,
      y = 2,
      label = char,
      angle = angle
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
      label = "pETM20-Avi-dsnPPR10-C2",
    size = 20 / .pt,
    family = "mono"
  ) +
  ggrepel::geom_text_repel(
    data = feature_labels$annotations,
    mapping = aes(
      x = (end - start) / 2 + start,
      y = 2.1,
      label = name
    ),
    nudge_y = 0.3,
    nudge_x = 100,
    ylim = c(2, 5), force = 10,direction = "y",
    family = "mono",
    box.padding = 0.3, point.padding = 1
  ) +
  theme(legend.position = "")


  NULL

ggsave("data/map_example.png")
