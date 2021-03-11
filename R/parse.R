library(seqinr)
library(stringr)
library(ggplot2)
 `%>%` <- magrittr::`%>%`

# read.gb::read.gb("~/Desktop/pETM20-dsnPPR10-C2-AviTag.gb")
# seqinr::read.fasta("~/Desktop/petm20.fa")
#
# read.gb::read.gb("~/Desktop/petm20.gb")
#
# isCircular <- TRUE
# genbankr::readGenBank("~/Desktop/petm20.gb")
#
# petm20 <- genbankr::parseGenBank("~/Desktop/petm20.gb")
# petm20
#
#
#
# genbankr::parseGenBank("~/Desktop/20.gb")
plasmid <- readLines("data/petm20.gb")


# stringr::str_pad(line, width = 76, side = "right")
is_feature <- FALSE
feature_list <- c()
for (i in seq_along(plasmid)) {
  line <- plasmid[i]

  if (str_detect(substr(line, 1, 10), "ORIGIN")) {
    is_feature <- FALSE
  }
  if (str_detect(substr(line, 1, 10), "BASE COUNT")) {
    is_feature <- FALSE
  }


  if (is_feature) {
    feature_list <- c(feature_list, line)
  }

  if (str_detect(substr(line, 1,10), "FEATURES")) {
    is_feature <- TRUE
  }
}

feature_list

counter <- 0
features <- data.frame(
  type = "",
  start = 0,
  end = 0
)
for (i in seq_along(feature_list)) {
  line <- feature_list[i]

  if (str_detect(substr(line, 1, 10), "\\w")) {
    new_feature <- TRUE
  } else {
    new_feature <- FALSE
  }

  if (new_feature) {
    counter <- counter + 1

    feature_type <- str_trim(substr(line, 1, 15), side = "both")
    print(feature_type)
    print(is(feature_type))

    feat_pos <- str_trim(substr(line, 19, 50), side = "both")
    feat_pos <- str_split(line, pattern = "\\.\\.")[[1]]
    # feat_pos <-
    feat_pos[1] <- paste0(str_extract_all(feat_pos, "\\d")[[1]], collapse = "")
    feat_pos[2] <- paste0(str_extract_all(feat_pos, "\\d")[[2]], collapse = "")
    # print(feat_pos)
    print(as.numeric(feat_pos))

    features[counter,] <- features[counter,1]
    features$type[counter] <- feature_type
    features$start[counter] <- as.numeric(feat_pos[1])
    features$end[counter] <- as.numeric(feat_pos[2])

  }

}

# grepl(substr(plasmid, 1, 10), "ORIGIN")

is_sequence <- FALSE

sequence <- c()

for (i in seq_along(plasmid)) {
  line <- plasmid[i]

  if (str_detect(line, "//")) {
    is_sequence <- FALSE
  }

  if (is_sequence) {
    tmp_seq <- substr(line, 10, 78) %>%
      str_trim() %>%
      str_replace_all(pattern = " ", replacement = "")
    sequence <- c(sequence, tmp_seq)
  }

  if (str_detect(substr(line, 1, 10), "ORIGIN")) {
    is_sequence <- TRUE
  }
}

sequence <- str_c(sequence, collapse = "")
sequence



df <- data.frame(
  name = "plasmid1",
  seqeunce = sequence
)
df <- as.data.frame(df)
df %>%
  ggplot(aes(x = 2)) +

  # geom_col(aes(y = nchar(sequence))) +
  geom_segment(aes(x = 2, xend = 2,
                   y = 0, yend = nchar(sequence)),
               size = 1) +
  coord_polar(theta = "y") +
  xlim(0, 4) +
  theme_void() +
  geom_segment(
    data = features,
    aes(x = 2, xend = 2,
        y = as.numeric(start),
        yend = as.numeric(end),
        colour = type),
    size = 3,
    arrow = arrow(angle = 10,
                  type = "open",
                  length = unit(0.01, "npc"))
  ) +
  ggrepel::geom_text_repel(
    data = features,
    aes(
      x = 2,
      y = (as.numeric(end) - as.numeric(start))/2 + as.numeric(start),
      label = type
    ),
    nudge_x = 1,
    box.padding = 0.5,
    xlim = c(4,10)
  ) +
  ylim(0, nchar(sequence)) +
  theme(legend.position = "") +
  annotate(geom = "text",
           x = 0, y = 0,
           label = paste0("Plasmid Label Goes Here\n",
                          nchar(sequence), " bp"))
  NULL
