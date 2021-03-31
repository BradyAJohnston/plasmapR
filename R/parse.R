library(stringr)
library(ggplot2)
library(dplyr)
`%>%` <- magrittr::`%>%`


plasmid <- readLines("data/petm20.gb")


get_features <- function(plasmid) {

  is_feature <- FALSE
  feature_list <- c()

  # sapply(plasmid, function(i) {
  for (i in seq_along(plasmid)) {
    line <- plasmid[i]
    # print()
    # line <- i
    # print(i)

    if (str_detect(substr(line, 1, 10), "ORIGIN")) {
      is_feature <- FALSE
    }

    if (str_detect(substr(line, 1, 10), "BASE COUNT")) {
      is_feature <- FALSE
    }

    if (is_feature) {
      feature_list <- c(feature_list, line)
      line
    }

    if (str_detect(substr(line, 1,10), "FEATURES")) {
      is_feature <- TRUE
    }
  }

  # }, simplify = TRUE)


  feature_list
}

# plasmid


features <- get_features(plasmid)
features



create_feature_df <- function(input) {
  counter <- 0
  features <- data.frame(
    type = "",
    start = 0,
    end = 0
  )

  for (i in seq_along(input)) {
    line <- input[i]

    if (str_detect(substr(line, 1, 10), "\\w")) {
      new_feature <- TRUE
    } else {
      new_feature <- FALSE
    }

    if (new_feature) {
      counter <- counter + 1

      feature_type <- str_trim(substr(line, 1, 15), side = "both")


      feat_pos <- str_trim(substr(line, 19, 50), side = "both")
      feat_pos <- str_split(line, pattern = "\\.\\.")[[1]]

      feat_pos[1] <- paste0(str_extract_all(feat_pos, "\\d")[[1]], collapse = "")
      feat_pos[2] <- paste0(str_extract_all(feat_pos, "\\d")[[2]], collapse = "")

      features[counter,] <- features[counter,1]
      features$type[counter] <- feature_type
      features$start[counter] <- as.numeric(feat_pos[1])
      features$end[counter] <- as.numeric(feat_pos[2])
      features$index <- counter

    }

    if (!new_feature) {
      if (str_detect(line, "/label"))
        features$name[counter] <- str_split(line, "=")[[1]][2]

    }

  }

  features

}
# plasmid
feat_df <- create_feature_df(features)


feat_df






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


ggplot() +
  geom_hline(yintercept = 2) +
  xlim(c(0, nchar(sequence))) +
  ylim(c(0,2.3)) +
  coord_polar() +
  geom_segment(
    data = feat_df %>% filter(type != "source"),
    aes(x = as.numeric(start),
        xend = as.numeric(end),
        y = 2,
        yend = 2,
        colour = type
  ),
  size = 4,
  arrow = arrow()
  )


