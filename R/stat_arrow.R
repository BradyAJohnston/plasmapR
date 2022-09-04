library(ggplot2)

plas <- plasmapR::parse_plasmid("data/petm20.gb")
# plas <- plasmapR::parse_plasmid(
#   "~/../Documents/GitHub/thesis/figures/plasmids/pETM20-dsnPPR10-C2-AviTag.gb"
#   )

plas$features$direction <- ifelse(plas$features$direction == "LEFT", 1, -1)


.create_arrow <- function(start,
                          end,
                          direction = 1,
                          middle,
                          width,
                          plasmid_length,
                          arrowhead_width = 5,
                          arrowhead_size = 8) {
  if (start > end) {
    end_temp <- end
    end <- start
    start <- end_temp
    direction <- direction * -1
  }

  length <- end - start

  arrow_width <- plasmid_length / 360 * arrowhead_size

  if (direction == -1) {
    end_temp <- end
    end <- start
    start <- end_temp
  }

  phlange <- end - direction * plasmid_length / 100


  if (abs(length) < abs(arrow_width)) {
    phlange <- start
  }

  phlange_outer <- middle + width
  phlange_inner <- middle - width

  data.frame(
    x = c(start,
          start,
          phlange,
          phlange,
          end,
          phlange,
          phlange,
          start),
    y = c(
      phlange_outer,
      phlange_inner,
      phlange_inner,
      phlange_inner - width * arrowhead_width,
      middle,
      phlange_outer + width * arrowhead_width,
      phlange_outer,
      phlange_outer
    )
  )
}


StatArrow <- ggproto(
  'StatArrow',
  Stat,
  setup_data = function(data, params) {
    wrong_orientation <- data$end < data$start
    end_temp <- data$end
    data$end[wrong_orientation] <- data$start[wrong_orientation]
    data$start[wrong_orientation] <- end_temp[wrong_orientation]
    data$middle <- 4

    vec_start <- data$start
    vec_end   <- data$end
    wrong_way <- vec_start > vec_end
    data$start[wrong_way] <- data$end[wrong_way]
    data$end[wrong_way] <- data$start[wrong_way]
    data$length <- data$end - data$start

    .find_overlaps <- function(data) {


      between <- function(x, y, z) {
        x > y & x < z
      }


      data$id <- seq_len(nrow(data))
      mat <- outer(data$id, data$id, FUN = function(x, y) {

        x_start <- data[x, "start"]
        x_end   <- data[x, "end"]
        y_start <- data[y, "start"]
        y_end   <- data[y, "end"]

        # check if either the start or the end of the feature lies within
        # the other feature
        (
          between(x_end, y_start, y_end) |
          between(x_start, y_start, y_end)
        ) & data$length[x] < data$length[y] # only say it is overlapping if it's shorter than the other feature

      })

      vec <- apply(mat, 2, function(x) data[x, "id"], simplify = TRUE)
      vec <- unique(unlist(vec))
      vec

    }

    overlap <- .find_overlaps(data)


    # overlap_end   <- sapply(data$end, function(i) TRUE %in% (i < data$end))
    # overlap <- overlap_start & overlap_end
    data$middle[overlap] <- data$middle[overlap] + 0.6
    data$arrowhead_width <- 0.5
    data$arrowhead_width[overlap] <- 0.5

    data
  },
  compute_group = function(data,
                           scales,
                           # start = 1,
                           # end = 800,
                           # middle = 4,
                           width = 0.2,
                           bp = 6000,
                           arrowhead_size = 8){
    arrows <-
      .create_arrow(
        start = data$start,
        end = data$end,
        middle = data$middle,
        direction = data$direction,
        arrowhead_width = data$arrowhead_width,
        width = width,
        plasmid_length = bp,
        arrowhead_size = arrowhead_size
      )

    arrows
  },
  required_aes = c('start', 'end'),
  default_aes = aes(
    direction = 1
  )
)

StatArrowLabel <- ggproto(
  'StatArrowLabel',
  StatArrow,
  compute_group = function(data,
                           scales,
                           bp = 6000,
                           invert = TRUE) {
    # data$length <- data$end - data$start

  df <- data.frame(
      x = mean(c(data$start, data$end)),
      y = data$middle
    )

  fil <- data$length / bp > nchar(data$label) / bp * 60

  if (invert) fil <- !fil

  df[fil]
  },
  required_aes = c('start', 'end')
)





stat_arrow <-
  function(mapping = NULL,
           data = NULL,
           geom = "polygon",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...,
           bp = 6000,
           middle = 4
           ) {



    ggplot2::layer(
      stat = "arrow",
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        bp = bp,
        middle = middle,
        # start = start,
        # end = end,
        ...)
    )
  }

# data.frame(
#   start = c(10, 700, 2300, 2500, 3000, 3300),
#   end = c(150, 200, 2100, 2000, 4000, 3900),
#   dir = c(1, -1, -1, -1, 1, 1)
#   )

plas$features |>

  ggplot(aes(
    start = start,
    end = end,
    direction = direction,
    fill = type,
    group = factor(index)
  )) +
  geom_hline(yintercept = 4) +
  coord_polar(start = pi / 4) +


  ggrepel::geom_label_repel(
    aes(label = name),
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
    bp = plas$length
    ) +
  ggfittext::geom_fit_text(
    aes(label = name, ymin = 3.5, ymax = 4.5, xmin = start, xmax = end - 50),
    stat = "arrowLabel",
    position = position_dodge2(),
    min.size = 1,
    invert = FALSE,
    flip = TRUE

  ) +
  ylim(c(0, NA)) +
  # facet_wrap(vars(end)) +
  xlim(c(0, bp = plas$length)) +
  theme_void() +
  annotate(
    geom = "text",
    x = 0,
    y =0,
    label = "Some Plasmid\n4800 bp"
  ) +
  scale_fill_brewer(type = 'qual', palette = 5) +
  theme(
    legend.position = "",
    # panel.background = element_rect(colour = 'black', fill = 'transparent')
  )
