

#' @noRd
.find_overlaps <- function(data) {
  between <- function(x, y, z) {
    x > y & x < z
  }


  data$id <- seq_len(nrow(data))
  mat <- outer(
    data$id,
    data$id,
    FUN = function(x, y) {
      x_start <- data[x, "start"]
      x_end   <- data[x, "end"]
      y_start <- data[y, "start"]
      y_end   <- data[y, "end"]

      # check if either the start or the end of the feature lies within
      # the other feature
      (between(x_end, y_start, y_end) |
          between(x_start, y_start, y_end)) &

        # only say it is overlapping if it's shorter than the other feature
        data$length[x] < data$length[y]

    }
  )

  vec <- apply(mat, 2, function(x)
    data[x, "id"], simplify = TRUE)
  vec <- unique(unlist(vec))
  vec

}


#' @noRd
.create_arrow <- function(start,
                          end,
                          phlange,
                          middle,
                          width,
                          direction = 1,
                          plasmid_length,
                          arrowhead_width = 5,
                          arrowhead_size = 8) {
  if (direction == 0) {
    points <- .points_rectangle(start, end, middle, width)
  } else {
    if (direction == -1) {
      end_temp <- end
      end <- start
      start <- end_temp
      direction <- direction * -1
    }
    # convert to 'arrow' nomenclature to make it more clear what is going on
    base <- start
    tip <- end
    midpoint <- middle
    points <-
      .points_arrow(base, tip, midpoint, phlange, arrowhead_width, width)
  }

  points
}

#' Compute positions for an Arrow
#' @export

StatArrow <- ggplot2::ggproto(
  'StatArrow',
  ggplot2::Stat,
  setup_data = function(data, params) {
    wrong_orientation <- data$end < data$start
    end_temp <- data$end
    data$end[wrong_orientation] <- data$start[wrong_orientation]
    data$start[wrong_orientation] <- end_temp[wrong_orientation]

    vec_start <- data$start
    vec_end   <- data$end
    wrong_way <- vec_start > vec_end
    data$start[wrong_way] <- data$end[wrong_way]
    data$end[wrong_way] <- data$start[wrong_way]
    data$length <- data$end - data$start

    # find the overlaps and offset the features on the y axis
    # so that the smaller overlapping features are not on the primary x axis
    overlap <- .find_overlaps(data)

    data$offset <- 0
    data$offset[overlap] <- 1
    data$arrowhead_width <- 0.5


    data
  },
  compute_group = function(data,
                           scales,
                           width = 0.15,
                           bp = 10000,
                           middle = 4,
                           phlange_angle = 5,
                           arrowhead_size = 8) {
    points <- .feature_get_dim(
      start = data$start,
      end = data$end,
      direction = data$direction,
      phlange_angle = phlange_angle,
      bp = bp
    )

    data$direction <- points$direction
    data$phlange <- points$phlange

    arrows <-
      .create_arrow(
        start = data$start,
        end = data$end,
        middle = middle + data$offset * width * 4,
        phlange = data$phlange,
        direction = data$direction,
        arrowhead_width = 0.5,
        width = width,
        plasmid_length = bp,
        arrowhead_size = arrowhead_size
      )

    arrows
  },
  required_aes = c('start', 'end'),
  default_aes = ggplot2::aes(direction = 1)
)

#' Will a label for a feature fit in the drawable feature box?
#' @keywords internal
.will_label_fit <- function(label, feature_length_bp, plasmid_bp) {
  (feature_length_bp / plasmid_bp) > (nchar(label) / plasmid_bp * 60)
}

#' Compute required values for labels
#' @export
StatArrowLabel <- ggplot2::ggproto(
  'StatArrowLabel',
  StatArrow,
  compute_group = function(data,
                           scales,
                           bp = 6000,
                           width = 0.15,
                           middle = 4,
                           invert = TRUE) {
    df <- data.frame(
      x = mean(c(data$start, data$end)),
      y = middle + data$offset * width * 4,
      xmin = data$start,
      xmax = data$end
    )

    mask <- .will_label_fit(data$label, data$length, bp)

    if (invert) {
      mask <- !mask
    }
    df[mask]
  }
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
           middle = 4,
           arrowhead_size = 8,
           linewidth = 1,
           phlange_angle = 8) {
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
        linewidth = linewidth,
        phlange_angle = phlange_angle,
        arrowhead_size = arrowhead_size,
        ...
      )
    )
  }
