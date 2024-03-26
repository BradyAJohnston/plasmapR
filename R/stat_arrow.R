#' @noRd
.rectangle_points <- function(start, end, middle, width) {
  top <- middle + width
  bottom <- middle - width
  data.frame(
    x = c(start, end, end, start, start),
    y = c(top, top, bottom, bottom, top)
  )
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
    points <- .rectangle_points(start, end, middle, width)
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
    points <- .arrow_points(base, tip, midpoint, phlange, arrowhead_width, width)
  }

  points
}

#' Compute positions for an Arrow
#' @export

StatArrow <- ggplot2::ggproto('StatArrow', ggplot2::Stat,
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

    points <- .feature_get_dim(
      start = data$start,
      end = data$end,
      direction = data$direction,
      phlange_angle = 8,
      bp = 6000
    )

    # data$start   <- points$start
    # data$end     <- points$end
    data$direction <- points$direction
    data$phlange <- points$phlange

    data
  },
  compute_group = function(data,
                           scales,
                           width = 0.15,
                           bp = 6000,
                           arrowhead_size = 8){


    arrows <-
      .create_arrow(
        start = data$start,
        end = data$end,
        middle = data$middle,
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
  default_aes = ggplot2::aes(
    direction = 1
  )
)

#' Compute required values for labels
#' @export
StatArrowLabel <- ggplot2::ggproto('StatArrowLabel', StatArrow,
  compute_group = function(data,
                           scales,
                           bp = 6000,
                           invert = TRUE) {
    # data$length <- data$end - data$start


  df <- data.frame(
      x = mean(c(data$start, data$end)),
      y = data$middle,
      ymin = 3.5,
      ymax = 4.5,
      xmin = data$start,
      xmax = data$end
    )

  fil <- data$length / bp > nchar(data$label) / bp * 60

  if (invert) fil <- !fil
  # if (! invert) df <- df[, c("y", "xmin", "xmax")]
  df[fil]
  }#,
  # required_aes = c('start', 'end')
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
           # middle = 4,
           arrowhead_size = 8
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
        # middle = middle,
        arrowhead_size = arrowhead_size,
        # start = start,
        # end = end,
        ...)
    )
  }
